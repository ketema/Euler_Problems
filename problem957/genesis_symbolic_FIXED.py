# genesis_symbolic_FIXED.py
"""
FIXED VERSION: Proper pencil-index encoding

KEY FIX: R4 (pair-invariance) is NOT applied blindly at day 0→1.
Instead, we keep pencil indices and only collapse when Pappus theorem allows it.

At day 0→1: X(i,j;a,b) → PencilNode(i,j,a,b) - 6 DISTINCT tokens
At day 2+: Pappus creates equivalences → then pair-collapse applies
"""
from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Set, Tuple, Union

# Extended term algebra with pencil indices
Leaf = str
PencilNode = Tuple[str, int, int, str, str]  # ('X', i, j, a, b)
PairNode = Tuple[str, Any, Any]              # ('⊗', left, right)
Term = Union[Leaf, PencilNode, PairNode]

RED = {"R1", "R2", "R3"}

def is_leaf(t: Term) -> bool:
    return isinstance(t, str)

def is_pencil(t: Term) -> bool:
    return (isinstance(t, tuple) and len(t) == 5 and
            t[0] == 'X' and isinstance(t[1], int) and isinstance(t[2], int))

def is_pair(t: Term) -> bool:
    return (isinstance(t, tuple) and len(t) == 3 and t[0] == '⊗')

def term_key(t: Term):
    """Canonical ordering for normalization"""
    if is_leaf(t):
        return (0, str(t))
    if is_pencil(t):
        _, i, j, a, b = t
        # Canonical form: i < j, then by (a,b) lex order
        if i > j:
            i, j = j, i
        if a > b:
            a, b = b, a
        return (1, i, j, a, b)
    if is_pair(t):
        _, L, R = t
        return (2, term_key(L), term_key(R))
    return (3,)

def make_pencil_node(i: int, j: int, a: str, b: str) -> PencilNode:
    """Create normalized pencil node"""
    # Canonical form: i < j
    if i > j:
        i, j = j, i
    return ('X', i, j, a, b)

def make_pair_node(a: Term, b: Term) -> PairNode:
    """Create normalized pair node (commutativity)"""
    if is_leaf(a) and is_leaf(b):
        if a == b:  # Idempotence
            return a
    # Commutativity
    if term_key(b) < term_key(a):
        a, b = b, a
    return ('⊗', a, b)

def unordered_pair(a: str, b: str) -> Tuple[str, str]:
    return (a, b) if a <= b else (b, a)

@dataclass
class State:
    day: int
    blues: Set[Leaf]
    # Provenance: track which base pairs generated each blue
    prov: Dict[Leaf, Set[Tuple[str, str]]]

def init_state() -> State:
    return State(day=0, blues={"b0_1", "b0_2"}, prov={})

def is_colored(t: Term, S: State) -> bool:
    """Check if term represents a colored (red or blue) point"""
    return is_leaf(t) and (t in RED or t in S.blues)

def old_blue_coincidence(t: Term, S: State) -> Term:
    """R8: If term matches provenance of existing blue, replace with that blue"""
    if is_pair(t):
        _, L, R = t
        if is_leaf(L) and is_leaf(R):
            key = unordered_pair(L, R)
            for blue, pairs in S.prov.items():
                if key in pairs:
                    return blue
    return t

def pappus_merge(t: Term, S: State) -> Term:
    """
    R9: Pappus hexagon merge (conservative guard)

    Pattern: ((A⊗B) ⊗ (C⊗D)) → ((A⊗D) ⊗ (C⊗B))

    Only if both {A,B} and {C,D} are in provenance (proven ranges)
    """
    if not is_pair(t):
        return t
    _, L, R = t
    if not (is_pair(L) and is_pair(R)):
        return t

    # Inner pairs
    if is_leaf(L[1]) and is_leaf(L[2]) and is_leaf(R[1]) and is_leaf(R[2]):
        A, B = L[1], L[2]
        C, D = R[1], R[2]

        # Guard: both pairs must be in provenance
        key1 = unordered_pair(A, B)
        key2 = unordered_pair(C, D)
        seen1 = any(key1 in pairs for pairs in S.prov.values())
        seen2 = any(key2 in pairs for pairs in S.prov.values())

        if seen1 and seen2:
            return make_pair_node(make_pair_node(A, D), make_pair_node(C, B))

    return t

def normalize_term(t: Term, S: State) -> Term:
    """Apply normalization rules to fixed point"""
    changed = True
    cur = t
    iterations = 0
    max_iterations = 100  # Prevent infinite loops

    while changed and iterations < max_iterations:
        changed = False
        iterations += 1

        # R8: Old-blue coincidence
        new = old_blue_coincidence(cur, S)
        if new != cur:
            cur, changed = new, True
            continue

        # R9: Pappus merge
        new = pappus_merge(cur, S)
        if new != cur:
            cur, changed = new, True
            continue

    return cur

def expand_one_day(S: State) -> State:
    """
    Generate new blues for day S.day → S.day+1

    KEY FIX: At day 0, create PencilNode terms (keep pencil indices)
             At day 1+, create PairNode terms (Pappus allows collapse)
    """
    B = list(S.blues)  # Current blues
    colored = RED | S.blues

    new_map: Dict[Term, Leaf] = {}
    new_prov: Dict[Leaf, Set[Tuple[str, str]]] = {}
    counter = 1

    # Iterate over all pencil pairs (i,j)
    for i, j in [(1, 2), (1, 3), (2, 3)]:
        # Iterate over all blue pairs (a,b) with a ≠ b
        for a in B:
            for b in B:
                if a == b:
                    continue

                # KEY FIX: Day 0 vs Day 1+ distinction
                if S.day == 0:
                    # Day 0→1: Keep pencil indices
                    # X(i,j;a,b) creates a PencilNode
                    t = make_pencil_node(i, j, a, b)
                else:
                    # Day 1+: Apply pair-collapse (Pappus is active)
                    # X(i,j;a,b) → a⊗b (pencil-invariant)
                    t = make_pair_node(a, b)

                # Normalize term
                t = normalize_term(t, S)

                # Skip if colored
                if is_colored(t, S):
                    continue

                # Add new blue
                if t not in new_map:
                    lbl = f"b{S.day+1}_{counter}"
                    counter += 1
                    new_map[t] = lbl
                    new_prov[lbl] = set()

                # Track provenance for ALL blues
                # PencilNodes: provenance is base pair (a,b)
                # PairNodes: provenance is also (a,b)
                if is_pencil(t):
                    _, _, _, ta, tb = t
                    new_prov[new_map[t]].add(unordered_pair(ta, tb))
                elif is_pair(t):
                    _, L, R = t
                    if is_leaf(L) and is_leaf(R):
                        new_prov[new_map[t]].add(unordered_pair(L, R))

    # Update state
    next_blues = S.blues | set(new_map.values())
    next_prov = dict(S.prov)
    for lbl, pairs in new_prov.items():
        next_prov.setdefault(lbl, set()).update(pairs)

    return State(day=S.day+1, blues=next_blues, prov=next_prov)

def solve(n: int) -> List[int]:
    """Solve to day n"""
    S = init_state()
    hist = [len(S.blues)]

    for day in range(n):
        S = expand_one_day(S)
        hist.append(len(S.blues))
        print(f"g({day+1}) = {hist[-1]} (day {day} → {day+1})")

    return hist

if __name__ == "__main__":
    print("="*80)
    print("GENESIS SYMBOLIC SOLVER - FIXED VERSION")
    print("="*80)
    print()
    print("KEY FIX: Proper pencil-index encoding at day 0→1")
    print()

    # Test base cases
    h = solve(2)
    print()
    print("VERIFICATION:")
    print(f"  g(1) = {h[1]} (expected: 8)")
    print(f"  g(2) = {h[2]} (expected: 28)")
    print()

    if h[1] != 8:
        print(f"✗ g(1) WRONG! Got {h[1]}, expected 8")
        print("  The pencil-index fix did not work as expected")
    elif h[2] != 28:
        print(f"✗ g(2) WRONG! Got {h[2]}, expected 28")
        print("  Pappus merge may need adjustment")
    else:
        print("✓ Base cases CORRECT!")
        print()
        print("Extending to g(5) for full verification...")
        h = solve(5)
        print()
        print(f"g(5) = {h[5]:,} (expected: 19,068)")

        if h[5] == 19068:
            print("✓ g(5) VERIFIED! Symbolic solver is CORRECT!")
            print()
            print("Computing g(16)...")
            h = solve(16)
            print()
            print("="*80)
            print("✓ SUCCESS!")
            print("="*80)
            print()
            print(f"ANSWER: g(16) = {h[16]:,}")
        else:
            print(f"✗ g(5) WRONG! Got {h[5]:,}, expected 19,068")
            print("  Higher-order Pappus/Desargues merges need implementation")

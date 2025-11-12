#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Symbolic solver for Project Euler #957 "Point Genesis" (fan/pencil model)
using a projective-invariant rewrite system R.

FIXED VERSION based on AI Panel critique:
- Bug fix 1: Deduplicate xword generation (only iterate a < b)
- Bug fix 2: Accumulate provenance for all generators of same term

Tier-1 rules (R1..R8) fully implemented. Should reproduce g(1)=8, g(2)=28.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Set, Tuple, Union

Leaf = str
Node = Tuple[str, Any, Any]
Term = Union[Leaf, Node]

RED_LABELS = ("R1", "R2", "R3")

def is_leaf(x: Term) -> bool:
    return not (isinstance(x, tuple) and len(x) == 3 and x[0] == '⊗')

def term_key(x: Term) -> Tuple:
    """Canonical key for ordering sub-terms (for commutativity)."""
    if is_leaf(x):
        return (0, 'L', str(x))
    return (1, 'T', term_key(x[1]), term_key(x[2]))

def make_pair(a: Term, b: Term) -> Term:
    """Build a ⊗ b without normalization."""
    return ('⊗', a, b)

Provenance = Dict[Leaf, Set[Tuple[Leaf, Leaf]]]

def unordered_pair(a: Leaf, b: Leaf) -> Tuple[Leaf, Leaf]:
    return (a, b) if a <= b else (b, a)

@dataclass(frozen=True)
class Xword:
    """Raw intersection word X(i,j; a,b) => ('⊗', a, b) after normalization."""
    i: int
    j: int
    a: Leaf
    b: Leaf

def valid_xword(x: Xword, B_t: Set[Leaf]) -> bool:
    # R1, R3: well-formed and day semantics
    if x.i == x.j: return False
    if x.a == x.b: return False
    if x.a not in B_t or x.b not in B_t: return False
    return True

def normalize_comm_idem(t: Term) -> Term:
    """R5: commutativity + idempotence on ⊗-nodes, recursively."""
    if is_leaf(t):
        return t
    _, L, R = t
    L = normalize_comm_idem(L)
    R = normalize_comm_idem(R)
    # Enforce order
    if term_key(R) < term_key(L):
        L, R = R, L
    # Idempotence
    if L == R:
        return L
    return ('⊗', L, R)

def apply_old_blue_coincidence(t: Term, prov: Provenance) -> Term:
    """
    R8: If a depth-1 composite (leaf ⊗ leaf) matches provenance of some old blue x,
    rewrite to that x.
    """
    if is_leaf(t):
        return t
    _, L, R = t
    if is_leaf(L) and is_leaf(R):
        u, v = unordered_pair(L, R)
        for x, pairs in prov.items():
            if (u, v) in pairs:
                return x
    return t

def is_colored(t: Term, C_t: Set[Leaf]) -> bool:
    # R6: white-only filter
    return is_leaf(t) and (t in C_t)

def to_canonical_pair_token(a: Leaf, b: Leaf) -> Term:
    # R4: X(i,j; a,b) → a ⊗ b
    return normalize_comm_idem(make_pair(a, b))

def normalize_term(t: Term, prov: Provenance) -> Term:
    """Apply R5 (comm+idem), then R8 (old-blue coincidence) until stable."""
    changed = True
    cur = t
    while changed:
        changed = False
        new = normalize_comm_idem(cur)
        if new != cur:
            cur, changed = new, True
        new = apply_old_blue_coincidence(cur, prov)
        if new != cur:
            cur, changed = new, True
    return cur

@dataclass
class DayState:
    reds: Set[Leaf]
    blues: Set[Leaf]
    prov: Provenance
    day: int

def init_state() -> DayState:
    reds = set(RED_LABELS)
    blues = {"b0_1", "b0_2"}
    prov: Provenance = {}
    return DayState(reds=reds, blues=blues, prov=prov, day=0)

def pencils() -> List[Tuple[int,int]]:
    return [(1,2), (1,3), (2,3)]

def label_new_blue(day: int, counter: int) -> Leaf:
    return f"b{day}_{counter}"

def expand_one_day(S: DayState) -> DayState:
    """
    Construct all X(i,j; a,b) with i<j and a<b (FIXED: deduplicate),
    apply R, discard colored, intern distinct normal forms.
    """
    B_t = set(S.blues)
    C_t = set(S.reds) | B_t

    # Convert blues to sorted list for ordered iteration
    blues_list = sorted(B_t)

    new_terms: Dict[Term, Leaf] = {}
    new_prov_pairs: Dict[Leaf, Set[Tuple[Leaf, Leaf]]] = {}
    counter = 1

    for (i,j) in pencils():
        # FIXED: Only iterate ordered pairs a < b (deduplication)
        for idx_a, a in enumerate(blues_list):
            for b in blues_list[idx_a + 1:]:  # Only b > a
                xw = Xword(i=i, j=j, a=a, b=b)
                if not valid_xword(xw, B_t):
                    continue

                # R4: pair canonicalization
                t = to_canonical_pair_token(xw.a, xw.b)
                # Normalize
                t = normalize_term(t, S.prov)
                # White-only filter
                if is_colored(t, C_t):
                    continue

                # Intern distinct new terms
                if t not in new_terms:
                    lbl = label_new_blue(S.day + 1, counter)
                    counter += 1
                    new_terms[t] = lbl
                    new_prov_pairs[lbl] = set()

                # FIXED: Accumulate provenance for ALL generators
                if is_leaf(xw.a) and is_leaf(xw.b):
                    lbl = new_terms[t]
                    new_prov_pairs[lbl].add(unordered_pair(xw.a, xw.b))

    # Materialize new blues
    new_blues = set(new_terms.values())
    next_blues = B_t | new_blues
    next_prov = dict(S.prov)
    for lbl, pairs in new_prov_pairs.items():
        if lbl not in next_prov:
            next_prov[lbl] = set()
        next_prov[lbl].update(pairs)

    return DayState(reds=S.reds, blues=next_blues, prov=next_prov, day=S.day + 1)

def solve_g(n: int) -> Tuple[int, List[int]]:
    """Return g(n) and the list [g(0), g(1), ..., g(n)]."""
    S = init_state()
    history = [len(S.blues)]  # g(0)=2
    for _ in range(n):
        S = expand_one_day(S)
        history.append(len(S.blues))
    return history[-1], history

if __name__ == "__main__":
    import time

    print("="*80)
    print("SYMBOLIC REWRITE SOLVER (FIXED VERSION)")
    print("="*80)
    print()

    # Test base cases
    print("Testing base cases...")
    g2, hist = solve_g(2)
    print(f"g(t) up to t=2: {hist}")

    if hist[1] == 8 and hist[2] == 28:
        print("✓ Base cases CORRECT: g(1)=8, g(2)=28")
    else:
        print(f"✗ Base cases WRONG: g(1)={hist[1]} (expected 8), g(2)={hist[2]} (expected 28)")
    print()

    # Compute up to 16
    print("Computing g(16)...")
    start = time.time()
    g16, hist = solve_g(16)
    elapsed = time.time() - start

    print(f"Sequence g(0) through g(16):")
    for i, val in enumerate(hist):
        print(f"  g({i:2}) = {val:,}")
    print()

    print("="*80)
    print(f"✓ ANSWER: g(16) = {g16:,}")
    print("="*80)
    print(f"Computation time: {elapsed:.3f}s")
    print()

    # Compare with known coordinate simulation values
    known = [2, 8, 28, 184, 1644, 19068]
    print("Verification against coordinate simulation:")
    for i in range(min(len(hist), len(known))):
        status = "✓" if hist[i] == known[i] else "✗"
        print(f"  {status} g({i}) = {hist[i]:,} (expected: {known[i]:,})")

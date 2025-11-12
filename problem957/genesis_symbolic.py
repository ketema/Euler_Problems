# genesis_symbolic.py
from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Dict, List, Set, Tuple, Union

Leaf = str
Node = Tuple[str, Any, Any]  # ('⊗', left, right)
Term = Union[Leaf, Node]

RED = {"R1","R2","R3"}

def is_leaf(t: Term)->bool:
    return not (isinstance(t, tuple) and len(t)==3 and t[0]=='⊗')

def term_key(t: Term):
    if is_leaf(t): return (0,str(t))
    _,L,R=t
    return (1,term_key(L),term_key(R))

def op(a: Term,b: Term)->Term:
    if is_leaf(a) and is_leaf(b) and a==b: return a         # idempotence
    # commutativity via ordering
    if term_key(b) < term_key(a): a,b=b,a
    return ('⊗',a,b)

def unordered_pair(a: Leaf,b: Leaf)->Tuple[Leaf,Leaf]:
    return (a,b) if a<=b else (b,a)

@dataclass
class State:
    day:int
    blues:Set[Leaf]
    prov:Dict[Leaf, Set[Tuple[Leaf,Leaf]]]  # each blue -> set of base unordered pairs

def init_state()->State:
    return State(day=0, blues={"b0_1","b0_2"}, prov={})

# ---------- Tier-1 rules: exact & safe ----------

def normalize_pair(a:Leaf,b:Leaf)->Term:
    # R4: X(i,j;a,b) -> a ⊗ b  with comm+idem baked via op
    return op(a,b)

def old_blue_coincidence(t:Term, S:State)->Term:
    # R8: if t is depth-1 pair (leaf ⊗ leaf) matching provenance of some old blue, rewrite to that blue
    if is_leaf(t): return t
    _,L,R=t
    if not(is_leaf(L) and is_leaf(R)): return t
    key = unordered_pair(L,R)
    for x,pairs in S.prov.items():
        if key in pairs: return x
    return t

def is_colored(t:Term,S:State)->bool:
    return is_leaf(t) and (t in RED or t in S.blues)

# ---------- Tier-2: guarded classical merges (safe defaults) ----------

def pappus_guard_merge(t:Term, S:State)->Term:
    """
    Safe schema (conservative):
    ((A⊗B) ⊗ (C⊗D))  ->  ((A⊗D) ⊗ (C⊗B))
    BUT ONLY IF provenance proves that (A,C,...) come from one range and (B,D,...) from another.
    We approximate that with: inner pairs are *both* depth-1 pairs whose unordered leaves
    have appeared together as pairs generating some day-(≤ current) blues (i.e., known ranges).
    If not provable, do nothing.
    """
    if is_leaf(t): return t
    _, L, R = t
    if is_leaf(L) or is_leaf(R): return t
    # inner pairs:
    if is_leaf(L[1]) and is_leaf(L[2]) and is_leaf(R[1]) and is_leaf(R[2]):
        A,B = L[1],L[2]
        C,D = R[1],R[2]
        # Guard: both {A,B} and {C,D} have been used as provenance of some existing blue
        key1 = unordered_pair(A,B)
        key2 = unordered_pair(C,D)
        seen1 = any(key1 in pairs for pairs in S.prov.values())
        seen2 = any(key2 in pairs for pairs in S.prov.values())
        if seen1 and seen2:
            return op( op(A,D), op(C,B) )
    return t

def desargues_guard_merge(t:Term, S:State)->Term:
    """
    Safe schema (conservative):
    (((a⊗b) ⊗ (a'⊗b')) ⊗ ((b⊗c) ⊗ (b'⊗c'))) -> ((a⊗c) ⊗ (a'⊗c'))
    Guard: all inner pairs are depth-1, and the three base pairs {a,b},{b,c},{a,c} (and primed)
    have occurred in provenance (so triangles are 'realized' in the structure).
    """
    if is_leaf(t): return t
    _, L, R = t
    if is_leaf(L) or is_leaf(R): return t
    # Expect L = (⊗,(a⊗b),(a'⊗b')), R = (⊗,(b⊗c),(b'⊗c'))
    def get_pair(node)->Tuple[Leaf,Leaf] | None:
        if is_leaf(node): return None
        _, x, y = node
        if is_leaf(x) and is_leaf(y): return unordered_pair(x,y)
        return None
    La = get_pair(L[1]); La2 = get_pair(L[2])
    Rb = get_pair(R[1]); Rc  = get_pair(R[2])
    if not(La and La2 and Rb and Rc): return t
    # reconstruct names
    a,b = La
    a_,b_ = La2
    bb,c = Rb  # pair containing b
    b__,c_ = Rc
    if b!=bb or b_!=b__:
        return t
    # guard: required base pairs must have occurred in provenance at least once
    need = [unordered_pair(a,c), unordered_pair(a_,c_), unordered_pair(a,b),
            unordered_pair(b,c), unordered_pair(a_,b_), unordered_pair(b_,c_)]
    ok = all(any(p in pairs for pairs in S.prov.values()) for p in need)
    if ok:
        return op( op(a,c), op(a_,c_) )
    return t

def normalize(t:Term, S:State)->Term:
    # iterate Tier-1 comm+idem (built into op), then R8, then guarded Tier-2 merges to fixed point
    changed=True
    cur=t
    while changed:
        changed=False
        new = old_blue_coincidence(cur,S)
        if new!=cur:
            cur,changed=new,True
            continue
        new = pappus_guard_merge(cur,S)
        if new!=cur:
            cur,changed=new,True
            continue
        new = desargues_guard_merge(cur,S)
        if new!=cur:
            cur,changed=new,True
            continue
    return cur

# ---------- Day evolution ----------

def expand_one_day(S:State)->State:
    B = set(S.blues)
    colored = RED | B
    new_map: Dict[Term, Leaf] = {}
    new_prov: Dict[Leaf, Set[Tuple[Leaf,Leaf]]] = {}
    c = 1
    for (i,j) in [(1,2),(1,3),(2,3)]:
        for a in B:
            for b in B:
                if a==b: continue
                t = normalize_pair(a,b)
                t = normalize(t,S)
                if is_colored(t,S): continue
                if t not in new_map:
                    lbl = f"b{S.day+1}_{c}"
                    c+=1
                    new_map[t]=lbl
                    new_prov[lbl]=set()
                # provenance only for depth-1 pairs
                if is_leaf(t):
                    pass
                else:
                    _,L,R=t
                    if is_leaf(L) and is_leaf(R):
                        new_prov[new_map[t]].add(unordered_pair(L,R))
    # materialize
    next_blues = B | set(new_map.values())
    next_prov = dict(S.prov)
    for lbl,pairs in new_prov.items():
        next_prov.setdefault(lbl,set()).update(pairs)
    return State(day=S.day+1, blues=next_blues, prov=next_prov)

def solve(n:int)->List[int]:
    S=init_state()
    hist=[len(S.blues)]
    for _ in range(n):
        S=expand_one_day(S)
        hist.append(len(S.blues))
    return hist

if __name__=="__main__":
    h=solve(2)
    print("g(t) up to 2:",h)
    assert h[1]==8 and h[2]==28, f"Base check failed: {h}"
    print("✓ g(1)=8, g(2)=28")
    # Try more (you can change this)
    h=solve(5)
    print("g(t) up to 5:",h)

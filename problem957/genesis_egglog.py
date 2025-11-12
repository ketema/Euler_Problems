#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import subprocess, tempfile, textwrap, shutil
from pathlib import Path
from itertools import combinations, product

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

def egg_escape(s: str) -> str:
    # Point names are atoms; we'll keep them simple (letters, digits, underscore).
    return s

def mk_egg_program(reds, blues, prov_pairs, candidates, nested_quads):
    """
    Build a single egglog program string for one day:
      - declare points (reds, blues)
      - guarded Pappus/Desargues rules
      - assert guard facts from provenance (on_same_range, triangle_edge)
      - define candidate expressions (pair A B), label them CAND_k
      - define nested expressions only when guards permit (to trigger merges)
      - run saturation; output canonical reps for each candidate
    """
    # Reds and blues are now declared as datatype constructors
    # Guard facts: we use provenance pairs both as "on_same_range" and "triangle_edge"
    facts = []
    for (u, v) in sorted(prov_pairs):
        facts.append(f"(assert (on_same_range {egg_escape(u)} {egg_escape(v)}))")
        facts.append(f"(assert (on_same_range {egg_escape(v)} {egg_escape(u)}))")
        facts.append(f"(assert (triangle_edge {egg_escape(u)} {egg_escape(v)}))")
        facts.append(f"(assert (triangle_edge {egg_escape(v)} {egg_escape(u)}))")

    # Candidate pair terms (these are the would-be new points)
    cands = []
    for idx, (a, b) in enumerate(candidates, start=1):
        cands.append(f"(define CAND_{idx} (pair {egg_escape(a)} {egg_escape(b)}))")

    # Nested terms (only for guarded quadruples/pairs so saturation can merge)
    nests = []
    for (A, B, C, D) in nested_quads:
        nests.append(f"(define NEST_P_{A}_{B}_{C}_{D} (pair (pair {A} {B}) (pair {C} {D})))")

    # For Desargues we’ll prepare only small patterns when all six edges exist;
    # we don’t need to enumerate all, guards will limit this anyway.
    # To keep this harness compact, we skip explicit Desargues nest building:
    # egglog can derive them via rules once enough pieces exist.
    # (If you want, add analogous 'NEST_D_...' defines.)

    prog = f"""
    ; ---------- sorts and functions ----------
    (datatype Point
        {" ".join(f"({r})" for r in sorted(reds))}
        {" ".join(f"({b})" for b in sorted(blues))})
    (function pair (Point Point) Point :default old)

    ; ---------- rewrite rules for commutativity and idempotency ----------
    (rewrite (pair x y) (pair y x))
    (rewrite (pair x x) x)

    ; ---------- relations for guards ----------
    (relation on_same_range (Point Point))
    (relation triangle_edge (Point Point))

    ; ---------- guarded Pappus (using rule with conditions) ----------
    (rule ((= e (pair (pair a b) (pair c d)))
           (on_same_range a c)
           (on_same_range b d))
          ((union e (pair (pair a d) (pair c b)))))

    ; ---------- guarded Desargues ----------
    (rule ((= e (pair (pair (pair a b) (pair a2 b2))
                      (pair (pair b c) (pair b2 c2))))
           (triangle_edge a b)
           (triangle_edge b c)
           (triangle_edge a c)
           (triangle_edge a2 b2)
           (triangle_edge b2 c2)
           (triangle_edge a2 c2))
          ((union e (pair (pair a c) (pair a2 c2)))))

    ; ---------- ready for assertions ----------
    ; ---------- guard facts from provenance ----------
    {"".join(facts)}

    ; ---------- candidates (possible new points today) ----------
    {"".join(cands)}

    ; ---------- nested expressions likely to trigger merges ----------
    {"".join(nests)}

    ; ---------- saturate ----------
    (run)

    ; ---------- output canonical reps for each candidate ----------
    {"".join(f"(output CAND_{i})" for i in range(1, len(candidates)+1))}
    """
    return textwrap.dedent(prog)

def run_egglog(program: str) -> str:
    exe = shutil.which("egglog")
    if not exe:
        raise RuntimeError("egglog binary not found in PATH (run `cargo install egglog`).")
    with tempfile.TemporaryDirectory() as td:
        p = Path(td) / "prog.egg"
        p.write_text(program, encoding="utf-8")
        out = subprocess.run([exe, str(p)], capture_output=True, text=True)
        if out.returncode != 0:
            print(f"ERROR: egglog failed with code {out.returncode}")
            print(f"STDERR: {out.stderr}")
            print(f"STDOUT: {out.stdout}")
            print(f"\nGenerated program:")
            print(program)
            raise RuntimeError(f"egglog failed: {out.stderr}")
        return out.stdout

def parse_outputs(stdout: str):
    """
    egglog prints each (output EXPR) as a line like:
      EXPR := <pretty-printed representative>
    We dedupe these strings to get canonical classes.
    """
    reps = []
    for line in stdout.splitlines():
        if ":=" in line:
            rhs = line.split(":=", 1)[1].strip()
            reps.append(rhs)
    return reps

# ------------------------------------------------------------
# Day loop
# ------------------------------------------------------------

def day_step(reds, blues, prov_pairs, build_nested=True):
    """
    Return (new_blues, new_pairs, reps_debug) for this day.
    - reds, blues: sets of names
    - prov_pairs: set of unordered pairs (u,v) that have occurred so far (provenance)
    """
    colored = set(reds) | set(blues)

    # All unordered candidate pairs from current blues
    pairs = [tuple(sorted(p)) for p in combinations(blues, 2)]

    # Candidates are 'pair a b' for all pairs not already colored (white-only handled later)
    candidates = []
    for (a, b) in pairs:
        candidates.append((a, b))

    # Build Pappus nested only when both inner pairs are in prov (guards)
    nested_quads = set()
    if build_nested:
        # only consider quadruples that meet the guard:
        # {A,B} and {C,D} both in prov_pairs  -> allows Pappus on (pair (A B) (C D))
        prov_list = list(prov_pairs)
        for (A, B), (C, D) in combinations(prov_list, 2):
            nested_quads.add(tuple(map(egg_escape, (A, B, C, D))))

    prog = mk_egg_program(reds, blues, prov_pairs, candidates, nested_quads)
    stdout = run_egglog(prog)
    reps = parse_outputs(stdout)

    # Count unique canonical reps among candidates
    rep_map = {}  # rep string -> set of generating pairs
    for (a, b), r in zip(candidates, reps):
        rep_map.setdefault(r, set()).add((a, b))

    # White-only filter: drop reps that equal an already-colored atom
    # (If egglog reduces pair(a,b) to b (old-blue), rhs will be 'b')
    new_reps = {}
    for rep, gens in rep_map.items():
        # Detect if rep is a single atom matching an old point
        is_atom = rep.isidentifier()
        if is_atom and rep in colored:
            continue
        new_reps[rep] = gens

    # Mint labels for distinct new canonical reps
    new_blues = set()
    new_pairs = set(prov_pairs)
    counter = 1
    for rep, gens in new_reps.items():
        lbl = f"b{len(blues)+len(new_blues)+counter}"
        counter += 1
        new_blues.add(lbl)
        # Record all generating unordered pairs for provenance
        for (u, v) in gens:
            new_pairs.add(tuple(sorted((u, v))))

    return new_blues, new_pairs, reps

def solve_g_egglog(n_days: int):
    reds = {"R1", "R2", "R3"}
    blues = {"b0_1", "b0_2"}
    prov_pairs = set()  # unordered base pairs seen in provenance
    history = [len(blues)]
    for t in range(1, n_days+1):
        new_blues, prov_pairs, reps_debug = day_step(reds, blues, prov_pairs, build_nested=True)
        blues |= new_blues
        history.append(len(blues))
        print(f"[bold]Day {t}[/]: new={len(new_blues)} total={len(blues)}")
    return history

if __name__ == "__main__":
    hist = solve_g_egglog(2)
    print("g(t) up to 2:", hist)
    # You should see g(1)=8, g(2)=28 once your egglog printing matches the parse.


"""
ASCII Art Visualization of the 5-Fan Drawing
"""

def draw_day_0():
    """Draw Day 0 configuration"""
    print("=" * 80)
    print("DAY 0: g(0) = 2")
    print("5 Red vertices (fan centers), 2 Blue vertices")
    print("=" * 80)
    print()

    art = """
                          r1
                          ●
                         /|\\
                        / | \\
                       /  |  \\
                      /   |   \\
                     /   b1    \\
                    /     ●     \\
                   /      |      \\
                  /       |       \\
            r2   /        |        \\   r5
            ●---/---------+---------\\---●
             \\ /          |          \\ /
              X          b2           X
             / \\          ●          / \\
            /   \\         |         /   \\
           /     \\        |        /     \\
          /       \\       |       /       \\
         ●---------\\------+------/---------●
        r3          \\     |     /         r4
                     \\    |    /
                      \\   |   /
                       \\  |  /
                        \\ | /
                         \\|/

    """

    print(art)

    print("\nCyclic Orders (viewing from each red outward):")
    print("-" * 60)
    print("  r1: [b1, b2]  → sees b1 then b2 going clockwise")
    print("  r2: [b2, b1]  → sees b2 then b1 (REVERSED!)  ←")
    print("  r3: [b2, b1]  → sees b2 then b1 (REVERSED!)  ←")
    print("  r4: [b2, b1]  → sees b2 then b1 (REVERSED!)  ←")
    print("  r5: [b1, b2]  → sees b1 then b2")
    print()

    print("Segment Pairs:")
    print("-" * 60)
    print("  Each red connects to both blues → 5 × 2 = 10 segments")
    print("  Segments: (r1,b1), (r1,b2), (r2,b1), (r2,b2), ...")
    print()

    print("Crossing Analysis:")
    print("-" * 60)
    print("  Red pairs with DIFFERENT orders → segments cross")
    print()
    print("  Disagreeing pairs:")
    print("    r1 vs r2: [1,2] vs [2,1] → (r1,b1)×(r2,b2) CROSSES ✓")
    print("    r1 vs r3: [1,2] vs [2,1] → (r1,b1)×(r3,b2) CROSSES ✓")
    print("    r1 vs r4: [1,2] vs [2,1] → (r1,b1)×(r4,b2) CROSSES ✓")
    print("    r5 vs r2: [1,2] vs [2,1] → (r5,b1)×(r2,b2) CROSSES ✓")
    print("    r5 vs r3: [1,2] vs [2,1] → (r5,b1)×(r3,b2) CROSSES ✓")
    print("    r5 vs r4: [1,2] vs [2,1] → (r5,b1)×(r4,b2) CROSSES ✓")
    print()
    print("  Agreeing pairs:")
    print("    r1 vs r5: [1,2] vs [1,2] → no crossings")
    print("    r2 vs r3: [2,1] vs [2,1] → no crossings")
    print("    r2 vs r4: [2,1] vs [2,1] → no crossings")
    print("    r3 vs r4: [2,1] vs [2,1] → no crossings")
    print()
    print("  TOTAL CROSSINGS: 6")
    print()

def draw_crossing_detail():
    """Show detailed crossing example"""
    print("=" * 80)
    print("HOW TWO SEGMENTS CROSS (Interleaving Orders)")
    print("=" * 80)
    print()

    print("Example: Do (r1, b1) and (r2, b2) cross?")
    print()

    print("Step 1: Check order around r1")
    print("-" * 60)
    print()
    print("         Cyclic order around r1: [b1, b2]")
    print()
    print("                    r1")
    print("                    ●")
    print("                   /|\\")
    print("                  / | \\")
    print("            [1]→ /  |  \\ ←[2]")
    print("               b1   |   b2")
    print("                ●   |    ●")
    print()
    print("         At r1: b1 comes BEFORE b2")
    print()

    print()
    print("Step 2: Check order around r2")
    print("-" * 60)
    print()
    print("         Cyclic order around r2: [b2, b1]")
    print()
    print("                    r2")
    print("                    ●")
    print("                   /|\\")
    print("                  / | \\")
    print("            [1]→ /  |  \\ ←[2]")
    print("               b2   |   b1")
    print("                ●   |    ●")
    print()
    print("         At r2: b2 comes BEFORE b1 (REVERSED!)")
    print()

    print()
    print("Step 3: Interleaving Test")
    print("-" * 60)
    print()
    print("  Order at r1: b1 < b2  (b1 first)")
    print("  Order at r2: b2 < b1  (b2 first)")
    print()
    print("  Orders DISAGREE → Segments INTERLEAVE → CROSSING!")
    print()

    print()
    print("Visualization of Crossing:")
    print("-" * 60)
    print()
    print("       r1                    r2")
    print("        ●                     ●")
    print("        |\\                   /|")
    print("        | \\                 / |")
    print("        |  \\     NEW      /  |")
    print("        |   \\    BLUE    /   |")
    print("    b1  |    \\    ●!    /    |  b1")
    print("     ●--|-----\\   ↑   /-----|--●")
    print("        |      \\ / \\ /      |")
    print("        |       X   X       |  ← segments cross here!")
    print("        |      / \\ / \\      |")
    print("     ●--|-----/   ↓   \\-----|--●")
    print("    b2  |    /    ●!    \\    |  b2")
    print("        |   /            \\   |")
    print("        |  /              \\  |")
    print("        | /                \\ |")
    print("        |/                  \\|")
    print()
    print("  Segment (r1→b1) crosses segment (r2→b2)")
    print("  Segment (r1→b2) crosses segment (r2→b1)")
    print()
    print("  For 1 pair of blues + 1 pair of disagreeing reds:")
    print("    → 1 crossing (new blue created)")
    print()

def draw_day_1():
    """Draw Day 1 result"""
    print()
    print("=" * 80)
    print("DAY 1: g(1) = 8")
    print("5 Red vertices, 8 Blue vertices (2 original + 6 new)")
    print("=" * 80)
    print()

    print("Original blues: b1, b2")
    print("New blues from crossings: b3, b4, b5, b6, b7, b8")
    print()

    art = """
                          r1
                          ●
                         /|\\
                        / | \\
                   b3● /  |  \\ ●b5
                      /  b1   \\
                     /    ●    \\
                    /     |     \\
                   /      |      \\
              b7● /       |       \\ ●b8
            r2   /        |        \\   r5
            ●---/---------+---------\\---●
             \\ /          |          \\ /
              X          b2           X
             / \\          ●          / \\
            /   \\         |         /   \\
           /  b4●\\        |        /●b6  \\
          /       \\       |       /       \\
         ●---------\\------+------/---------●
        r3          \\     |     /         r4
                     \\    |    /
                      \\   |   /
                       \\  |  /
                        \\ | /
                         \\|/

    """
    print(art)

    print("Total segments: 5 reds × 8 blues = 40 segments")
    print()
    print("Next step (Day 1→2):")
    print("  Check all pairs of reds for crossings among 8 blues")
    print("  Using cyclic orders updated with new blues")
    print("  Result: 20 new crossings → g(2) = 28")
    print()

def show_formula():
    """Show the formula and sequence"""
    print()
    print("=" * 80)
    print("FORMULA AND SEQUENCE")
    print("=" * 80)
    print()

    print("Formula: g(t) = 7t² - t + 2")
    print()
    print("Day | g(t) | New blues | Formula check")
    print("----|------|-----------|---------------")
    print(" 0  |   2  |    -      |   2")
    print(" 1  |   8  |    6      |   8   ✓")
    print(" 2  |  28  |   20      |  28   ✓")
    print(" 3  |  62  |   34      |  62")
    print(" 4  | 110  |   48      | 110")
    print(" 5  | 172  |   62      | 172")
    print("... | ...  |   ...     | ...")
    print(" 16 | 1778 |  168      | 1778  ← ANSWER")
    print()

    print("Key insight: Quadratic growth!")
    print("  New crossings ≈ O(t) per day")
    print("  Total blues ≈ O(t²)")
    print()

def main():
    draw_day_0()
    print("\n" * 2)
    draw_crossing_detail()
    print("\n" * 2)
    draw_day_1()
    print("\n" * 2)
    show_formula()

    # Save to file
    import sys
    from io import StringIO

    # Capture output
    old_stdout = sys.stdout
    sys.stdout = StringIO()
    draw_day_0()
    print("\n" * 2)
    draw_crossing_detail()
    print("\n" * 2)
    draw_day_1()
    print("\n" * 2)
    show_formula()
    output = sys.stdout.getvalue()
    sys.stdout = old_stdout

    # Write to file
    with open('/home/user/Euler_Problems/problem957/VISUALIZATION.txt', 'w') as f:
        f.write(output)

    print("\n✓ Saved to VISUALIZATION.txt")

if __name__ == '__main__':
    main()

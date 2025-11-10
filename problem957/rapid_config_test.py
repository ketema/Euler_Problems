"""
Rapidly test many geometric configurations to find which gives
tractable growth pattern for g(1)=8, g(2)=28

Strategy: Test standard geometric shapes and configurations
"""

from fractions import Fraction
import math

def line_intersection(p1, p2, p3, p4):
    x1, y1 = p1
    x2, y2 = p2
    x3, y3 = p3
    x4, y4 = p4
    denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
    if abs(denom) < 1e-10:
        return None
    t = ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)) / denom
    return (x1 + t*(x2-x1), y1 + t*(y2-y1))

def simulate_days(reds, blues, max_days=4, max_points=500):
    """Simulate and return g values. Stop early if explodes."""
    g = [len(blues)]

    for day in range(max_days):
        lines = [(r, b) for r in reds for b in blues]
        new = set()

        for i, (r1, b1) in enumerate(lines):
            for r2, b2 in lines[i+1:]:
                if r1 == r2 or b1 == b2:
                    continue
                pt = line_intersection(r1, b1, r2, b2)
                if pt and pt not in blues and pt not in reds:
                    new.add(pt)

        if len(new) == 0:
            return None  # Degenerate

        blues = blues + list(new)
        g.append(len(blues))

        if len(blues) > max_points:
            return None  # Explodes

    return g

def test_config(name, reds, blues):
    """Test a configuration."""
    g = simulate_days(reds, blues, max_days=4, max_points=500)

    if g is None:
        return None

    if g[1] == 8 and g[2] == 28:
        print(f"✓✓✓ {name}: g = {g[:5]}")
        return g
    elif g[1] == 8:
        print(f"  ✓ {name}: g(1)=8, but g(2)={g[2]} (need 28)")
        return None
    else:
        return None

print("="*70)
print("RAPID CONFIGURATION TESTING")
print("="*70)
print()

configs = []

# 1. EQUILATERAL TRIANGLE + symmetric blues
print("1. Equilateral triangle (3 reds) + symmetric blues")
s3 = Fraction(3)**Fraction(1,2) if False else math.sqrt(3)
for bx, by in [(0, 0), (0.5, 0), (0.5, s3/6), (0, -0.5), (0.5, -0.3)]:
    for bx2, by2 in [(1, 0), (0.25, s3/4), (0.75, s3/4), (0.5, s3/3)]:
        if (bx, by) == (bx2, by2):
            continue
        reds = [(0, 0), (1, 0), (0.5, s3/2)]
        blues = [(bx, by), (bx2, by2)]
        g = test_config(f"  Equilat+({bx:.2f},{by:.2f}),({bx2:.2f},{by2:.2f})", reds, blues)
        if g:
            configs.append(("Equilateral", reds, blues, g))

# 2. UNIT CIRCLE - 3 reds on circle, blues at center or on circle
print("\n2. Circle: 3 reds on unit circle")
angles_3 = [0, 2*math.pi/3, 4*math.pi/3]  # 120° apart
reds_circle = [(math.cos(a), math.sin(a)) for a in angles_3]

for b1 in [(0, 0), (0.5, 0), (0, 0.5), (math.cos(math.pi/6), math.sin(math.pi/6))]:
    for b2 in [(0.2, 0.3), (0.3, -0.2), (math.cos(math.pi/4), math.sin(math.pi/4))]:
        if b1 == b2:
            continue
        g = test_config(f"  Circle+({b1[0]:.2f},{b1[1]:.2f})", reds_circle, [b1, b2])
        if g:
            configs.append(("Circle", reds_circle, [b1, b2], g))

# 3. RIGHT TRIANGLE
print("\n3. Right triangle (3 reds)")
reds_right = [(0, 0), (1, 0), (0, 1)]
for b1 in [(0.3, 0.3), (0.5, 0.5), (0.25, 0.25), (-0.5, -0.5), (2, 2)]:
    for b2 in [(0.7, 0.2), (0.2, 0.7), (0.4, 0.6), (-1, 0), (0, -1)]:
        if b1 == b2:
            continue
        g = test_config(f"  RightTri+({b1[0]:.2f},{b1[1]:.2f})", reds_right, [b1, b2])
        if g:
            configs.append(("RightTriangle", reds_right, [b1, b2], g))

# 4. COLLINEAR configurations (on a line) - degenerate but might have special structure
print("\n4. Reds collinear (degenerate but trying)")
reds_line = [(0, 0), (1, 0), (2, 0)]
for b1 in [(0, 1), (1, 1), (0.5, 2)]:
    for b2 in [(0, 2), (1, 2), (0.5, -1)]:
        if b1 == b2:
            continue
        g = test_config(f"  Collinear+({b1[0]:.2f},{b1[1]:.2f})", reds_line, [b1, b2])
        if g:
            configs.append(("Collinear", reds_line, [b1, b2], g))

# 5. ALL 5 POINTS ON A CIRCLE
print("\n5. All 5 points on circle (Pascal's theorem)")
angles_5 = [i * 2 * math.pi / 5 for i in range(5)]
pts_5 = [(math.cos(a), math.sin(a)) for a in angles_5]
reds_5circle = pts_5[:3]
blues_5circle = pts_5[3:5]
g = test_config("  Pentagon on circle", reds_5circle, blues_5circle)
if g:
    configs.append(("Pentagon on circle", reds_5circle, blues_5circle, g))

# 6. ALL 5 POINTS ON PARABOLA y = x²
print("\n6. All 5 points on parabola y = x²")
xs = [-1, -0.5, 0, 0.5, 1]
parab_pts = [(x, x*x) for x in xs]
g = test_config("  Parabola y=x²", parab_pts[:3], parab_pts[3:5])
if g:
    configs.append(("Parabola", parab_pts[:3], parab_pts[3:5], g))

# 7. HYPERBOLA x² - y² = 1
print("\n7. All 5 points on hyperbola x² - y² = 1")
# Points: (±cosh(t), sinh(t))
ts = [0, 0.5, 1, -0.5, -1]
hyp_pts = [(math.cosh(t), math.sinh(t)) for t in ts]
g = test_config("  Hyperbola x²-y²=1", hyp_pts[:3], hyp_pts[3:5])
if g:
    configs.append(("Hyperbola", hyp_pts[:3], hyp_pts[3:5], g))

# 8. RHOMBUS/DIAMOND - reds at 3 vertices
print("\n8. Diamond/Rhombus")
reds_diamond = [(0, 0), (1, 0), (0.5, math.sqrt(3)/2)]
blues_diamond = [(0.5, 0), (0.25, math.sqrt(3)/4)]
g = test_config("  Diamond", reds_diamond, blues_diamond)
if g:
    configs.append(("Diamond", reds_diamond, blues_diamond, g))

print("\n" + "="*70)
print("VALID CONFIGURATIONS FOUND:")
print("="*70)

if len(configs) == 0:
    print("✗ No configurations found matching g(1)=8, g(2)=28")
    print("\nExpanding search...")
else:
    for name, reds, blues, g in configs:
        print(f"\n{name}:")
        print(f"  Sequence: {g}")
        print(f"  Reds: {reds}")
        print(f"  Blues: {blues}")

        # Check if tractable to g(16)
        # Estimate growth rate
        if len(g) >= 3:
            r1 = g[2] / g[1] if g[1] > 0 else 0
            r2 = g[3] / g[2] if g[2] > 0 else 0
            avg_ratio = (r1 + r2) / 2
            est_g16 = g[2] * (avg_ratio ** 14)
            print(f"  Growth rate: ~{avg_ratio:.2f}x per day")
            print(f"  Estimated g(16): ~{est_g16:.0e}")

            if est_g16 < 1e6:
                print(f"  ✓ Tractable! Can compute to g(16)")
            else:
                print(f"  ✗ Too large, infeasible")

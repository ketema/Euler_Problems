#!/usr/bin/env python3
"""
Extreme zoom-out visualization of g(5) = 19,068 points
Shows TRUE geometric structure extending to ±17k units
"""

from sage.all import *
import matplotlib.pyplot as plt
import time

# Line intersection function
def line_intersection(p1, p2, p3, p4):
    d1 = p2 - p1
    d2 = p4 - p3
    det = d1[0]*(-d2[1]) - d1[1]*(-d2[0])
    if det == 0:
        return None
    rhs = p3 - p1
    t = (rhs[0]*(-d2[1]) - rhs[1]*(-d2[0])) / det
    return p1 + t*d1

# Compute next day
def compute_next_day(reds, blues):
    existing = set([tuple(r) for r in reds] + [tuple(b) for b in blues])
    lines = []
    for r in reds:
        for b in blues:
            lines.append((r, b))
    new_points = set()
    for i, (p1, p2) in enumerate(lines):
        for (p3, p4) in lines[i+1:]:
            pt = line_intersection(p1, p2, p3, p4)
            if pt is not None:
                pt_tuple = tuple(pt)
                if pt_tuple not in existing and pt_tuple not in new_points:
                    new_points.add(pt_tuple)
    return [vector(QQ, pt) for pt in new_points]

# Configuration
R1 = vector(QQ, [0, 0])
R2 = vector(QQ, [1, 0])
R3 = vector(QQ, [0, 1])
B1 = vector(QQ, [1, 1])
B2 = vector(QQ, [3, 9])

print("="*70)
print("COMPUTING g(5) FOR VISUALIZATION")
print("="*70)
print()

reds = [R1, R2, R3]
all_blues = [B1, B2]

for day in range(1, 6):
    start = time.time()
    new_blues = compute_next_day(reds, all_blues)
    all_blues = all_blues + new_blues
    elapsed = time.time() - start
    print(f"Day {day}: g({day}) = {len(all_blues):,} ({elapsed:.1f}s)")

print(f"\nTotal g(5) = {len(all_blues):,} points")
print()

# Extract coordinates
x_coords = [float(b[0]) for b in all_blues]
y_coords = [float(b[1]) for b in all_blues]

# Statistics
x_min, x_max = min(x_coords), max(x_coords)
y_min, y_max = min(y_coords), max(y_coords)

print("Bounding box:")
print(f"  x ∈ [{x_min:.2f}, {x_max:.2f}]")
print(f"  y ∈ [{y_min:.2f}, {y_max:.2f}]")
print()

# Create extreme zoom-out plot
print("Creating extreme zoom-out visualization...")
fig, ax = plt.subplots(figsize=(16, 16))

# Separate initial blues
initial_blues = [B1, B2]
generated_blues = [b for b in all_blues if b not in initial_blues]

# Plot generated blues (tiny points)
gen_x = [float(b[0]) for b in generated_blues]
gen_y = [float(b[1]) for b in generated_blues]
ax.scatter(gen_x, gen_y, c='blue', s=0.5, alpha=0.8,
           label=f'Generated blues ({len(generated_blues):,})', zorder=2)

# Plot initial blues
init_x = [float(b[0]) for b in initial_blues]
init_y = [float(b[1]) for b in initial_blues]
ax.scatter(init_x, init_y, c='cyan', s=100, marker='o',
           edgecolors='black', linewidths=3,
           label='Initial blues (B1, B2)', zorder=4)

# Plot red triangle
red_x = [float(R1[0]), float(R2[0]), float(R3[0])]
red_y = [float(R1[1]), float(R2[1]), float(R3[1])]
ax.scatter(red_x, red_y, c='red', s=300, marker='s',
           edgecolors='black', linewidths=4,
           label='Red triangle', zorder=5)

# Draw triangle edges
ax.plot([red_x[0], red_x[1]], [red_y[0], red_y[1]], 'r-', linewidth=3, alpha=0.6, zorder=3)
ax.plot([red_x[1], red_x[2]], [red_y[1], red_y[2]], 'r-', linewidth=3, alpha=0.6, zorder=3)
ax.plot([red_x[2], red_x[0]], [red_y[2], red_y[0]], 'r-', linewidth=3, alpha=0.6, zorder=3)

# Mark central core region
from matplotlib.patches import Rectangle
core_rect = Rectangle((-3, -3), 6, 8, fill=False, edgecolor='orange',
                      linewidth=4, linestyle='--',
                      label='Previous zoom region (±3 units)')
ax.add_patch(core_rect)

# Set extreme zoom out
ax.set_xlim(-18000, 1500)
ax.set_ylim(-18000, 2500)

ax.set_xlabel('x', fontsize=16, fontweight='bold')
ax.set_ylabel('y', fontsize=16, fontweight='bold')
ax.set_title(f'EXTREME ZOOM OUT: Full g(5) = {len(all_blues):,} Point Structure\n' +
             f'Bounding box: x ∈ [{x_min:.0f}, {x_max:.0f}], y ∈ [{y_min:.0f}, {y_max:.0f}]',
             fontsize=18, fontweight='bold')
ax.legend(fontsize=14, loc='upper right')
ax.grid(True, alpha=0.3, linestyle=':', linewidth=0.5)
ax.axhline(y=0, color='k', linewidth=1, alpha=0.4)
ax.axvline(x=0, color='k', linewidth=1, alpha=0.4)
ax.set_aspect('equal')

plt.tight_layout()
output_file = '/Users/ketema/projects/Euler_Problems/problem957/g5_extreme_zoomout.png'
plt.savefig(output_file, dpi=150, bbox_inches='tight')
print(f"\nSaved to: {output_file}")
print("="*70)

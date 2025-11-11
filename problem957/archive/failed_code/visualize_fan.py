"""
Visualize the 5-fan drawing structure
"""

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch
import numpy as np

def draw_fan_structure():
    """Draw the 5-fan structure for days 0 and 1"""

    fig, axes = plt.subplots(1, 2, figsize=(16, 8))

    # ========== DAY 0 ==========
    ax1 = axes[0]
    ax1.set_xlim(-1.5, 1.5)
    ax1.set_ylim(-1.5, 1.5)
    ax1.set_aspect('equal')
    ax1.axis('off')
    ax1.set_title('Day 0: g(0) = 2\n5 reds, 2 blues', fontsize=16, fontweight='bold')

    # Place 5 red vertices in a pentagon
    red_angles = np.linspace(0, 2*np.pi, 6)[:-1]  # 5 points
    red_positions = {}
    for i, angle in enumerate(red_angles):
        x = 1.0 * np.cos(angle)
        y = 1.0 * np.sin(angle)
        red_positions[i+1] = (x, y)

        # Draw red vertex
        circle = plt.Circle((x, y), 0.08, color='red', zorder=10)
        ax1.add_patch(circle)
        ax1.text(x*1.15, y*1.15, f'r{i+1}', fontsize=12, ha='center', va='center',
                fontweight='bold', color='darkred')

    # Place 2 blue vertices in center region
    blue_positions = {
        1: (-0.15, 0.2),
        2: (0.15, -0.2)
    }

    for b_id, (x, y) in blue_positions.items():
        # Draw blue vertex
        circle = plt.Circle((x, y), 0.08, color='blue', zorder=10)
        ax1.add_patch(circle)
        ax1.text(x, y, f'b{b_id}', fontsize=10, ha='center', va='center',
                fontweight='bold', color='white')

    # Draw all segments from reds to blues
    for r_id, (rx, ry) in red_positions.items():
        for b_id, (bx, by) in blue_positions.items():
            ax1.plot([rx, bx], [ry, by], 'gray', linewidth=1, alpha=0.4, zorder=1)

    # Add legend showing cyclic orders
    legend_text = "Cyclic orders around reds:\n"
    legend_text += "r₁: [1,2]\n"
    legend_text += "r₂: [2,1] ←reversed\n"
    legend_text += "r₃: [2,1] ←reversed\n"
    legend_text += "r₄: [2,1] ←reversed\n"
    legend_text += "r₅: [1,2]"

    ax1.text(-1.4, -1.3, legend_text, fontsize=9, family='monospace',
            bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))

    # Show crossing count
    ax1.text(0, -1.3, 'Disagreeing pairs: 6\n→ 6 crossings',
            fontsize=11, ha='center', fontweight='bold',
            bbox=dict(boxstyle='round', facecolor='lightgreen', alpha=0.7))

    # ========== DAY 1 ==========
    ax2 = axes[1]
    ax2.set_xlim(-1.5, 1.5)
    ax2.set_ylim(-1.5, 1.5)
    ax2.set_aspect('equal')
    ax2.axis('off')
    ax2.set_title('Day 1: g(1) = 8\n5 reds, 8 blues (6 new from crossings)',
                  fontsize=16, fontweight='bold')

    # Draw red vertices (same positions)
    for i, angle in enumerate(red_angles):
        x = 1.0 * np.cos(angle)
        y = 1.0 * np.sin(angle)

        circle = plt.Circle((x, y), 0.08, color='red', zorder=10)
        ax2.add_patch(circle)
        ax2.text(x*1.15, y*1.15, f'r{i+1}', fontsize=12, ha='center', va='center',
                fontweight='bold', color='darkred')

    # Original 2 blues
    for b_id, (x, y) in blue_positions.items():
        circle = plt.Circle((x, y), 0.06, color='blue', zorder=10)
        ax2.add_patch(circle)
        ax2.text(x, y-0.15, f'{b_id}', fontsize=8, ha='center', va='center',
                color='blue')

    # Add 6 new blues (scattered around center)
    new_blue_positions = [
        (0.35, 0.25),
        (-0.35, 0.25),
        (0.4, -0.1),
        (-0.4, -0.1),
        (0.0, 0.4),
        (0.0, -0.45),
    ]

    for i, (x, y) in enumerate(new_blue_positions):
        circle = plt.Circle((x, y), 0.05, color='lightblue', zorder=10,
                          edgecolor='blue', linewidth=1.5)
        ax2.add_patch(circle)
        ax2.text(x, y-0.12, f'{i+3}', fontsize=7, ha='center', va='center',
                color='darkblue')

    # Draw some segments to show complexity (not all, it would be too cluttered)
    # Just draw segments from r1 to show fan concept
    r1_pos = red_positions[1]
    for b_id, (bx, by) in blue_positions.items():
        ax2.plot([r1_pos[0], bx], [r1_pos[1], by], 'red', linewidth=1.5,
                alpha=0.6, zorder=2, label='r₁ fan' if b_id == 1 else '')
    for (bx, by) in new_blue_positions:
        ax2.plot([r1_pos[0], bx], [r1_pos[1], by], 'red', linewidth=1,
                alpha=0.4, zorder=2)

    # Draw segments from r2 to show fan concept
    r2_pos = red_positions[2]
    for b_id, (bx, by) in blue_positions.items():
        ax2.plot([r2_pos[0], bx], [r2_pos[1], by], 'orange', linewidth=1.5,
                alpha=0.6, zorder=2, label='r₂ fan' if b_id == 1 else '')
    for (bx, by) in new_blue_positions[:3]:  # Just first 3 new ones
        ax2.plot([r2_pos[0], bx], [r2_pos[1], by], 'orange', linewidth=1,
                alpha=0.4, zorder=2)

    # Add note about crossings
    ax2.text(0, -1.3, 'New blues created at\nsegment crossings',
            fontsize=11, ha='center', fontweight='bold',
            bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.7))

    # Add legend
    ax2.legend(loc='upper left', fontsize=9, framealpha=0.9)

    plt.tight_layout()
    plt.savefig('/home/user/Euler_Problems/problem957/fan_visualization.png',
                dpi=150, bbox_inches='tight')
    print("✓ Saved fan_visualization.png")

    # Create a second figure showing the cyclic order concept
    fig2, ax = plt.subplots(1, 1, figsize=(12, 10))
    ax.set_xlim(-2, 10)
    ax.set_ylim(-1, 9)
    ax.axis('off')
    ax.set_title('Cyclic Order Concept: How Crossings are Counted',
                fontsize=18, fontweight='bold', pad=20)

    # Draw two circular diagrams showing cyclic order
    # Red 1
    center1 = (2, 5)
    radius = 1.2

    # Draw circle
    circle = plt.Circle(center1, radius, fill=False, edgecolor='red', linewidth=3)
    ax.add_patch(circle)
    ax.text(center1[0], center1[1], 'r₁', fontsize=16, ha='center', va='center',
           fontweight='bold', color='red')

    # Place blues around circle in order [1, 2]
    angles1 = [90, -30]  # degrees
    labels1 = ['b₁', 'b₂']
    positions1 = {}

    for angle, label in zip(angles1, labels1):
        rad = np.radians(angle)
        x = center1[0] + (radius + 0.5) * np.cos(rad)
        y = center1[1] + (radius + 0.5) * np.sin(rad)
        positions1[label] = (x, y)

        circle_b = plt.Circle((x, y), 0.15, color='blue', zorder=10)
        ax.add_patch(circle_b)
        ax.text(x, y, label[1], fontsize=10, ha='center', va='center',
               fontweight='bold', color='white')

        # Draw arrow showing order
        arrow_x = center1[0] + radius * 0.7 * np.cos(rad)
        arrow_y = center1[1] + radius * 0.7 * np.sin(rad)
        ax.plot([center1[0], arrow_x], [center1[1], arrow_y],
               'red', linewidth=2, alpha=0.5)

    # Draw arc showing order
    arc_angles = np.linspace(np.radians(angles1[0]),
                            np.radians(angles1[1]) + 2*np.pi, 50)
    arc_x = center1[0] + (radius + 0.3) * np.cos(arc_angles)
    arc_y = center1[1] + (radius + 0.3) * np.sin(arc_angles)
    ax.plot(arc_x, arc_y, 'red', linewidth=2, alpha=0.3, linestyle='--')
    ax.annotate('', xy=(arc_x[-5], arc_y[-5]), xytext=(arc_x[-15], arc_y[-15]),
               arrowprops=dict(arrowstyle='->', color='red', lw=2))

    ax.text(center1[0], center1[1] - radius - 0.8, 'Order: [1, 2]',
           fontsize=12, ha='center', fontweight='bold',
           bbox=dict(boxstyle='round', facecolor='white', edgecolor='red', linewidth=2))

    # Red 2
    center2 = (6, 5)

    # Draw circle
    circle = plt.Circle(center2, radius, fill=False, edgecolor='orange', linewidth=3)
    ax.add_patch(circle)
    ax.text(center2[0], center2[1], 'r₂', fontsize=16, ha='center', va='center',
           fontweight='bold', color='orange')

    # Place blues around circle in order [2, 1] (reversed!)
    angles2 = [90, -30]  # same angular positions
    labels2 = ['b₂', 'b₁']  # but reversed labels!
    positions2 = {}

    for angle, label in zip(angles2, labels2):
        rad = np.radians(angle)
        x = center2[0] + (radius + 0.5) * np.cos(rad)
        y = center2[1] + (radius + 0.5) * np.sin(rad)
        positions2[label] = (x, y)

        circle_b = plt.Circle((x, y), 0.15, color='blue', zorder=10)
        ax.add_patch(circle_b)
        ax.text(x, y, label[1], fontsize=10, ha='center', va='center',
               fontweight='bold', color='white')

        # Draw arrow showing order
        arrow_x = center2[0] + radius * 0.7 * np.cos(rad)
        arrow_y = center2[1] + radius * 0.7 * np.sin(rad)
        ax.plot([center2[0], arrow_x], [center2[1], arrow_y],
               'orange', linewidth=2, alpha=0.5)

    # Draw arc showing order
    arc_angles = np.linspace(np.radians(angles2[0]),
                            np.radians(angles2[1]) + 2*np.pi, 50)
    arc_x = center2[0] + (radius + 0.3) * np.cos(arc_angles)
    arc_y = center2[1] + (radius + 0.3) * np.sin(arc_angles)
    ax.plot(arc_x, arc_y, 'orange', linewidth=2, alpha=0.3, linestyle='--')
    ax.annotate('', xy=(arc_x[-5], arc_y[-5]), xytext=(arc_x[-15], arc_y[-15]),
               arrowprops=dict(arrowstyle='->', color='orange', lw=2))

    ax.text(center2[0], center2[1] - radius - 0.8, 'Order: [2, 1]',
           fontsize=12, ha='center', fontweight='bold',
           bbox=dict(boxstyle='round', facecolor='white', edgecolor='orange', linewidth=2))

    # Draw segment comparison below
    y_base = 2.5

    # Explanation box
    explanation = """Interleaving Test:
• At r₁: b₁ comes before b₂  [1 < 2]
• At r₂: b₂ comes before b₁  [2 < 1]
• Orders DISAGREE → Segments (r₁,b₁) and (r₂,b₂) CROSS!
• Also: (r₁,b₂) and (r₂,b₁) CROSS!

Result: 1 crossing for the pair (b₁, b₂)"""

    ax.text(4, y_base - 0.5, explanation, fontsize=11, ha='center', va='top',
           family='monospace',
           bbox=dict(boxstyle='round', facecolor='lightyellow',
                    edgecolor='black', linewidth=2, pad=0.7))

    # Bottom: show the actual crossings
    ax.text(4, 0.3, 'Segment Crossing Diagram', fontsize=14, ha='center',
           fontweight='bold')

    # Simplified crossing diagram
    # Draw two segments that cross
    ax.plot([1, 7], [0, 0], 'gray', linewidth=2, alpha=0.3)  # baseline
    ax.plot([1.5, 5.5], [0.1, -0.1], 'red', linewidth=3, alpha=0.7, label='(r₁, b₁)')
    ax.plot([2.5, 6.5], [-0.1, 0.1], 'orange', linewidth=3, alpha=0.7, label='(r₂, b₂)')

    # Mark crossing point
    cross_x = 4
    cross_y = 0
    ax.plot(cross_x, cross_y, 'go', markersize=15, zorder=20)
    ax.text(cross_x + 0.3, cross_y + 0.3, 'NEW\nBLUE!', fontsize=10,
           fontweight='bold', color='green',
           bbox=dict(boxstyle='round', facecolor='lightgreen'))

    ax.legend(loc='lower right', fontsize=11, framealpha=0.9)

    plt.tight_layout()
    plt.savefig('/home/user/Euler_Problems/problem957/cyclic_order_explanation.png',
                dpi=150, bbox_inches='tight')
    print("✓ Saved cyclic_order_explanation.png")

if __name__ == '__main__':
    draw_fan_structure()
    print("\n✓ Visualizations complete!")

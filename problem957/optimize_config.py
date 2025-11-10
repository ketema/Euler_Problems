#!/usr/bin/env python3
"""
Use numerical optimization to find configuration giving g(1)=8, g(2)=28.
"""

import numpy as np
from scipy.optimize import differential_evolution


def simulate_float(reds_flat, blues_flat, max_days=2):
    """Simulate using floating point."""
    eps = 1e-9

    reds = [(reds_flat[i], reds_flat[i+1]) for i in range(0, 6, 2)]
    blues = set([(blues_flat[i], blues_flat[i+1]) for i in range(0, 4, 2)])

    def intersect(p1, p2, p3, p4):
        x1, y1 = p1
        x2, y2 = p2
        x3, y3 = p3
        x4, y4 = p4

        denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        if abs(denom) < eps:
            return None

        t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
        return (x1 + t * (x2 - x1), y1 + t * (y2 - y1))

    results = [len(blues)]

    for day in range(max_days):
        pairs = [(r, b) for r in reds for b in blues]
        new_blues = set(blues)
        
        for i in range(len(pairs)):
            for j in range(i+1, len(pairs)):
                pt = intersect(pairs[i][0], pairs[i][1], pairs[j][0], pairs[j][1])
                if pt:
                    new_blues.add((round(pt[0], 6), round(pt[1], 6)))

        blues = new_blues
        results.append(len(blues))

    return results


def objective(x):
    try:
        seq = simulate_float(x[:6], x[6:], max_days=2)
        return abs(seq[1] - 8) + abs(seq[2] - 28) if len(seq) >= 3 else 1e10
    except:
        return 1e10


print("Optimizing for g(1)=8, g(2)=28...")

result = differential_evolution(
    objective,
    [(-10, 10)] * 10,
    maxiter=500,
    popsize=20,
    seed=42
)

print(f"\nBest error: {result.fun}")
seq = simulate_float(result.x[:6], result.x[6:], max_days=5)
print(f"Sequence: {seq}")

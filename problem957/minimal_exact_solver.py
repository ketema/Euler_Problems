#!/usr/bin/env python3
"""
Minimal exact solver - carefully count intersections

The key insight: In "general position", we should get exactly 6 new blues on day 1.
If we get fewer, it's because of accidental concurrencies.

Strategy: Try MANY random configurations and find one that gives g(1)=8

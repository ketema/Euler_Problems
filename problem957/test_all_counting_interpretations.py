#!/usr/bin/env python3
"""
Test DIFFERENT ways to count/interpret g(n).

Possibilities:
1. g(n) = total blues (including initial 2)
2. g(n) = NEW blues generated on day n only
3. g(n) = number of intersection points (not blues)
4. g(n) = number of lines
5. g(n) = number of distinct points (reds + blues)
6. Something else entirely

CRITICAL: We know g(1)=8 and g(2)=28 from problem.
Test which interpretation gives these values.

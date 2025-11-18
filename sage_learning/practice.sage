# Lesson 2.3 Subsets (Power Set)

S = Subsets([1, 2, 3])
print("All subsets:")
for subset in S:
    print(subset)
print("Total:", S.cardinality())

# Filter: only subsets of size 2

S2 = Subsets([1, 2, 3, 4], 2)
print("\nSubsets of size 2:")
print(list(S2))

# Expected: 2^3 = 8 subsets (includes empty set), then 6 size-2 subsets.

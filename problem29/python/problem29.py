def solve(a_min, a_max):
    distinct_terms = set()
    for a in range(a_min, a_max + 1):
        for b in range(a_min, a_max + 1):
            distinct_terms.add(a**b)
    return len(distinct_terms)

if __name__ == '__main__':
    print(solve(2, 100))

#!/usr/bin/env python3
"""Problem 23: Non-Abundant Sums - Dense implementation"""

def sum_divisors(n):
    return sum(d for i in range(1, int(n**0.5)+1) if n%i==0 for d in ([i,n//i] if i*i!=n else [i]))-n if n>1 else 0

def abundant_numbers(limit):
    return [n for n in range(12, limit+1) if sum_divisors(n)>n]

def solve(limit=28123):
    a=abundant_numbers(limit);s=set()
    for i in a:
        for j in a:
            if i+j>limit:break
            s.add(i+j)
    return sum(n for n in range(1,limit+1) if n not in s)

if __name__ == "__main__":
    print(f"Answer: {solve()}")

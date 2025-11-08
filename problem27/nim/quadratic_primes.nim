import math, sets

proc sieve(n: int): HashSet[int] =
  var isPrime = newSeq[bool](n + 1)
  for i in 2..n: isPrime[i] = true
  for i in 2..int(sqrt(float(n))):
    if isPrime[i]:
      for j in countup(i*i, n, i): isPrime[j] = false
  for i in 2..n:
    if isPrime[i]: result.incl(i)

let primes = sieve(100000)
proc isPrime(n: int): bool = n > 1 and n in primes

proc countConsecutive(a, b: int): int =
  var n = 0
  while isPrime(n*n + a*n + b): inc n
  return n

var maxN, maxA, maxB = 0
for b in sieve(1000):
  for a in -999..999:
    let n = countConsecutive(a, b)
    if n > maxN: (maxN, maxA, maxB) = (n, a, b)

echo "Answer: ", maxA * maxB
echo "Coefficients: a=", maxA, " b=", maxB, " (", maxN, " primes)"

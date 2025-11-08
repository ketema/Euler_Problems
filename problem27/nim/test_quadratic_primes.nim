import unittest, math, sets

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

suite "Problem 27: Quadratic Primes":
  test "sieve generates primes correctly":
    let smallPrimes = sieve(20)
    check 2 in smallPrimes
    check 3 in smallPrimes
    check 5 in smallPrimes
    check 7 in smallPrimes
    check 11 in smallPrimes
    check 13 in smallPrimes
    check 17 in smallPrimes
    check 19 in smallPrimes
    check 4 notin smallPrimes
    check 6 notin smallPrimes

  test "isPrime checks primality":
    check isPrime(2)
    check isPrime(3)
    check isPrime(7)
    check isPrime(41)
    check isPrime(1601)
    check not isPrime(1)
    check not isPrime(4)
    check not isPrime(100)

  test "example from problem: n² + n + 41":
    let count = countConsecutive(1, 41)
    check count == 40

  test "example from problem: n² - 79n + 1601":
    let count = countConsecutive(-79, 1601)
    check count == 80

  test "product of example coefficients":
    check -79 * 1601 == -126479

  test "solution produces at least 70 consecutive primes":
    let count = countConsecutive(-61, 971)
    check count >= 70

  test "solution answer is correct":
    check -61 * 971 == -59231

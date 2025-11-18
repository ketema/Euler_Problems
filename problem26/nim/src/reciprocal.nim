import tables

proc remainderSequence*(d: int): seq[int] =
  ## Generate sequence of remainders from long division of 1/d
  ## Returns empty sequence if d <= 0
  ## Collects remainders until cycle detected or termination
  ## FIXED: Add remainder BEFORE computing next (critical for cycle detection)
  if d <= 0:
    return @[]

  result = @[]
  var remainder = 1
  var seen = initTable[int, bool]()

  # Maximum cycle length is d-1, so limit iterations to d
  for i in 0..<d:
    result.add(remainder)        # Add FIRST (critical fix)
    if seen.hasKey(remainder):   # If we've seen it before, cycle completes
      break
    seen[remainder] = true
    remainder = (remainder * 10) mod d
    if remainder == 0:           # Check termination
      break

  # Result contains the remainder sequence with cycle-closing remainder (if cycle exists)

proc detectCycle*(remainders: seq[int]): int =
  ## Detect cycle length in remainder sequence
  ## Returns 0 if no cycle found
  var positions: Table[int, int] = initTable[int, int]()

  for position, remainder in remainders:
    if positions.hasKey(remainder):
      return position - positions[remainder]
    positions[remainder] = position

  return 0

proc findCycleLength*(d: int): int =
  ## Find the length of recurring cycle in decimal expansion of 1/d
  ## Returns 0 for invalid input (d <= 0) or terminating decimals
  ## Composes remainderSequence + detectCycle for modular design
  if d <= 0:
    return 0

  let remainders = remainderSequence(d)
  return detectCycle(remainders)

proc isPureFactorOf2And5(n: int): bool =
  ## Returns true if n = 2^a × 5^b (pure product, terminating decimal)
  ## Numbers like 6 (2×3) or 15 (3×5) have cycles and should NOT be skipped
  var temp = n
  while temp mod 2 == 0:
    temp = temp div 2
  while temp mod 5 == 0:
    temp = temp div 5
  return temp == 1  # Only factors were 2 and 5

proc findLongestCycle*(limit: int): (int, int) =
  ## Find d < limit with longest cycle in 1/d
  ## Returns (d, cycle_length)
  ## Mathematical optimization: Skip PURE products of 2 and 5 (terminating decimals)
  var maxD = 0
  var maxLength = 0

  for d in 2..<limit:
    # Mathematical property: 1/d terminates iff d = 2^a × 5^b (pure product)
    # FIXED: Only skip pure powers, not all multiples (e.g., 6=2×3 has cycles)
    if isPureFactorOf2And5(d):
      continue

    let length = findCycleLength(d)
    if length > maxLength:
      maxD = d
      maxLength = length

  result = (maxD, maxLength)

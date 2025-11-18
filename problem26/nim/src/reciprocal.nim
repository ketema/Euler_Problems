import tables

proc remainderSequence*(d: int): seq[int] =
  ## Generate sequence of remainders from long division of 1/d
  ## Returns empty sequence if d <= 0
  ## Collects remainders up to one full cycle (limited to d iterations)
  if d <= 0:
    return @[]

  result = @[]
  var remainder = 1

  # Maximum cycle length is d-1, so limit iterations to d
  for i in 0..<d:
    if remainder == 0:
      # Decimal terminates
      break
    result.add(remainder)
    remainder = (remainder * 10) mod d

  # Result contains the remainder sequence
  # If sequence ends with remainder==0, no cycle (terminates)
  # Otherwise, sequence contains one complete cycle

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

proc findLongestCycle*(limit: int): (int, int) =
  ## Find d < limit with longest cycle in 1/d
  ## Returns (d, cycle_length)
  var maxD = 0
  var maxLength = 0

  for d in 2..<limit:
    let length = findCycleLength(d)
    if length > maxLength:
      maxD = d
      maxLength = length

  result = (maxD, maxLength)

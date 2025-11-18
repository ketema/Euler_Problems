import tables

proc remainderSequence*(d: int): seq[int] =
  ## Generate sequence of remainders from long division of 1/d
  ## Returns empty sequence if d <= 0
  ## Note: For cycle detection, use findCycleLength directly (more efficient)
  if d <= 0:
    return @[]

  result = @[]
  var remainder = 1
  var positions = initTable[int, int]()

  while remainder != 0:
    if positions.hasKey(remainder):
      # Cycle found, stop here
      break
    positions[remainder] = result.len
    result.add(remainder)
    remainder = (remainder * 10) mod d

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
  if d <= 0:
    return 0

  var remainder = 1
  var positions = initTable[int, int]()
  var position = 0

  while remainder != 0:
    if positions.hasKey(remainder):
      return position - positions[remainder]
    positions[remainder] = position
    remainder = (remainder * 10) mod d
    position += 1

  return 0  # Terminates

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

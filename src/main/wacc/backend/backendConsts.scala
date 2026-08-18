package backend

/* 
backendConsts:
    - contains backend constants used thorughout backend
    - avoid magic numbers
*/

private val ByteSize  = 1
private val DwordSize = 4
private val WordSize = 8
private val PairSize = 2 * WordSize

private val trueBool = 1
private val falseBool = 0
private val zeroOffset = 0
private val incrementOffset = 1
private val zeroCount = 0
private val incrementCount = 1
private val nullVal = 0
private val oneVal = 1
private val zeroBytes = 0

private val AsciiUpperBound = 128
private val ErrExitCode = 255
private val AlignMask = -16
private val defaultTypeOffset = 4

private val renamedArrVal = -1
private val renamedIdxVal = -2
private val renamedLenVal = -3

private val printCount = 3
private val declareCount = 1
private val defaultCount = 0


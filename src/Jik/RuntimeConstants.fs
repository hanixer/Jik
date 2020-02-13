module RuntimeConstants

let wordSize = 8

let fixnumTag = 0x00
let fixnumMask = 0x03
let fixnumShift = 2

let pairTag = 0x01
let pairMask = 0x07

let flonumTag = 0x02
let flonumMask = 0x07

let symbolTag = 0x03
let symbolMask = 0x07

let closureTag = 0x05
let closureMask = 0x07

let typedObjectTag = 0x07
let typedObjectMask = 0x07

let boolTag = 0x06
let boolMask = 0xF7
let falseLiteral = 0x06
let trueLiteral = 0x0E
let boolBit = 3

let charTag = 0x16
let charMask = 0xFF
let charShift = 8

let unboundLiteral = 0x1E
let unboundMask = 0xFF

let nilLiteral = 0x26
let nilMask = 0xFF

let forwardMarker = 0x2E

let eofLiteral = 0x36
let eofMask = 0xFF

let voidTag = 0x3E
let voidMask = 0xFF

let vectorTag = 0x00
let vectorMask = 0x03
let vectorSizeShift = 32

let stringTag = 0x02
let stringMask = 0x07
let stringSizeShift = 8

let freePointer = "freePointer"
let errorHandlerLabel = ".L_errorHandler"
let wrongArgCountHandler = ".L_wrongArgCountHandler"
let procErrorHandler = ".L_procErrorHandler"
let globVarErrorHandler = ".L_globVarErrorHandler"
let globVarNameTable = "globVarNameTable"
let globRootsTable = "globRootsTable"
let rootStackBegin = "rootStackBegin"
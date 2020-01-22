module RuntimeConstants

let wordSize = 8
let fixnumShift = 2
let fixnumMask = 0x03
let fixnumTag = 0x00
let falseLiteral = 0x2F
let trueLiteral = 0x6F
let boolBit = 6
let boolTag = 0x2F
let charShift = 8
let charTag = 0x0F
let charMask = 0xFF
let nilLiteral = 0x8F
let nilMask = 0xFF

let pairTag = 0x01
let pairMask = 0x07
let pairSize = 2 * wordSize
let carOffset = 0
let cdrOffset = wordSize

let vectorTag = 0x05
let vectorMask = 0x07
let stringTag = 0x06
let stringMask = 0x07

let closureTag = 0x02
let closureMask = 0x07

let symbolTag = 0x03
let symbolMask = 0x07

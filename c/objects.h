#ifndef OBJECTS_H
#define OBJECTS_H

#include <stdint.h>

/// Keep synchronized with RuntimeConstants.fs

#define wordSize 8

#define fixnumTag 0x00
#define fixnumMask 0x03
#define fixnumShift 2

#define pairTag 0x01
#define pairMask 0x07

#define flonumTag 0x02
#define flonumMask 0x07

#define symbolTag 0x03
#define symbolMask 0x07

#define closureTag 0x05
#define closureMask 0x07
#define closureSizeShift 32

#define typedObjectTag 0x07
#define typedObjectMask 0x07

#define boolTag 0x06
#define boolMask 0xF7
#define falseLiteral 0x06
#define trueLiteral 0x0E
#define boolBit 3

#define charTag 0x16
#define charMask 0xFF
#define charShift 8

#define unboundLiteral 0x1E
#define unboundMask 0xFF

#define nilLiteral 0x26
#define nilMask 0xFF

#define forwardMarker 0x2E

#define eofLiteral 0x36
#define eofMask 0xFF

#define voidTag 0x3E
#define voidMask 0xFF

#define vectorTag 0x00
#define vectorMask 0x03
#define vectorSizeShift 32

#define stringTag 0x02
#define stringMask 0x07
#define stringSizeShift 8

typedef uint64_t ptr_t;
typedef ptr_t* ptrptr;

int fixnumToInt(ptr_t p);
ptr_t intToFixnum(int n);
ptrptr toPtrptr(ptr_t p);
ptr_t charToPtr(char c);
int isFixnum(ptr_t p);
int isFlonum(ptr_t p);
int isPair(ptr_t p);
int isTypedObject(ptr_t p);
int isVector(ptr_t p);
int isString(ptr_t p);
int isClosure(ptr_t p);
int isSymbol(ptr_t p);
int isEof(ptr_t p);
int isNil(ptr_t p);
ptr_t stringOfSymbol(ptr_t p);
ptr_t car(ptr_t p);
ptr_t cdr(ptr_t p);
int vectorSize(ptr_t p);
ptr_t vectorRef(ptr_t p, int i);
int stringSize(ptr_t p);
char* stringData(ptr_t p);
char stringRef(ptr_t p, int i);
double flonumData(ptr_t p);

#endif
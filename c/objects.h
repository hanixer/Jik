#ifndef OBJECTS_H
#define OBJECTS_H

#include <stdint.h>

#define wordSize 8
#define fixnumShift 0x02
#define fixnumMask 0x03
#define fixnumTag 0x00
#define trueLiteral 0x6F
#define falseLiteral 0x2F
#define nilLiteral 0x8F
#define charShift 0x08
#define charMask 0xFF
#define charTag 0x0F
#define pairTag 0x01
#define pairMask 0x07
#define pairSize (2 * wordSize)
#define vectorTag 0x05
#define vectorMask 0x07
#define stringTag 0x06
#define stringMask 0x07
#define closureTag 0x02
#define closureMask 0x07
#define eofTag 0x5F
#define eofMask 0xFF
#define carOffset 0
#define cdrOffset wordSize

typedef uint64_t ptr_t;
typedef ptr_t* ptrptr;



int fixnumToInt(ptr_t p);
ptr_t intToFixnum(int n);
ptrptr toPtrptr(ptr_t p);
ptr_t charToPtr(char c);
int isFixnum(ptr_t p);
int isPair(ptr_t p);
int isVector(ptr_t p);
int isString(ptr_t p);
int isClosure(ptr_t p);
int isEof(ptr_t p);
int isNil(ptr_t p);
ptr_t car(ptr_t p);
ptr_t cdr(ptr_t p);
int vectorSize(ptr_t p);
ptr_t vectorRef(ptr_t p, int i);
int stringSize(ptr_t p);
char* stringData(ptr_t p);
char stringRef(ptr_t p, int i);

#endif
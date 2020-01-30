#include "objects.h"

int fixnumToInt(ptr_t p) { return p >> fixnumShift; }
ptr_t intToFixnum(int n) { return n << fixnumShift; }
ptrptr toPtrptr(ptr_t p) { return (ptrptr) p; }
ptr_t charToPtr(char c) { return (((ptr_t) c << charShift) | charTag); }
int isFixnum(ptr_t p) {return (p & fixnumMask) == fixnumTag; }
int isPair(ptr_t p) { return (p & pairMask) == pairTag; }
int isVector(ptr_t p) { return (p & vectorMask) == vectorTag; }
int isString(ptr_t p) { return (p & stringMask) == stringTag; }
int isClosure(ptr_t p) { return (p & closureMask) == closureTag; }
int isEof(ptr_t p) { return (p & eofMask) == eofTag; }
int isNil(ptr_t p) { return p == nilLiteral; }
ptr_t car(ptr_t p) { return (ptr_t) *(toPtrptr(p - pairTag)); }
ptr_t cdr(ptr_t p) { return *(toPtrptr(p - pairTag + wordSize)); }
int vectorSize(ptr_t p) { return fixnumToInt(*(toPtrptr(p - vectorTag))); }
ptr_t vectorRef(ptr_t p, int i) { return *(toPtrptr(p - vectorTag + (i + 1) * wordSize)); }
int stringSize(ptr_t p) { return fixnumToInt(*(toPtrptr(p - stringTag))); }
char* stringData(ptr_t p) { return ((char*)(p - stringTag)) + wordSize; }
char stringRef(ptr_t p, int i) { return (*(toPtrptr(p - stringTag + wordSize + i))); }
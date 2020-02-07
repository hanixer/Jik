#include "objects.h"
#include <stdio.h>

int fixnumToInt(ptr_t p) { return p >> fixnumShift; }
ptr_t intToFixnum(int n) { return n << fixnumShift; }
ptrptr toPtrptr(ptr_t p) { return (ptrptr) p; }
ptr_t charToPtr(char c) { return (((ptr_t) c << charShift) | charTag); }
int isFixnum(ptr_t p) {return (p & fixnumMask) == fixnumTag; }
int isPair(ptr_t p) { return (p & pairMask) == pairTag; }
int isVector(ptr_t p) { return (p & vectorMask) == vectorTag; }
int isString(ptr_t p) { return (p & stringMask) == stringTag; }
int isClosure(ptr_t p) { return (p & closureMask) == closureTag; }
int isSymbol(ptr_t p) { return (p & symbolMask) == symbolTag; }
int isEof(ptr_t p) { return (p & eofMask) == eofTag; }
int isNil(ptr_t p) { return p == nilLiteral; }
int vectorSize(ptr_t p) { return fixnumToInt(*(toPtrptr(p - vectorTag))); }
ptr_t vectorRef(ptr_t p, int i) { return *(toPtrptr(p - vectorTag + (i + 1) * wordSize)); }
int stringSize(ptr_t p) { return fixnumToInt(*(toPtrptr(p - stringTag))); }
char* stringData(ptr_t p) { return ((char*)(p - stringTag)) + wordSize; }
char stringRef(ptr_t p, int i) { return (*(toPtrptr(p - stringTag + wordSize + i))); }

ptr_t stringOfSymbol(ptr_t p)
{
    ptr_t v = * (toPtrptr(p - symbolTag + wordSize));
    // printf("car: p = %p, val = %p\n", p, v);
    return v;
}

ptr_t car(ptr_t p)
{
    ptr_t v = * (toPtrptr(p - pairTag + wordSize));
    // printf("car: p = %p, val = %p\n", p, v);
    return v;
}

ptr_t cdr(ptr_t p)
{
    ptr_t v = * (toPtrptr(p - pairTag + 2 * wordSize));
    // printf("cdr: p = %p, val = %p\n", p, v);
    return v;
}
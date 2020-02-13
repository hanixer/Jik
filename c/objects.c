#include "objects.h"
#include <stdio.h>

ptrptr toPtrptr(ptr_t p) { return (ptrptr)p; }
ptr_t toPtr(ptrptr p) { return (ptr_t)p; }

static int isObjectOfType(ptr_t obj, int mask, int tag)
{
    int isTypedObject = (obj & typedObjectMask) == typedObjectTag;
    if (isTypedObject)
    {
        ptrptr pp = toPtrptr(obj - typedObjectTag);
        return (*pp & mask) == tag;
    }

    return 0;
}

int fixnumToInt(ptr_t p) { return p >> fixnumShift; }
ptr_t intToFixnum(int n) { return n << fixnumShift; }
ptr_t charToPtr(char c) { return (((ptr_t)c << charShift) | charTag); }
int isFixnum(ptr_t p) { return (p & fixnumMask) == fixnumTag; }
int isFlonum(ptr_t p) { return (p & flonumMask) == flonumTag; }
int isPair(ptr_t p) { return (p & pairMask) == pairTag; }
int isTypedObject(ptr_t p) { return (p & typedObjectMask) == typedObjectTag; }
int isVector(ptr_t p) { return isObjectOfType(p, vectorMask, vectorTag); }
int isString(ptr_t p) { return isObjectOfType(p, stringMask, stringTag); }
int isClosure(ptr_t p) { return (p & closureMask) == closureTag; }
int isSymbol(ptr_t p) { return (p & symbolMask) == symbolTag; }
int isEof(ptr_t p) { return p == eofLiteral; }
int isNil(ptr_t p) { return p == nilLiteral; }

int vectorSize(ptr_t p) { return fixnumToInt(*(toPtrptr(p - typedObjectTag))); }
ptr_t vectorRef(ptr_t p, int i) { return *(toPtrptr(p - typedObjectTag + (i + 1) * wordSize)); }

int stringSize(ptr_t p) { return *(toPtrptr(p - typedObjectTag)) >> stringSizeShift; }
char *stringData(ptr_t p) { return ((char *)(p - typedObjectTag)) + wordSize; }
char stringRef(ptr_t p, int i) { return *(toPtrptr(p - typedObjectTag + wordSize + i)); }

ptr_t stringOfSymbol(ptr_t p)
{
    ptr_t v = *(toPtrptr(p - symbolTag + wordSize));
    // printf("car: p = %p, val = %p\n", p, v);
    return v;
}

ptr_t car(ptr_t p)
{
    ptr_t v = *(toPtrptr(p - pairTag + wordSize));
    // printf("car: p = %p, val = %p\n", p, v);
    return v;
}

ptr_t cdr(ptr_t p)
{
    ptr_t v = *(toPtrptr(p - pairTag + 2 * wordSize));
    // printf("cdr: p = %p, val = %p\n", p, v);
    return v;
}

double flonumData(ptr_t p)
{
    return *((double*)(p - flonumTag + wordSize));
}
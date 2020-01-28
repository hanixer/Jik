#include "gc.h"

int64_t* freePointer;

/// Points to the beginning of FromSpace.
int64_t* fromSpaceBegin;

/// Points to one memory location past the FromSpace.
int64_t* fromSpaceEnd;

void collect() {

}
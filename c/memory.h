#include <stdint.h>
#include "objects.h"

/// Copying garbage collector.
/// There are two spaces: FromSpace and ToSpace.
/// Memory is allocated in FromSpace.
/// When FromSpace is filled, garbage collection is invoked.
/// All reachable objects are copied from FromSpace to ToSpace.
/// After that FromSpace and ToSpace switch their roles.

/// A pointer to the next free memory location in the FromSpace.
extern uint64_t* freePointer;

/// Points to the beginning of FromSpace.
extern uint64_t* fromSpaceBegin;

/// Points to one memory location past the FromSpace.
extern uint64_t* fromSpaceEnd;

/// Begin of the root stack.
extern uint64_t* rootStackBegin;

void gcInitialize(uint64_t heapSize, uint64_t rootStackSize);

void collect(uint64_t* rootStack, int64_t size);

/// Should be called from C.
void *allocateC(uint64_t size);

char* allocateProtectedSpace(int size);

void deallocateProtectedSpace(char* ptr, int size);
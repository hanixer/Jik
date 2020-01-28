#include <stdint.h>

/// Copying garbage collector.
/// There are two spaces: FromSpace and ToSpace.
/// Memory is allocated in FromSpace.
/// When FromSpace is filled, garbage collection is invoked.
/// All reachable objects are copied from FromSpace to ToSpace.
/// After that FromSpace and ToSpace switch their roles.

/// A pointer to the next free memory location in the FromSpace.
extern int64_t* freePointer;

/// Points to the beginning of FromSpace.
extern int64_t* fromSpaceBegin;

/// Points to one memory location past the FromSpace.
extern int64_t* fromSpaceEnd;

void gcInitialize(uint64_t heapSize);

void collect(int64_t** stack, int64_t size);

char* allocateProtectedSpace(int size);

void deallocateProtectedSpace(char* ptr, int size);
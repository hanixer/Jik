#include "memory.h"
#include <windows.h>
#include <stdio.h>

extern int64_t* globRootsTable;

int64_t* freePointer;

/// Points to the beginning of FromSpace.
int64_t* fromSpaceBegin;

/// Points to one memory location past the FromSpace.
int64_t* fromSpaceEnd;

/// Points to the beginning of ToSpace.
static int64_t* toSpaceBegin;

/// Points to one memory location past the ToSpace.
static int64_t* toSpaceEnd;

char* allocateProtectedSpace(int size) {
	SYSTEM_INFO si;
	GetSystemInfo(&si);
	int pageSize = si.dwPageSize;
	int aligned = (size + pageSize - 1) / pageSize * pageSize;
	int total = aligned + pageSize * 2;
	char* pBase = (char*) VirtualAlloc(0, total, MEM_RESERVE, PAGE_NOACCESS);
	char* pSpace = (char*) VirtualAlloc(pBase + pageSize, aligned, MEM_COMMIT, PAGE_READWRITE);
	return pSpace;
}

void deallocateProtectedSpace(char* ptr, int size) {
	SYSTEM_INFO si;
	GetSystemInfo(&si);
	int pageSize = si.dwPageSize;
	int aligned = (size + pageSize - 1) / pageSize * pageSize;
	int total = aligned + pageSize * 2;
	VirtualFree(ptr - aligned, total, MEM_RELEASE);
}

void gcInitialize(uint64_t heapSize) {
    char* heap = allocateProtectedSpace(heapSize);
    freePointer = (int64_t*) heap;
    fromSpaceBegin = freePointer;
    fromSpaceEnd = (int64_t*) (heap + heapSize);

	heap = allocateProtectedSpace(heapSize);
    toSpaceBegin = (int64_t*) heap;
	toSpaceEnd = (int64_t*) (heap + heapSize);
}

void collect(int64_t** stack, int64_t bytesNeeded) {
    printf("  --- we are in collect\n");
    printf("  --- freePointer		= %p\n", freePointer);
    printf("  --- fromSpaceBegin 	= %p\n", fromSpaceBegin);
    printf("  --- fromSpaceEnd 		= %p\n", fromSpaceEnd);
    printf("  --- stack 			= %p\n", stack);
    printf("  --- size 				= %d\n", bytesNeeded);
}
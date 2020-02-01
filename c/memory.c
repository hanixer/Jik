#include "memory.h"
#include <windows.h>
#include <stdio.h>

extern ptr_t **globRootsTable;

uint64_t *freePointer;

/// Points to the beginning of FromSpace.
uint64_t *fromSpaceBegin;

/// Points to one memory location past the FromSpace.
uint64_t *fromSpaceEnd;

/// Points to the beginning of ToSpace.
static uint64_t *toSpaceBegin;

/// Points to one memory location past the ToSpace.
static uint64_t *toSpaceEnd;

/// Pointers used in copying of live data.
static uint64_t *copyPtrBegin;
static uint64_t *copyPtrEnd;

uint64_t *rootStackBegin;

char *allocateProtectedSpace(int size)
{
	SYSTEM_INFO si;
	GetSystemInfo(&si);
	int pageSize = si.dwPageSize;
	int aligned = (size + pageSize - 1) / pageSize * pageSize;
	int total = aligned + pageSize * 2;
	char *pBase = (char *)VirtualAlloc(0, total, MEM_RESERVE, PAGE_NOACCESS);
	char *pSpace = (char *)VirtualAlloc(pBase + pageSize, aligned, MEM_COMMIT, PAGE_READWRITE);
	return pSpace;
}

void deallocateProtectedSpace(char *ptr, int size)
{
	SYSTEM_INFO si;
	GetSystemInfo(&si);
	int pageSize = si.dwPageSize;
	int aligned = (size + pageSize - 1) / pageSize * pageSize;
	int total = aligned + pageSize * 2;
	VirtualFree(ptr - aligned, total, MEM_RELEASE);
}

void gcInitialize(uint64_t heapSize, uint64_t rootStackSize)
{
	char *pointer = allocateProtectedSpace(heapSize);
	freePointer = (uint64_t *)pointer;
	fromSpaceBegin = freePointer;
	fromSpaceEnd = (uint64_t *)(pointer + heapSize);

	pointer = allocateProtectedSpace(heapSize);
	toSpaceBegin = (uint64_t *)pointer;
	toSpaceEnd = (uint64_t *)(pointer + heapSize);

	pointer = allocateProtectedSpace(rootStackSize);
	rootStackBegin = (uint64_t *)pointer;
	rootStackBegin[0] = 0;
}

void copyVectorOrClosure(ptr_t *p, uint64_t tag)
{
	ptr_t pHeap = (*p - tag);
	ptr_t *pFrom = (ptr_t *)pHeap;
	uint64_t firstCell = *pFrom; // Value in the first cell of the object.
	if (firstCell & 1)
	{
		// forward pointer.
		*p = (firstCell - 1) + tag;
		printf("Already copied to ToSpace. New address: %p\n", *p);
	}
	else
	{
		ptr_t size = (firstCell >> fixnumShift) + 1;
		ptr_t pTo = (ptr_t)copyPtrEnd;
		memcpy(copyPtrEnd, pFrom, size * wordSize);
		*pFrom = pTo | 1; // Add forward bit.
		copyPtrEnd += size;
		*p = pTo | tag;
		printf("Copy object to ToSpace.\n");
		printf("size = %d, new address = %p\n", size, pTo);
	}
}

/// Cheney copying algorithm.
void copyData(ptr_t *p)
{
	printf("copyData: %p\n", *p);
	if (isVector(*p))
	{
		printf("vector\n");
		copyVectorOrClosure(p, vectorTag);
	}
	else if (isClosure(*p))
	{
		printf("closure\n");
		copyVectorOrClosure(p, closureTag);
	}

	// if (isPair(p) || isVector(p) || isString(p) || isClosure(p)) {

	// }
}

void hexDump(const char *desc, const void *addr, const int len);

void collect(uint64_t *rootStack, int64_t bytesNeeded)
{
	if (rootStack < rootStackBegin)
	{
		printf("Error! rootStack (0x%p) is less than rootStackBegin (0x%p)", rootStack, rootStackBegin);
		exit(1);
	}

	printf("--- we are in collect\n");
	printf("--- freePointer    = 0x%p\n", freePointer);
	printf("--- fromSpaceBegin = 0x%p\n", fromSpaceBegin);
	printf("--- fromSpaceEnd   = 0x%p\n", fromSpaceEnd);
	printf("--- stack          = 0x%p\n", rootStack);
	printf("--- size           = %d\n", bytesNeeded);
	hexDump("root stack", rootStackBegin, (rootStack - rootStackBegin) * wordSize);
	hexDump("from space", fromSpaceBegin, (fromSpaceEnd - fromSpaceBegin) * wordSize);
	hexDump("to space", toSpaceBegin, (toSpaceEnd - toSpaceBegin) * wordSize);

	copyPtrBegin = toSpaceBegin;
	copyPtrEnd = toSpaceBegin;

	// Copy root stack data.
	for (uint64_t *curr = rootStack; curr != rootStackBegin; curr--)
	{
		copyData(curr);
		// hexDump("from space after copy", fromSpaceBegin, (fromSpaceEnd - fromSpaceBegin) * wordSize);
	}

	// Copy data pointed by globals.
	for (int i = 0; globRootsTable[i] != 0; i++)
	{
		copyData(globRootsTable[i]);
	}

	while (copyPtrBegin < copyPtrEnd)
	{
		copyData(copyPtrBegin);
		copyPtrBegin++;
	}

	uint64_t *pBegin = fromSpaceBegin;
	uint64_t *pEnd = fromSpaceEnd;

	fromSpaceBegin = toSpaceBegin;
	fromSpaceEnd = toSpaceEnd;
	toSpaceBegin = pBegin;
	toSpaceEnd = pEnd;

	memset(toSpaceBegin, 0, (toSpaceEnd - toSpaceBegin) * wordSize);

	hexDump("from space", fromSpaceBegin, (fromSpaceEnd - fromSpaceBegin) * wordSize);
	hexDump("to space", toSpaceBegin, (toSpaceEnd - toSpaceBegin) * wordSize);
}

void hexDump(const char *desc, const void *addr, const int len)
{
	if (desc)
	{
		printf("%s:\n", desc);
	}

	const unsigned char *pch = (const unsigned char *)addr;

	// Process by 16 bytes.
	for (size_t i = 0; i < len; i += 16)
	{
		// Print address.
		printf("0x%p   ", pch + i);

		// Print each byte.
		for (size_t j = 0; j < 16; j++)
		{
			if (j == 8) printf(" ");

			printf("%02x ", pch[i + j]);
		}

		printf("  ");

		// Print quads.
		for (size_t j = 0; j < 2; j++)
		{
			const uint64_t* pu64 = (const uint64_t*)(pch + i);
			const uint64_t* pu642 = pu64 + 1;
			uint64_t va = pu64[j];
			printf("0x%016x ", pu64[j]);
		}

		// New line.
		printf("\n");
	}
}


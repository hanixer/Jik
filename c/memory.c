#include "memory.h"
#include <windows.h>
#include <stdio.h>
#include <assert.h>

#define DEBUG_LOG_GC
#define DEBUG_FORCE_GC

extern ptr_t **globRootsTable;

uint64_t *freePointer;

/// Points to the beginning of FromSpace.
uint64_t *fromSpaceBegin;

/// Points to one memory location past the FromSpace.
uint64_t *fromSpaceEnd;
static uint8_t *fromBitmap;

/// Points to the beginning of ToSpace.
static uint64_t *toSpaceBegin;

/// Points to one memory location past the ToSpace.
static uint64_t *toSpaceEnd;
static uint8_t *toBitmap;

/// Size of fromBitmap and toBitmap.
static uint64_t bitmapSize;

/// Pointers used in copying of live data.
static uint64_t *copyPtrBegin;
static uint64_t *copyPtrEnd;

uint64_t *rootStackBegin;

void hexDump(const char *desc, const void *addr, const int len);

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
	bitmapSize = (heapSize + (wordSize - 1)) / wordSize;

	char *pointer = allocateProtectedSpace(heapSize);
	freePointer = (uint64_t *)pointer;
	fromSpaceBegin = freePointer;
	fromSpaceEnd = (uint64_t *)(pointer + heapSize);
	fromBitmap = allocateProtectedSpace(bitmapSize);
	memset(fromBitmap, 0, bitmapSize);

	pointer = allocateProtectedSpace(heapSize);
	toSpaceBegin = (uint64_t *)pointer;
	toSpaceEnd = (uint64_t *)(pointer + heapSize);
	toBitmap = allocateProtectedSpace(bitmapSize);
	memset(toBitmap, 0, bitmapSize);

	pointer = allocateProtectedSpace(rootStackSize);
	rootStackBegin = (uint64_t *)pointer;
	rootStackBegin[0] = 0;
}

void markInBitmap(uint8_t *bitmap, uint64_t *spaceBegin, uint64_t *address)
{
	assert(spaceBegin <= address);
	uint64_t diff = address - spaceBegin;
	uint64_t q = diff / 8;
	uint64_t r = diff % 8;
	bitmap[q] = bitmap[q] | (1 << r);

	hexDump("bitmap changed: ", bitmap, bitmapSize);
}

void copyHelper(ptr_t *p, ptr_t *pFrom, uint64_t size, uint64_t tag)
{
	ptr_t pTo = (ptr_t)copyPtrEnd;
	memcpy(copyPtrEnd, pFrom, size * wordSize);
	*pFrom = pTo | 1; // Add forward bit.
	copyPtrEnd += size;
	*p = pTo | tag;
	markInBitmap(toBitmap, toSpaceBegin, (uint64_t*) pTo);
#ifdef DEBUG_LOG_GC
	printf("Copy object to ToSpace.\n");
	printf("size = %d, new address = %p\n", size, pTo);
#endif
}

/// Check that address is valid by looking into bitset.
/// We only check FromSpace case.
int isAddressValid(uint64_t *p)
{
	if (!(p >= fromSpaceBegin && p < fromSpaceEnd))
	{
		return 0;
	}

	uint64_t diff = p - fromSpaceBegin;
	uint64_t q = diff / 8;
	uint64_t r = diff % 8;

	return fromBitmap[q] & (1 << r);
}

void copyVectorOrClosure(ptr_t *p, uint64_t tag)
{
	ptr_t pHeap = (*p - tag);
	ptr_t *pFrom = (ptr_t *)pHeap;

	if (!isAddressValid(pFrom)) return;

	uint64_t firstCell = *pFrom; // Value in the first cell of the object.
	if (firstCell & 1)
	{
		// forward pointer.
		*p = (firstCell - 1) + tag;
#ifdef DEBUG_LOG_GC
		printf("Already copied to ToSpace. New address: %p\n", *p);
#endif
	}
	else
	{
		ptr_t size = (firstCell >> fixnumShift) + 1;
		copyHelper(p, pFrom, size, tag);
	}
}

void copyString(ptr_t *p)
{
	ptr_t pHeap = (*p - stringTag);
	ptr_t *pFrom = (ptr_t *)pHeap;

	if (!isAddressValid(pFrom)) return;

	uint64_t firstCell = *pFrom; // Value in the first cell of the object.
	if (firstCell & 1)
	{
		// forward pointer.
		*p = (firstCell - 1) + stringTag;
#ifdef DEBUG_LOG_GC
		printf("Already copied to ToSpace. New address: %p\n", *p);
#endif
	}
	else
	{
		ptr_t size = ((firstCell >> fixnumShift) + (wordSize - 1) / wordSize) + 1;
		copyHelper(p, pFrom, size, stringTag);
	}
}

void copyPair(ptr_t *p)
{
	ptr_t pHeap = (*p - pairTag);
	ptr_t *pFrom = (ptr_t *)pHeap;

	printf("valid address? %d\n", isAddressValid(pFrom));

	if (!isAddressValid(pFrom)) return;

	uint64_t firstCell = *pFrom; // Value in the first cell of the object.
	if (firstCell & 1)
	{
		// forward pointer.
		*p = (firstCell - 1) + pairTag;
#ifdef DEBUG_LOG_GC
		printf("Already copied to ToSpace. New address: %p\n", *p);
#endif
	}
	else
	{
		ptr_t size = 3;
		copyHelper(p, pFrom, size, pairTag);
	}
}

/// Cheney copying algorithm.
void copyData(ptr_t *p)
{
#ifdef DEBUG_LOG_GC
	printf("copyData: %p\n", *p);
#endif
	if (isVector(*p))
	{
		copyVectorOrClosure(p, vectorTag);
	}
	else if (isClosure(*p))
	{
		copyVectorOrClosure(p, closureTag);
	}
	else if (isString(*p))
	{
		copyString(p);
	}
	else if (isPair(*p))
	{
		copyPair(p);
	}
}

void *allocate(uint64_t *rootStack, uint64_t size)
{
	printf("allocate size = %d\n", (int) size);
#ifdef DEBUG_FORCE_GC
	collect(rootStack, size);
#endif
	uint64_t sizeAligned = (size + (wordSize - 1)) / wordSize;
	if (freePointer + size >= fromSpaceEnd)
	{
		collect(rootStack, size);
	}
	void *p = freePointer;
	markInBitmap(fromBitmap, fromSpaceBegin, freePointer);
	freePointer += sizeAligned;
	return p;
}

static void finishCollection()
{
	uint64_t *pBegin = fromSpaceBegin;
	uint64_t *pEnd = fromSpaceEnd;

	fromSpaceBegin = toSpaceBegin;
	fromSpaceEnd = toSpaceEnd;
	toSpaceBegin = pBegin;
	toSpaceEnd = pEnd;

	freePointer = copyPtrBegin;

	memset(toSpaceBegin, 0, (toSpaceEnd - toSpaceBegin) * wordSize);

	uint8_t *bitmap = fromBitmap;
	fromBitmap = toBitmap;
	toBitmap = bitmap;

	memset(toBitmap, 0, bitmapSize);

#ifdef DEBUG_LOG_GC
	hexDump("from space", fromSpaceBegin, (fromSpaceEnd - fromSpaceBegin) * wordSize);
	hexDump("to space", toSpaceBegin, (toSpaceEnd - toSpaceBegin) * wordSize);
	printf("######################\n");
	printf("## collection finished\n");
	printf("######################\n");
#endif

}

void collect(uint64_t *rootStack, int64_t bytesNeeded)
{
	if (rootStack < rootStackBegin)
	{
		printf("Error! rootStack (0x%p) is less than rootStackBegin (0x%p)", rootStack, rootStackBegin);
		exit(1);
	}

#ifdef DEBUG_LOG_GC
	printf("-- we are in collect\n");
	printf("-- freePointer    = 0x%p\n", freePointer);
	printf("-- fromSpaceBegin = 0x%p\n", fromSpaceBegin);
	printf("-- fromSpaceEnd   = 0x%p\n", fromSpaceEnd);
	printf("-- stack          = 0x%p\n", rootStack);
	printf("-- size           = %d\n", bytesNeeded);
	hexDump("root stack", rootStackBegin, (rootStack - rootStackBegin) * wordSize);
	hexDump("from space", fromSpaceBegin, (fromSpaceEnd - fromSpaceBegin) * wordSize);
	hexDump("to space", toSpaceBegin, (toSpaceEnd - toSpaceBegin) * wordSize);
#endif

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

	// Walk and copy remaining data.
	while (copyPtrBegin < copyPtrEnd)
	{
		copyData(copyPtrBegin);
		copyPtrBegin++;
	}

	finishCollection();
}

void hexDump(const char *desc, const void *addr, int len)
{
	if (desc)
	{
		printf("%s:\n", desc);
	}

	if (len > 126) len = 126;

	const unsigned char *pch = (const unsigned char *)addr;

	// Process by 16 bytes.
	for (size_t i = 0; i < len; i += 16)
	{
		// Print address.
		printf("0x%p   ", pch + i);

		// Print each byte.
		for (size_t j = 0; j < 16; j++)
		{
			if (j == 8)
				printf(" ");

			printf("%02x ", pch[i + j]);
		}

		printf("  ");

		// Print quads.
		for (size_t j = 0; j < 2; j++)
		{
			const uint64_t *pu64 = (const uint64_t *)(pch + i);
			const uint64_t *pu642 = pu64 + 1;
			uint64_t va = pu64[j];
			printf("0x%016x ", pu64[j]);
		}

		// New line.
		printf("\n");
	}
}

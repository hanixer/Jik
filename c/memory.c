#include "memory.h"
#include <windows.h>
#include <stdio.h>
#include <assert.h>

// #define DEBUG_LOG_GC
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
uint64_t *rootStackCurr;

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
	rootStackCurr = rootStackBegin;
}

void markInBitmap(uint8_t *bitmap, uint64_t *spaceBegin, uint64_t *address)
{
	assert(spaceBegin <= address);
	uint64_t diff = address - spaceBegin;
	uint64_t q = diff / 8;
	uint64_t r = diff % 8;
	bitmap[q] = bitmap[q] | (1 << r);
}

/// Check that address is valid by looking into bitset.
/// We only check FromSpace case.
int isAddressValid(uint64_t *p)
{
	if (p >= fromSpaceBegin && p < fromSpaceEnd)
	{
		uint64_t diff = p - fromSpaceBegin;
		uint64_t q = diff / 8;
		uint64_t r = diff % 8;

		return fromBitmap[q] & (1 << r);
	}
	else
	{
		return 0;
	}
}

void calculateSizeAndSecondaryTag(ptr_t *p, uint64_t firstCell, uint64_t *wordsCount, uint64_t *secondaryTag)
{
	if (isVector(*p))
	{
		*wordsCount = (firstCell >> fixnumShift) + 1;
		*secondaryTag = vectorTag;
	}
	else if (isClosure(*p))
	{
		*wordsCount = (firstCell >> closureSizeShift) + 1;
	}
	else if (isString(*p))
	{
		uint64_t shifted = firstCell >> stringSizeShift;
		uint64_t rounded = shifted + (wordSize - 1);
		*wordsCount = (rounded / wordSize) + 1;
		*secondaryTag = stringTag;
	}
	else if (isPair(*p))
	{
		*wordsCount = 3;
	}
	else if (isSymbol(*p))
	{
		*wordsCount = 2;
	}
	else if (isFlonum(*p))
	{
		*wordsCount = 2;
	}
}

void copyError()
{
	printf("error during copy");
	fflush(stdout);
	exit(1);
}

static int isForwardPointer(uint64_t firstCell)
{
	return (firstCell & 1) != 0;
}

void copyHelper(ptr_t *p, uint64_t primaryTag)
{
	ptr_t pHeap = *p - primaryTag;
	ptr_t *pFrom = (ptr_t *)pHeap;

	if (!isAddressValid(pFrom))
		return;

	uint64_t firstCell = *pFrom; // Value in the first cell of the object.
	if (isForwardPointer(firstCell))
	{
		// Get forward pointer and combine it with the tag.
		// The assumptions is that all objects have at least two cells.
		// And we can store forward pointer in the second cell.
		*p = (firstCell - 1) + primaryTag;
#ifdef DEBUG_LOG_GC
		printf("Already copied to ToSpace. New address: %p\n", *p);
#endif
	}
	else
	{
		// Copy an object from FromSpace to ToSpace.
		// Calculate amount of words occupied by the object.
		// Mark a bit in bitmap corresponding to the new address of the object.

		uint64_t wordsCount = 0;
		uint64_t secondaryTag = 0;
		calculateSizeAndSecondaryTag(p, firstCell, &wordsCount, &secondaryTag);

		if (wordsCount == 1)
		{
			copyError();
		}

		ptr_t pTo = (ptr_t)copyPtrEnd;
		memcpy(copyPtrEnd, pFrom, wordsCount * wordSize);
		// TODO: secondary tag
		// copyPtrEnd[0] |= secondaryTag; // Add secondary tag for typed object if needed.
		copyPtrEnd += wordsCount;

		pFrom[0] = pTo | 1; // Add forward marker.

		*p = pTo | primaryTag;

		markInBitmap(toBitmap, toSpaceBegin, (uint64_t *)pTo);

#ifdef DEBUG_LOG_GC
		printf("Copy object to ToSpace.\n");
		printf("size = %d, new address = %p\n", wordsCount, pTo);
#endif
	}
}

/// If given object should be copied - copy it.
/// If object already copied - write new address.
/// If it is not a valid object or an immediate value - skip it.
void copyData(ptr_t *p)
{
#ifdef DEBUG_LOG_GC
	printf("copyData: %p\n", *p);
#endif
	if (isTypedObject(*p))
	{
		copyHelper(p, typedObjectTag);
	}
	else if (isClosure(*p))
	{
		copyHelper(p, closureTag);
	}
	else if (isString(*p))
	{
		copyHelper(p, stringTag);
	}
	else if (isPair(*p))
	{
		copyHelper(p, pairTag);
	}
	else if (isSymbol(*p))
	{
		copyHelper(p, symbolTag);
	}
	else if (isFlonum(*p))
	{
		copyHelper(p, flonumTag);
	}
}

void *allocate(uint64_t *rootStack, uint64_t size)
{

	uint64_t sizeAligned = (size + (wordSize - 1)) / wordSize;
// printf("allocate: size = %d, aligned = %d, remaining = %d\n", size, sizeAligned, fromSpaceEnd - freePointer);
#ifdef DEBUG_LOG_GC
	printf("allocate: size = %d, aligned = %d\n", size, sizeAligned);
#endif
#ifdef DEBUG_FORCE_GC
	collect(rootStack, size);
#endif
	if (freePointer + size >= fromSpaceEnd)
	{
		collect(rootStack, size);
	}
	void *p = freePointer;
	markInBitmap(fromBitmap, fromSpaceBegin, freePointer);
	freePointer += sizeAligned;

	if (freePointer >= fromSpaceEnd)
	{
		fprintf(stderr, "no more heap space\n");
		fflush(stderr);
		exit(1);
	}

	return p;
}

void *allocateC(uint64_t size)
{
	return allocate(rootStackCurr, size);
}

static void finishCollection(uint64_t *rootStack)
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
	hexDump("root stack", rootStackBegin, (rootStack - rootStackBegin + 1) * wordSize);
	for (int i = 0; globRootsTable[i] != 0; i++)
	{
		hexDump("global", globRootsTable[i], wordSize);
	}
	hexDump("from space", fromSpaceBegin, (freePointer - fromSpaceBegin + 2) * wordSize);
	// hexDump("to space", toSpaceBegin, 10 * wordSize);
	printf("######################\n");
	printf("## collection finished\n");
	printf("######################\n");
	fflush(stdout);
#endif
}

void collect(uint64_t *const rootStack, int64_t bytesNeeded)
{
	// printf("!!!!!! collect: size = %d\n", bytesNeeded);
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
	hexDump("root stack", rootStackBegin, (rootStack - rootStackBegin + 1) * wordSize);
	hexDump("from space", fromSpaceBegin, (freePointer - fromSpaceBegin + 2) * wordSize);
// hexDump("to space", toSpaceBegin, (freePointer - fromSpaceBegin + 2) * wordSize);
#endif

	copyPtrBegin = toSpaceBegin;
	copyPtrEnd = toSpaceBegin;

	// Copy root stack data.
	for (uint64_t *curr = rootStack; curr >= rootStackBegin; curr--)
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

	finishCollection(rootStack);
}

//////////////////////////////////////
// Allocations.

/// Argument - is the number of vector elements.
/// Should be converted from scheme representation.
ptr_t allocateVector(ptr_t s)
{
	uint64_t size = fixnumToInt(s);
	uint64_t cells = size + 1;
	uint64_t* p = (uint64_t*)allocateC(cells * wordSize);
	p[0] = size << vectorSizeShift;
	return ((ptr_t)p) | typedObjectTag;
}

/// free - number of free arguments
/// procAddr - address of procedure
ptr_t allocateClosure(int free, uint64_t procAddr)
{
	uint64_t cells = free + 2;
	uint64_t* p = (uint64_t*)allocateC(cells * wordSize);
	p[0] = (cells - 1) << closureSizeShift; // Store count of free args + cell for proc.
	p[1] = procAddr;
	return ((ptr_t)p) | closureTag;
}

ptr_t allocatePair()
{
	uint64_t cells = 3;
	uint64_t* p = (uint64_t*)allocateC(cells * wordSize);
	p[0] = 0;
	return ((ptr_t)p) | pairTag;
}


void hexDump(const char *desc, const void *addr, int len)
{
	if (desc)
	{
		printf("%s:\n", desc);
	}

	// if (len > 126) len = 126;

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

#include "memory.h"
#include <windows.h>
#include <stdio.h>


extern ptr_t** globRootsTable;

uint64_t* freePointer;

/// Points to the beginning of FromSpace.
uint64_t* fromSpaceBegin;

/// Points to one memory location past the FromSpace.
uint64_t* fromSpaceEnd;

/// Points to the beginning of ToSpace.
static uint64_t* toSpaceBegin;

/// Points to one memory location past the ToSpace.
static uint64_t* toSpaceEnd;

/// Pointers used in copying of live data.
static uint64_t* copyPtrBegin;
static uint64_t* copyPtrEnd;


uint64_t* rootStackBegin;

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

void gcInitialize(uint64_t heapSize, uint64_t rootStackSize) {
    char* pointer = allocateProtectedSpace(heapSize);
    freePointer = (uint64_t*) pointer;
    fromSpaceBegin = freePointer;
    fromSpaceEnd = (uint64_t*) (pointer + heapSize);

	pointer = allocateProtectedSpace(heapSize);
    toSpaceBegin = (uint64_t*) pointer;
	toSpaceEnd = (uint64_t*) (pointer + heapSize);

	pointer = allocateProtectedSpace(rootStackSize);
	rootStackBegin = (uint64_t*) pointer;
	rootStackBegin[0] = 0;
}

/// Cheney copying algorithm.
void copyData(ptr_t* p) {
	if (isVector(*p)) {
		ptr_t pHeap = (*p - vectorTag);
		if (pHeap & 1) {
			// forward pointer.
			*p = (pHeap - 1) + vectorTag;
		}
		else {
			ptr_t* pFrom = (ptr_t*)pHeap;
			ptr_t size = ((*pFrom) >> fixnumShift) + 1;
			ptr_t pTo = (ptr_t) copyPtrEnd;
			memcpy(copyPtrEnd, pFrom, size * wordSize);
			*pFrom = pTo | 1; // Add forward bit.
			copyPtrEnd += size;
			*p = pTo;
		}
	}

	// if (isPair(p) || isVector(p) || isString(p) || isClosure(p)) {

	// }
}

void hexDump(const char * desc, const void * addr, const int len);

void collect(uint64_t* rootStack, int64_t bytesNeeded) {
    printf("--- we are in collect\n");
    printf("--- freePointer    = %p\n", freePointer);
    printf("--- fromSpaceBegin = %p\n", fromSpaceBegin);
    printf("--- fromSpaceEnd   = %p\n", fromSpaceEnd);
    printf("--- stack          = %p\n", rootStack);
    printf("--- size           = %d\n", bytesNeeded);
	hexDump("from space", fromSpaceBegin, (fromSpaceEnd - fromSpaceBegin) * wordSize);
	hexDump("to space", toSpaceBegin, (toSpaceEnd - toSpaceBegin) * wordSize);

	copyPtrBegin = toSpaceBegin;
	copyPtrEnd = toSpaceBegin;

	// Copy root stack data.
	for (uint64_t* curr = rootStack; *curr != 0; curr--) {
		copyData(curr);
	}

	// Copy data pointed by globals.
	for (int i = 0; globRootsTable[i] != 0; i++) {
		copyData(globRootsTable[i]);
	}

	while (copyPtrBegin < copyPtrEnd) {
		copyData(copyPtrBegin);
		copyPtrBegin++;
	}

	uint64_t* pBegin = fromSpaceBegin;
	uint64_t* pEnd = fromSpaceEnd;

	fromSpaceBegin = toSpaceBegin;
	fromSpaceEnd = toSpaceEnd;
	toSpaceBegin = pBegin;
	toSpaceEnd = pEnd;

	memset(toSpaceBegin, 0, (toSpaceEnd - toSpaceBegin) * wordSize);

	hexDump("from space", fromSpaceBegin, (fromSpaceEnd - fromSpaceBegin) * wordSize);
	hexDump("to space", toSpaceBegin, (toSpaceEnd - toSpaceBegin) * wordSize);
}

void hexDump (const char * desc, const void * addr, const int len) {
// Copyright 2018 Dominik Liebler
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
    int i;
    unsigned char buff[17];
    const unsigned char * pc = (const unsigned char *)addr;

    // Output description if given.

    if (desc != NULL)
        printf ("%s:\n", desc);

    // Length checks.

    if (len == 0) {
        printf("  ZERO LENGTH\n");
        return;
    }
    else if (len < 0) {
        printf("  NEGATIVE LENGTH: %d\n", len);
        return;
    }

    // Process every byte in the data.
    for (i = 0; i < len; i++) {
        // Multiple of 16 means new line (with line offset).
        if ((i % 16) == 0) {
            // Don't print ASCII buffer for the "zeroth" line.
            if (i != 0)
                printf ("  %s\n", buff);

            // Output the offset.
            printf ("%08p ", pc + i);
        }

        // Now the hex code for the specific character.
        printf (" %02x", pc[i]);

        // And buffer a printable ASCII character for later.
        if ((pc[i] < 0x20) || (pc[i] > 0x7e)) // isprint() may be better.
            buff[i % 16] = '.';
        else
            buff[i % 16] = pc[i];
        buff[(i % 16) + 1] = '\0';
    }

    // Pad out last line if not exactly 16 characters.
    while ((i % 16) != 0) {
        printf ("   ");
        i++;
    }

    // And print the final ASCII buffer.
    printf ("  %s\n", buff);
}
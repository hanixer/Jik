#include <stdio.h>
#include <windows.h>
#include <stdint.h>

#define wordSize 8
#define fixnumShift 0x02
#define fixnumMask 0x03
#define fixnumTag 0x00
#define trueLiteral 0x6F
#define falseLiteral 0x2F
#define nilLiteral 0x8F
#define charShift 0x08
#define charMask 0xFF
#define charTag 0x0F
#define pairTag 0x01
#define pairMask 0x07
#define pairSize (2 * wordSize)
#define vectorTag 0x05
#define vectorMask 0x07
#define stringTag 0x06
#define stringMask 0x07
#define carOffset 0
#define cdrOffset wordSize

extern int schemeEntry(char* stack, char* heap);

typedef uint64_t ptr;
typedef ptr* ptrptr;

ptrptr toPtrptr(ptr p) { return (ptrptr) p; }
ptr charToPtr(char c) { return (((ptr) c << charShift) | charTag); }
int isFixnum(ptr p) {return (p & fixnumMask) == fixnumTag; }
int isPair(ptr p) { return (p & pairMask) == pairTag; }
int isVector(ptr p) { return (p & vectorMask) == vectorTag; }
int isString(ptr p) { return (p & stringMask) == stringTag; }
int isNil(ptr p) { return p == nilLiteral; }
ptr car(ptr p) { return (ptr) *(toPtrptr(p - pairTag)); }
ptr cdr(ptr p) { return *(toPtrptr(p - pairTag + wordSize)); }
int vectorSize(ptr p) { return (int) *(toPtrptr(p - vectorTag)) >> fixnumShift; }
ptr vectorRef(ptr p, int i) { return *(toPtrptr(p - vectorTag + (i + 1) * wordSize)); }
int stringSize(ptr p) { return (int) *(toPtrptr(p - stringTag)) >> fixnumShift; }
char stringRef(ptr p, int i) { return (*(toPtrptr(p - stringTag + wordSize + i))); }

static void printPair(ptr p);
static void printVector(ptr p);
static void printString(ptr p);

static void printPtr(ptr p) {
    if (isFixnum(p)) {
        int n = (int) p;
        printf("%d", n >> fixnumShift);
    } else if (p == falseLiteral) {
        printf("#f");
    } else if (p == trueLiteral) {
        printf("#t");
    } else if (p == nilLiteral) {
        printf("()");
    } else if ((p & charMask) == charTag) {
        char stringg[2] = {0, 0};
        stringg[0] = p >> charShift;
        printf("#\\%s", stringg);
    } else if (isPair(p)) {
        printPair(p);
    } else if (isVector(p)) {
        printVector(p);
    } else if (isString(p)) {
        printString(p);
    } else {
        printf("<unknown 0x%08x>", p);
    }
}

static void printPair(ptr p) {
    printf("(");
    printPtr(car(p));
    ptr curr = cdr(p);
    while (!isNil(curr)) {
        if (isPair(curr)) {
            printf(" ");
            printPtr(car(curr));
            curr = cdr(curr);
        } else {
            printf(" . ");
            printPtr(curr);
            break;
        }
    }
    printf(")");
}

static void printVector(ptr p) {
    printf("#(");
    for (int i = 0; i < vectorSize(p); ++i) {
        printPtr(vectorRef(p, i));
        if (i < vectorSize(p) - 1) {
            printf(" ");
        }
    }
    printf(")");
}

static void printString(ptr p) {
    printf("\"");
    for (int i = 0; i < stringSize(p); ++i) {
        printf("%c", stringRef(p, i));
    }
    printf("\"");
}

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

int main() {
    int stackSize = 16 * 4096;
    char* stack = allocateProtectedSpace(stackSize);
    char* stackHigherAddr = stack + stackSize - wordSize;    
    int heapSize = 32 * 4096;
    char* heap = allocateProtectedSpace(heapSize);
    printf("stack=%x stackHigher=%x heap=%x\n", stack, stackHigherAddr, heap);
    printPtr(schemeEntry(stackHigherAddr, heap));
    printf("\n");
    deallocateProtectedSpace(stackHigherAddr, stackSize);
    return 0;
}
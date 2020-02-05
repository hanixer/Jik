#include <stdio.h>
#include <stdint.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <windows.h>
#include <string.h>
#include <errno.h>
#include "memory.h"
#include "objects.h"

extern int schemeEntry();
extern char *globVarNameTable[];

static void printPair(ptr_t p);
static void printVector(ptr_t p);
static void printString(ptr_t p);
char *allocAndCopyString(ptr_t p);

void printPtr(ptr_t p)
{
    if (isFixnum(p))
    {
        int n = (int)p;
        printf("%d", n >> fixnumShift);
    }
    else if (p == falseLiteral)
    {
        printf("#f");
    }
    else if (p == trueLiteral)
    {
        printf("#t");
    }
    else if (p == nilLiteral)
    {
        printf("()");
    }
    else if ((p & charMask) == charTag)
    {
        char stringg[2] = {0, 0};
        stringg[0] = p >> charShift;
        printf("#\\%s", stringg);
    }
    else if (isPair(p))
    {
        printPair(p);
    }
    else if (isVector(p))
    {
        printVector(p);
    }
    else if (isString(p))
    {
        printString(p);
    }
    else if (isEof(p))
    {
        printf("#!eof");
    }
    else
    {
        printf("<unknown 0x%08x>", p);
    }
    fflush(stdout);
}

static void printPair(ptr_t p)
{
    printf("(");
    printPtr(car(p));
    ptr_t curr = cdr(p);
    while (!isNil(curr))
    {
        if (isPair(curr))
        {
            printf(" ");
            printPtr(car(curr));
            curr = cdr(curr);
        }
        else
        {
            printf(" . ");
            printPtr(curr);
            break;
        }
    }
    printf(")");
}

static void printVector(ptr_t p)
{
    printf("#(");
    for (int i = 0; i < vectorSize(p); ++i)
    {
        printPtr(vectorRef(p, i));
        if (i < vectorSize(p) - 1)
        {
            printf(" ");
        }
    }
    printf(")");
}

static void printString(ptr_t p)
{
    printf("\"");
    for (int i = 0; i < stringSize(p); ++i)
    {
        printf("%c", stringRef(p, i));
    }
    printf("\"");
}

ptr_t s_write(ptr_t fd, ptr_t str, ptr_t len)
{
    int cfd = fixnumToInt(fd);
    char *cstr = stringData(str);
    int clen = fixnumToInt(len);
    int bytes = write(cfd, cstr, clen);
    return intToFixnum(bytes);
}

ptr_t s_read(ptr_t fd, ptr_t str, ptr_t len)
{
    int cfd = fixnumToInt(fd);
    char *cstr = stringData(str);
    int clen = fixnumToInt(len);
    int bytes = read(cfd, cstr, clen);
    return intToFixnum(bytes);
}

ptr_t s_print6args(ptr_t a1, ptr_t a2, ptr_t a3, ptr_t a4, ptr_t a5, ptr_t a6)
{
    printPtr(a1);
    printPtr(a2);
    printPtr(a3);
    printPtr(a4);
    printPtr(a5);
    printPtr(a6);

    return 0;
}

void asmError()
{
    fprintf(stderr, "error, probably bad procedure call");
    exit(1);
}

void procError(void* caller)
{
    fprintf(stderr, "error, closure is expected for call, but wrong object is given, caller = 0x%p", caller);
    exit(1);
}

void wrongArgCountError()
{
    fprintf(stderr, "error, wrong number of arguments is passed to a function");
    exit(1);
}

void globVarError(char *varAddress)
{
    // see printGlobalOriginals in CodePrinter.fs
    for (int i = 0; globVarNameTable[i] != 0; i += 2)
    {
        // the first entry is an address
        if (globVarNameTable[i] == varAddress)
        {
            // the second entry is a string
            char *str = globVarNameTable[i + 1];
            fprintf(stderr, "global variable '%s' is not initialized\n", str);
            exit(1);
        }
    }

    fprintf(stderr, "global variable with address '%p' was not found\n", varAddress);
    exit(1);
}

void s_error(ptr_t x)
{
    fprintf(stderr, "error!\n");
    printf("error!\n");
    printPtr(x);
    exit(1);
}

char *allocAndCopyString(ptr_t p)
{
    int size = stringSize(p);
    char *str = stringData(p);
    char *newStr = malloc(size + 1);
    for (int i = 0; i < size; ++i)
    {
        newStr[i] = str[i];
    }
    newStr[size] = 0;
    return newStr;
}

ptr_t s_openFileW(ptr_t filename)
{
    char *str = allocAndCopyString(filename);
    int fd = open(str, O_CREAT | O_WRONLY);
    int err = errno;
    char *estr = strerror(err);
    free(str);
    return intToFixnum(fd);
}

ptr_t s_openFileR(ptr_t filename)
{
    char *str = allocAndCopyString(filename);
    int fd = open(str, O_RDONLY);
    int err = errno;
    char *estr = strerror(err);
    free(str);
    return intToFixnum(fd);
}

ptr_t s_closeFile(ptr_t fd)
{
    int cfd = fixnumToInt(fd);
    int ret = close(fd);
    return intToFixnum(ret);
}

static int allocations[5];

void printAllocInfo()
{
    printf("Allocations info:\n");
    printf("vector: %d\n", allocations[0]);
    printf("string: %d\n", allocations[1]);
    printf("cons: %d\n", allocations[2]);
    printf("closure: %d\n", allocations[3]);
    printf("other: %d\n", allocations[5]);
}

void s_exit(ptr_t p)
{
    // printAllocInfo();
    ExitProcess((UINT)fixnumToInt(p));
}

void checkHeapSpaceAvailable(int which)
{
    allocations[which]++;
}

int main()
{
    int stackSize = 10 * 16 * 4096;
    char *stack = allocateProtectedSpace(stackSize);
    char *stackHigherAddr = stack + stackSize - 2 * wordSize;
    // int heapSize = 50;
    int heapSize = 20 * 1024;
    int rootStackSize = stackSize;
    gcInitialize(heapSize, rootStackSize);
    ptr_t result = schemeEntry(stackHigherAddr);
    printPtr(result);
    return 0;
}
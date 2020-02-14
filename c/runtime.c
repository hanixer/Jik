#include <stdio.h>
#include <stdint.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <windows.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include "memory.h"
#include "objects.h"

extern int schemeEntry();
extern char *globVarNameTable[];

static void printSymbol(ptr_t p);
static void printPair(ptr_t p);
static void printVector(ptr_t p);
static void printString(ptr_t p, int);
static void printFlonum(ptr_t p);
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
        printString(p, 1);
    }
    else if (isEof(p))
    {
        printf("#!eof");
    }
    else if (isSymbol(p))
    {
        printSymbol(p);
    }
    else if (isFlonum(p))
    {
        printFlonum(p);
    }
    else
    {
        printf("<unknown 0x%08x>", p);
    }
    fflush(stdout);
}

static void printFlonum(ptr_t p)
{
    printf("%f", flonumData(p));
}

static void printSymbol(ptr_t p)
{
    printf("'");
    printString(stringOfSymbol(p), 0);
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

static void printString(ptr_t p, int printDQuotes)
{
    if (printDQuotes)
        printf("\"");

    for (int i = 0; i < stringSize(p); ++i)
    {
        printf("%c", stringRef(p, i));
    }

    if (printDQuotes)
        printf("\"");
}

ptr_t s_write(ptr_t fd, ptr_t str, ptr_t len)
{
    // printf("fd %p, str %p, len %p\n", fd, str, len);
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

void procError(void *caller)
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
    int fd = open(str, O_CREAT | O_WRONLY | O_TRUNC);
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
    int err = errno;
    int cfd = fixnumToInt(fd);
    int ret = close(cfd);
    char *estr = strerror(err);
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

ptr_t s_expt(ptr_t x, ptr_t y)
{
    double xx = flonumData(x);
    double yy = flonumData(y);
    double z = pow(xx, yy);
    return allocateFlonum(z);
}

ptr_t s_randomFlonum()
{
    double val = rand() / (RAND_MAX + 1.0);
    return allocateFlonum(val);
}

char *stackTop;

int main()
{
    int stackSize = 10 * 16 * 4096;
    char *stack = allocateProtectedSpace(stackSize);
    char *stackHigherAddr = stack + stackSize - 2 * wordSize;
    stackTop = stackHigherAddr;
    // int heapSize = 50;
    int heapSize = 1000 * 1024;
    int rootStackSize = stackSize;
    gcInitialize(heapSize, rootStackSize);
    ptr_t result = schemeEntry(stackHigherAddr);
    printPtr(result);
    return 0;
}
#include "stdio.h"

int n = 0;

int fib(int n) {
    if (n <= 1) {
        return n;
    } 
    else {
        return n + fib(n - 1);
    }
}

int manyLocals() {
    int a = n + 1;
    int b = n + 1;
    int a1 = a + n + 1;
    int b1 = b + n + 1;
    int a2 = a1 + a + n + 1;
    int b2 = b1 + b + n + 1;
    int a3 = a2 + a1 + a + n + 1;
    int b3 = b2 + b1 + b + n + 1;
    int a4 = a3 + a2 + a1 + a + n + 1;
    int b4 = b3 + b2 + b1 + b + n + 1;
    n += a4 + a3 + a2 + a1 + a + n + 1;
    n += b4 + b3 + b2 + b1 + b + n + 1;
}

int main() {    
    printf("%d\n", fib(100000));
    return 0;
}
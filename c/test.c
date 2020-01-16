#include <stdio.h>

int n = 0;

int reallyGlobality = 293871;

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

int manyArgs(int x1, int x2, int x3, int x4, int x5) {
    n = x1 + x2 + x3 + x5;
    manyLocals();
    n += x1 + x5;
}

int perm() {
    int l = 0;
    if (n) {
        int k = n;
        int m = k % n;
        k += n + l;
        m += m + k;
        k += n + n + n;
        m += n+ l +n + n;
        k += k + l;
        m += m;
        l += k + m;
    }
    else {
        int k = n;
        int m = k % n;
        k -= n + l;
        m -= m + k;
        k -= n + n + n;
        m -= n+ l +n + n;
        k -= k + l;
        m -= m;
        l -= k - m;
    }
    return l;
}

void setQuick() {
    n = 1000000;
}

void broad(char* p) {
    p[0] = 1;
    p[1] = 1;
    p[2] = 2;
    p[3] = 3;
}
int fing(int* a) {
    int b = 0;
    for (size_t i = 0; i < 5; i++)
    {
        b = a[i];
    }
    return b;
}

int thing222(int a) {
    if (a == 0x8) {
        return 100500;
    }
    return 0;
}

int compare(int a, int b) {
    if (a < b) {
        printf("l");
    }
    else if (a <= b) {
        printf("le");
    }
    else if (a == b) {
        printf ("eq");
    }

}

int main() {
    compare(1, 2);
    return 1;
}
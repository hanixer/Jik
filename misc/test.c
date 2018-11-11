#include "scheme.h"

/*
(if #f 1 2)
*/

int n;
int m;
int o;
int p;
int q;




void tutro(int x1, int x2) {
    n = x1;
    p = x2;
}

void makro(int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8) {
    int x = x1;
    x+=x2;
    x+=x3;
    x+=x4;
    x+=x5;
    p = q = p = q = x;
    x += p;
    p = q = x;
    n = x1;
    m = x2;
    p = x8;
    tutro(x7, x6);
}

void mikro(int x1, int x2, int x3, int x4, int x5, int x6) {
    n = x1;
    m = x2;
    mikro(1, m, p, x6, x2, x3);
    p = x6;
}

typedef void (*Func)(int, int);

void comp(int x1, int x2, Func func) {
    if (x1 % x2) return;

    x1 += x2;
    x2 += x1;
    x1 += x2;
    n = x1;
    func(n, m);
}

int main() {    
    mikro(1,2,3,4,5,6);
    comp(1,2,tutro);
    return n ? (m ? o : p) : q;
}
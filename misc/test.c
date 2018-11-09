#include "scheme.h"

/*
(if #f 1 2)
*/

int n;
int m;
int o;
int p;
int q;

void mikro(int x1, int x2, int x3, int x4, int x5, int x6) {
    n = x1;
    m = x2;
    o = x3;
    p = x6;
}

int main() {    
    mikro(1,2,3,4,5,6);
    return n ? (m ? o : p) : q;
}
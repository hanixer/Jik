#include "scheme.h"

/*
(if #f 1 2)
*/

int n;
int m;
int o;
int p;
int q;

typedef void (*Func)(int);

void testing(int n) {
    m = n;
}

int main() {    
    Func func;
    if (m) {
        func =  testing;
    }
    else {
        func = 0;
    }
    func(33);
    return 0;
}
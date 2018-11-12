#include <stdio.h>

union Value;

typedef union Value (*Lambda)();

enum Tag {
    INT,
    BOOLEAN,
    CLOSURE,
    CELL,
    NIL
};

struct Int {
    enum Tag t;
    int n;
};

struct Boolean {
    enum Tag t;
    int b;
};

struct Closure {
    enum Tag t;
    Lambda lam;
};

struct Cell {
    enum Tag t;
    union Value* addr;
};

struct Nil {
    enum Tag t;
};

union Value {
    struct Int z;
    struct Boolean b;
    struct Closure clo;
    struct Cell cell;
    struct Nil nil;
};

typedef union Value Value;

Value makeInt(int n) {
    Value val;
    val.z.t = INT;
    val.z.n = n;
    return val;
}

Value makeBool(int b) {
    Value val;
    val.z.t = BOOLEAN;
    val.b.b = b;
    return val;
}

Value makeNil() {
    Value val;
    val.nil.t = NIL;
    return val;
}

void printValue(Value value) {
    if (value.z.t == INT) {
        printf("%d", value.z.n);
    } else if (value.b.t == BOOLEAN) {
        printf("%s", value.b.b == 0 ? "#f" : "#t");
    } else if (value.nil.t == NIL) {
        printf("()");
    }
}

Value __plus(Value v1, Value v2) {
    return makeInt(v1.z.n + v2.z.n);
}
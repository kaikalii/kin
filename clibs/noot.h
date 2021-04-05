#ifndef NOOT_VALUE_H
#define NOOT_VALUE_H

#include <math.h>
#include <stdio.h>
#include "utf8.h"
#include "tgc.h"

typedef unsigned char byte;

typedef struct NootString {
    byte* s;
    size_t len;
} NootString;

typedef enum NootType {
    Nil,
    Bool,
    Nat,
    Int,
    Real,
    String,
    List,
    Table,
    Function,
    Error
} NootType;

struct NootValue;

typedef struct NootValue(*NootFn)(int, struct NootValue*);

typedef union NootData {
    unsigned char Bool;
    unsigned long Nat;
    long Int;
    double Real;
    NootString String;
    NootFn Function;
    struct NootValue* Error;
} NootData;

typedef struct NootValue {
    NootType type;
    NootData data;
} NootValue;

const NootValue NOOT_NIL = { .type = Nil };

NootValue new_nat(unsigned long i) {
    NootValue val = { .type = Nat, .data = {.Nat = i} };
    return val;
}

NootValue new_int(long i) {
    NootValue val = { .type = Int, .data = {.Int = i} };
    return val;
}

NootValue new_real(double i) {
    NootValue val = { .type = Real, .data = {.Real = i} };
    return val;
}

NootValue new_function(NootFn f) {
    NootValue val = { .type = Function, .data = {.Function = f} };
    return val;
}

NootString new_noot_string(byte* s, size_t len) {
    NootString string = {
        .s = s,
        .len = len,
    };
    return string;
}

NootValue new_string(byte* s, size_t len) {
    NootValue val = { .type = String, .data = {.String = new_noot_string(s, len)} };
    return val;
}

NootValue noot_call(NootValue val, int count, NootValue* args) {
    switch (val.type) {
    case Function:
        return (*val.data.Function)(count, args);
    }
}

NootValue noot_print(int count, NootValue* args) {
    for (int i = 0; i < count; i++) {
        if (i > 0)
            printf("\t");
        NootValue val = args[i];
        switch (val.type) {
        case Nil:
            break;
        case Nat:
            printf("%d", val.data.Nat);
            break;
        case Int:
            printf("%d", val.data.Int);
            break;
        case Real:
            printf("%f", val.data.Real);
            break;
        case String:
            printf("%s", val.data.String.s);
            break;
        }
    }
}

NootValue noot_add(NootValue a, NootValue b) {
    switch (a.type) {
    case Nat:
        switch (b.type) {
        case Nat:
            return new_nat(a.data.Nat + b.data.Nat);
        case Int:
            return new_int(a.data.Nat + b.data.Int);
        case Real:
            return new_real(a.data.Nat + b.data.Real);
        }
    case Int:
        switch (b.type) {
        case Nat:
            return new_int(a.data.Int + b.data.Nat);
        case Int:
            return new_int(a.data.Int + b.data.Int);
        case Real:
            return new_real(a.data.Int + b.data.Real);
        }
    case Real:
        switch (b.type) {
        case Nat:
            return new_real(a.data.Real + b.data.Nat);
        case Int:
            return new_real(a.data.Real + b.data.Int);
        case Real:
            return new_real(a.data.Real + b.data.Real);
        }
    }
}

NootValue noot_sub(NootValue a, NootValue b) {
    switch (a.type) {
    case Nat:
        switch (b.type) {
        case Nat:
            return new_nat(a.data.Nat - b.data.Nat);
        case Int:
            return new_int(a.data.Nat - b.data.Int);
        case Real:
            return new_real(a.data.Nat - b.data.Real);
        }
    case Int:
        switch (b.type) {
        case Nat:
            return new_int(a.data.Int - b.data.Nat);
        case Int:
            return new_int(a.data.Int - b.data.Int);
        case Real:
            return new_real(a.data.Int - b.data.Real);
        }
    case Real:
        switch (b.type) {
        case Nat:
            return new_real(a.data.Real - b.data.Nat);
        case Int:
            return new_real(a.data.Real - b.data.Int);
        case Real:
            return new_real(a.data.Real - b.data.Real);
        }
    }
}

NootValue noot_mul(NootValue a, NootValue b) {
    switch (a.type) {
    case Nat:
        switch (b.type) {
        case Nat:
            return new_nat(a.data.Nat * b.data.Nat);
        case Int:
            return new_int(a.data.Nat * b.data.Int);
        case Real:
            return new_real(a.data.Nat * b.data.Real);
        }
    case Int:
        switch (b.type) {
        case Nat:
            return new_int(a.data.Int * b.data.Nat);
        case Int:
            return new_int(a.data.Int * b.data.Int);
        case Real:
            return new_real(a.data.Int * b.data.Real);
        }
    case Real:
        switch (b.type) {
        case Nat:
            return new_real(a.data.Real * b.data.Nat);
        case Int:
            return new_real(a.data.Real * b.data.Int);
        case Real:
            return new_real(a.data.Real * b.data.Real);
        }
    }
}

NootValue noot_div(NootValue a, NootValue b) {
    switch (a.type) {
    case Nat:
        switch (b.type) {
        case Nat:
            return new_nat(a.data.Nat / b.data.Nat);
        case Int:
            return new_int(a.data.Nat / b.data.Int);
        case Real:
            return new_real(a.data.Nat / b.data.Real);
        }
    case Int:
        switch (b.type) {
        case Nat:
            return new_int(a.data.Int / b.data.Nat);
        case Int:
            return new_int(a.data.Int / b.data.Int);
        case Real:
            return new_real(a.data.Int / b.data.Real);
        }
    case Real:
        switch (b.type) {
        case Nat:
            return new_real(a.data.Real / b.data.Nat);
        case Int:
            return new_real(a.data.Real / b.data.Int);
        case Real:
            return new_real(a.data.Real / b.data.Real);
        }
    }
}

NootValue noot_rem(NootValue a, NootValue b) {
    switch (a.type) {
    case Nat:
        switch (b.type) {
        case Nat:
            return new_nat(a.data.Nat % b.data.Nat);
        case Int:
            return new_int(a.data.Nat % b.data.Int);
        case Real:
            return new_real(fmod(a.data.Nat, b.data.Real));
        }
    case Int:
        switch (b.type) {
        case Nat:
            return new_int(a.data.Int % b.data.Nat);
        case Int:
            return new_int(a.data.Int % b.data.Int);
        case Real:
            return new_real(fmod(a.data.Int, b.data.Real));
        }
    case Real:
        switch (b.type) {
        case Nat:
            return new_real(fmod(a.data.Real, b.data.Nat));
        case Int:
            return new_real(fmod(a.data.Real, b.data.Int));
        case Real:
            return new_real(fmod(a.data.Real, b.data.Real));
        }
    }
}

#endif
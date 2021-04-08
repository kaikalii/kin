#ifndef NOOT_VALUE_H
#define NOOT_VALUE_H

#include <math.h>
#include <stdio.h>
#include "utf8.h"
#include "tgc.h"

static tgc_t noot_gc;

typedef unsigned char byte;

typedef struct NootString {
    byte* s;
    size_t len;
} NootString;

typedef enum NootType {
    Nil,
    Bool,
    Int,
    Real,
    String,
    List,
    Table,
    Function,
    Error,
} NootType;

typedef struct NootValue NootValue;

typedef NootValue(*NootFn)(int, NootValue* args);

typedef union NootData {
    byte Bool;
    unsigned long Nat;
    long Int;
    double Real;
    NootString String;
    NootFn Function;
    struct NootValue* Error;
} NootData;

struct NootValue {
    NootType type;
    NootData data;
};

const NootValue NOOT_NIL = { .type = Nil };

NootValue new_bool(byte b) {
    NootValue val = { .type = Bool, .data = {.Bool = b} };
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
    NootValue val = {
        .type = Function,
        .data = {.Function = f }
    };
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
        case Bool:
            if (val.data.Bool) printf("true");
            else printf("false");
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
    return NOOT_NIL;
}

NootValue noot_println(int count, NootValue* args) {
    NootValue res = noot_print(count, args);
    printf("\n");
    return res;
}

NootValue noot_add(NootValue a, NootValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int + b.data.Int);
        case Real:
            return new_real(a.data.Int + b.data.Real);
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(a.data.Real + b.data.Int);
        case Real:
            return new_real(a.data.Real + b.data.Real);
        }
    }
}

NootValue noot_sub(NootValue a, NootValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int - b.data.Int);
        case Real:
            return new_real(a.data.Int - b.data.Real);
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(a.data.Real - b.data.Int);
        case Real:
            return new_real(a.data.Real - b.data.Real);
        }
    }
}

NootValue noot_mul(NootValue a, NootValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int * b.data.Int);
        case Real:
            return new_real(a.data.Int * b.data.Real);
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(a.data.Real * b.data.Int);
        case Real:
            return new_real(a.data.Real * b.data.Real);
        }
    }
}

NootValue noot_div(NootValue a, NootValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int / b.data.Int);
        case Real:
            return new_real(a.data.Int / b.data.Real);
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(a.data.Real / b.data.Int);
        case Real:
            return new_real(a.data.Real / b.data.Real);
        }
    }
}

NootValue noot_rem(NootValue a, NootValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int % b.data.Int);
        case Real:
            return new_real(fmod(a.data.Int, b.data.Real));
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(fmod(a.data.Real, b.data.Int));
        case Real:
            return new_real(fmod(a.data.Real, b.data.Real));
        }
    }
}

int noot_eq_impl(NootValue a, NootValue b) {
    switch (a.type) {
    case Nil: return b.type == Nil;
    case Bool: return b.type == Bool && a.data.Bool == b.data.Bool;
    case Int:
        switch (b.type) {
        case Int:
            return a.data.Int == b.data.Int;
        case Real:
            return a.data.Int == b.data.Real;
        default: return 0;
        }
    case Real:
        switch (b.type) {
        case Int:
            return a.data.Real == b.data.Int;
        case Real:
            return a.data.Real == b.data.Real;
        default: return 0;
        }
    case String: return b.type == String && utf8cmp(a.data.String.s, b.data.String.s) == 0;
    case Error: return b.type == Error && noot_eq_impl(*a.data.Error, *b.data.Error);
    case Function: return b.type == Function && a.data.Function == b.data.Function;
    }
}

int noot_lt_impl(NootValue a, NootValue b) {
    switch (a.type) {
    case Nil: return 0;
    case Bool: return b.type == Bool && a.data.Bool < b.data.Bool;
    case Int:
        switch (b.type) {
        case Int:
            return a.data.Int < b.data.Int;
        case Real:
            return a.data.Int < b.data.Real;
        default: return 0;
        }
    case Real:
        switch (b.type) {
        case Int:
            return a.data.Real < b.data.Int;
        case Real:
            return a.data.Real < b.data.Real;
        default: return 0;
        }
    case String: return b.type == String && utf8cmp(a.data.String.s, b.data.String.s) < 0;
    case Error: return b.type == Error && noot_eq_impl(*a.data.Error, *b.data.Error);
    case Function: return b.type == Function && a.data.Function < b.data.Function;
    }
}

int noot_gt_impl(NootValue a, NootValue b) {
    switch (a.type) {
    case Nil: return 0;
    case Bool: return b.type == Bool && a.data.Bool > b.data.Bool;
    case Int:
        switch (b.type) {
        case Int:
            return a.data.Int > b.data.Int;
        case Real:
            return a.data.Int > b.data.Real;
        default: return 0;
        }
    case Real:
        switch (b.type) {
        case Int:
            return a.data.Real > b.data.Int;
        case Real:
            return a.data.Real > b.data.Real;
        default: return 0;
        }
    case String: return b.type == String && utf8cmp(a.data.String.s, b.data.String.s) > 0;
    case Error: return b.type == Error && noot_eq_impl(*a.data.Error, *b.data.Error);
    case Function: return b.type == Function && a.data.Function > b.data.Function;
    }
}

NootValue noot_eq(NootValue a, NootValue b) {
    return new_bool(noot_eq_impl(a, b));
}

NootValue noot_neq(NootValue a, NootValue b) {
    return new_bool(!noot_eq_impl(a, b));
}

NootValue noot_lt(NootValue a, NootValue b) {
    return new_bool(noot_lt_impl(a, b));
}

NootValue noot_le(NootValue a, NootValue b) {
    return new_bool(noot_lt_impl(a, b) || noot_eq_impl(a, b));
}

NootValue noot_gt(NootValue a, NootValue b) {
    return new_bool(noot_gt_impl(a, b));
}

NootValue noot_ge(NootValue a, NootValue b) {
    return new_bool(noot_gt_impl(a, b) || noot_eq_impl(a, b));
}

NootValue noot_neg(NootValue val) {
    switch (val.type) {
    case Int: return new_int(-val.data.Int);
    case Real: return new_real(-val.data.Real);
    }
}

NootValue noot_not(NootValue val) {
    if (val.type == Bool) return new_bool(!val.data.Bool);
    else return new_bool(val.type == Nil);
}

int noot_is_true(NootValue val) {
    if (val.type == Bool) return val.data.Bool;
    else return val.type != Nil;
}

#endif
#ifndef KIN_VALUE_H
#define KIN_VALUE_H

#include <math.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>

static char** kin_call_stack = NULL;
static size_t kin_call_stack_len = 0;
static size_t kin_call_stack_capacity = 0;

#ifndef min
#define min(a, b) a < b ? a : b
#endif

void kin_push_call_stack(char* call_string) {
    size_t new_len = kin_call_stack_len + 1;
    if (new_len >= kin_call_stack_capacity) {
        kin_call_stack_capacity = kin_call_stack_capacity == 0 ? 1 : kin_call_stack_capacity * 2;
        kin_call_stack = (char**)realloc(kin_call_stack, kin_call_stack_capacity * sizeof(char*));
    }
    kin_call_stack[kin_call_stack_len] = call_string;
    kin_call_stack_len = new_len;
}

void kin_pop_call_stack() {
    kin_call_stack_len -= 1;
}

void kin_panic_impl(char* message) {
    printf("%s\n", message);
    for (int i = kin_call_stack_len - 1; i >= 0; i--)
        printf("at %s\n", kin_call_stack[i]);
    exit(EXIT_FAILURE);
}

// The type of a byte
typedef unsigned char byte;

// The Kin string representation
typedef struct KinString {
    char* s;
    size_t len;
} KinString;

// Kin types
typedef enum KinType {
    Nil,
    Bool,
    Int,
    Real,
    String,
    Function,
    Closure,
    Error,
} KinType;

static char* kin_type_names[] = {
    "nil",
    "bool",
    "int",
    "real",
    "string",
    "function",
    "function",
    "error",
};

// Foward declarations

typedef struct KinValue KinValue;

// The function pointer type for regular Kin functions
typedef KinValue(*KinFn)(uint8_t, KinValue* args);
// The function pointer type for Kin closures
typedef KinValue(*KinClosureFn)(uint8_t, KinValue* args, KinValue* captures);

// A Kin closure
typedef struct KinFunction {
    KinValue* captures;
    KinClosureFn f;
} KinFunction;

// The data of a Kin value
typedef union KinData {
    bool Bool;
    unsigned long Nat;
    long Int;
    double Real;
    KinString String;
    KinFn Function;
    KinFunction Closure;
    struct KinValue* Error;
} KinData;

// A kin value with a type and data
struct KinValue {
    KinType type;
    KinData data;
    KinValue* mom;
    KinValue* dad;
};

#define new_val(_type, ...) (KinValue) { .type = _type, .data = {._type = __VA_ARGS__}, .mom = NULL, .dad = NULL }
#define new_bool(b) new_val(Bool, b)
#define new_int(i) new_val(Int, i)
#define new_real(i) new_val(Real, i)
#define new_function(f) new_val(Function, f)
#define new_closure(function, caps) new_val(Closure, { .f = function, .captures = caps })
#define new_kin_string(string, l) (KinString) { .s = string, .len = l }
#define new_string(s, len) new_val(String, new_kin_string(s, len))

// The nil Kin value
static KinValue KIN_NIL = { .type = Nil, .mom = NULL, .dad = NULL };
// The true Kin value
static KinValue KIN_TRUE = new_bool(true);
// The false Kin value
static KinValue KIN_FALSE = new_bool(false);

KinValue kin_head(KinValue val) {
    val.mom = NULL;
    val.dad = NULL;
    return val;
}

KinValue kin_mom(uint8_t count, KinValue* args) {
    KinValue val = count >= 1 ? args[0] : KIN_NIL;
    return val.mom ? *val.mom : KIN_NIL;
}

KinValue kin_dad(uint8_t count, KinValue* args) {
    KinValue val = count >= 1 ? args[0] : KIN_NIL;
    return val.dad ? *val.dad : KIN_NIL;
}

void kin_binary_type_panic(char* message, KinType a, KinType b) {
    char str[256];
    sprintf(str, message, kin_type_names[a], kin_type_names[b]);
    kin_panic_impl(str);
}

void kin_unary_type_panic(char* message, KinType ty) {
    char str[256];
    sprintf(str, message, kin_type_names[ty]);
    kin_panic_impl(str);
}

// Create a new Kin error from a value
KinValue kin_error(uint8_t count, KinValue* inner) {
    return new_val(Error, inner);
}

// Call a Kin function or closure value
KinValue kin_call(KinValue val, int count, KinValue* args, char* call_site) {
    kin_push_call_stack(call_site);
    KinValue res;
    switch (val.type) {
    case Function:;
        res = (*val.data.Function)(count, args);
        kin_pop_call_stack();
        return res;
    case Closure:;
        res = (*val.data.Closure.f)(count, args, val.data.Closure.captures);
        kin_pop_call_stack();
        return res;
    default:
        kin_unary_type_panic("Attempted to call %s value", val.type);
        return KIN_NIL;
    }
}

KinValue kin_print(uint8_t count, KinValue* args) {
    KinValue val = count >= 1 ? args[0] : KIN_NIL;
    switch (val.type) {
    case Nil:
        printf("nil");
        break;
    case Bool:
        if (val.data.Bool) printf("true");
        else printf("false");
        break;
    case Int:
        printf("%ld", val.data.Int);
        break;
    case Real:;
        char str[50];
        sprintf(str, "%f", val.data.Real);
        int i = strlen(str);
        if (i == 0) break;
        i -= 1;
        while (str[i] == '0' || str[i] == '.') i--;
        printf("%*.*s", i + 1, i + 1, str);
        break;
    case String:;
        int len = val.data.String.len;
        printf("%*.*s", len, len, val.data.String.s);
        break;
    case Function:
    case Closure:
        printf("function");
        break;
    case Error:
        printf("Error: ");
        kin_print(1, val.data.Error);
        break;
    }
    return val;
}

KinValue kin_println(uint8_t count, KinValue* args) {
    KinValue res = kin_print(count, args);
    printf("\n");
    return res;
}

KinValue kin_panic(uint8_t count, KinValue* args) {
    printf("\nKin panicked:\n");
    kin_println(count, args);
    kin_panic_impl("");
    return KIN_NIL;
}

KinValue kin_call_bin_op(KinValue f(KinValue, KinValue), KinValue a, KinValue b, char* call_site) {
    kin_push_call_stack(call_site);
    KinValue res = f(a, b);
    kin_pop_call_stack();
    return res;
}

KinValue kin_add(KinValue a, KinValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int + b.data.Int);
        case Real:
            return new_real(a.data.Int + b.data.Real);
        default: break;
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(a.data.Real + b.data.Int);
        case Real:
            return new_real(a.data.Real + b.data.Real);
        default: break;
        }
    default: break;
    }
    kin_binary_type_panic("Attempted to add incompatible types %s and %s", a.type, b.type);
    return KIN_NIL;
}

KinValue kin_sub(KinValue a, KinValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int - b.data.Int);
        case Real:
            return new_real(a.data.Int - b.data.Real);
        default: break;
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(a.data.Real - b.data.Int);
        case Real:
            return new_real(a.data.Real - b.data.Real);
        default: break;
        }
    default: break;
    }
    kin_binary_type_panic("Attempted to subtract incompatible types %s and %s", a.type, b.type);
    return KIN_NIL;
}

KinValue kin_mul(KinValue a, KinValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int * b.data.Int);
        case Real:
            return new_real(a.data.Int * b.data.Real);
        default: break;
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(a.data.Real * b.data.Int);
        case Real:
            return new_real(a.data.Real * b.data.Real);
        default: break;
        }
    default: break;
    }
    kin_binary_type_panic("Attempted to subtract multiply types %s and %s", a.type, b.type);
    return KIN_NIL;
}

KinValue kin_div(KinValue a, KinValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int / b.data.Int);
        case Real:
            return new_real(a.data.Int / b.data.Real);
        default: break;
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(a.data.Real / b.data.Int);
        case Real:
            return new_real(a.data.Real / b.data.Real);
        default: break;
        }
    default: break;
    }
    kin_binary_type_panic("Attempted to divide incompatible types %s and %s", a.type, b.type);
    return KIN_NIL;
}

KinValue kin_rem(KinValue a, KinValue b) {
    switch (a.type) {
    case Int:
        switch (b.type) {
        case Int:
            return new_int(a.data.Int % b.data.Int);
        case Real:
            return new_real(fmod(a.data.Int, b.data.Real));
        default: break;
        }
    case Real:
        switch (b.type) {
        case Int:
            return new_real(fmod(a.data.Real, b.data.Int));
        case Real:
            return new_real(fmod(a.data.Real, b.data.Real));
        default: break;
        }
    default: break;
    }
    kin_binary_type_panic("Attempted to divide incompatible types %s and %s", a.type, b.type);
    return KIN_NIL;
}

#define bin_fn(f) KinValue f## _fn(uint8_t count, KinValue* args) {  \
    KinValue left = count >= 1 ? args[0] : KIN_NIL; \
    KinValue right = count >= 2 ? args[1] : KIN_NIL; \
    return f(left, right); \
}

bool kin_eq_impl(KinValue a, KinValue b) {
    switch (a.type) {
    case Nil: return b.type == Nil;
    case Bool: return b.type == Bool && a.data.Bool == b.data.Bool;
    case Int:
        switch (b.type) {
        case Int:
            return a.data.Int == b.data.Int;
        case Real:
            return a.data.Int == b.data.Real;
        default: return false;
        }
    case Real:
        switch (b.type) {
        case Int:
            return a.data.Real == b.data.Int;
        case Real:
            return a.data.Real == b.data.Real;
        default: return false;
        }
    case String:
        if (b.type == String && a.data.String.len == b.data.String.len) {
            for (int i = 0; i < a.data.String.len; i++)
                if (a.data.String.s[i] != b.data.String.s[i]) return false;
            return true;
        }
        else return false;
    case Function: return b.type == Function && a.data.Function == b.data.Function;
    case Closure: return b.type == Closure && a.data.Closure.f == b.data.Closure.f;
    case Error: return b.type == Error && kin_eq_impl(*a.data.Error, *b.data.Error);
    default: return false;
    }
}

bool kin_lt_impl(KinValue a, KinValue b) {
    switch (a.type) {
    case Bool: if (b.type == Bool) return a.data.Bool < b.data.Bool; break;
    case Int:
        switch (b.type) {
        case Int: return a.data.Int < b.data.Int;
        case Real: return a.data.Int < b.data.Real;
        default: break;
        }
    case Real:
        switch (b.type) {
        case Int: return a.data.Real < b.data.Int;
        case Real: return a.data.Real < b.data.Real;
        default: break;
        }
    case String:
        if (b.type == String) {
            for (int i = 0; i < min(a.data.String.len, b.data.String.len); i++) {
                byte ac = a.data.String.s[i];
                byte bc = b.data.String.s[i];
                if (ac != bc) return ac < bc;
            }
            return a.data.String.len < b.data.String.len;
        }
        break;
    case Function: if (b.type == Function) return (size_t)a.data.Function < (size_t)b.data.Function; break;
    case Closure: if (b.type == Closure) return (size_t)a.data.Closure.f < (size_t)b.data.Closure.f; break;
    case Error: if (b.type == Error) return kin_eq_impl(*a.data.Error, *b.data.Error); break;
    default: break;
    }
    kin_binary_type_panic("Attempted to compare incompatible types %s and %s", a.type, b.type);
    return false;
}

bool kin_gt_impl(KinValue a, KinValue b) {
    switch (a.type) {
    case Bool: if (b.type == Bool) return a.data.Bool > b.data.Bool; break;
    case Int:
        switch (b.type) {
        case Int: return a.data.Int > b.data.Int;
        case Real: return a.data.Int > b.data.Real;
        default: break;
        }
    case Real:
        switch (b.type) {
        case Int: return a.data.Real > b.data.Int;
        case Real: return a.data.Real > b.data.Real;
        default: break;
        }
    case String:
        if (b.type == String) {
            for (int i = 0; i < min(a.data.String.len, b.data.String.len); i++) {
                byte ac = a.data.String.s[i];
                byte bc = b.data.String.s[i];
                if (ac != bc) return ac > bc;
            }
            return a.data.String.len > b.data.String.len;
        }
        break;
    case Function: if (b.type == Function) return (size_t)a.data.Function > (size_t)b.data.Function; break;
    case Closure: if (b.type == Closure) return (size_t)a.data.Closure.f > (size_t)b.data.Closure.f; break;
    case Error: if (b.type == Error) return kin_eq_impl(*a.data.Error, *b.data.Error); break;
    default: break;
    }
    kin_binary_type_panic("Attempted to compare incompatible types %s and %s", a.type, b.type);
    return false;
}

KinValue kin_eq(KinValue a, KinValue b) {
    return new_bool(kin_eq_impl(a, b));
}

KinValue kin_neq(KinValue a, KinValue b) {
    return new_bool(!kin_eq_impl(a, b));
}

KinValue kin_lt(KinValue a, KinValue b) {
    return new_bool(kin_lt_impl(a, b));
}

KinValue kin_le(KinValue a, KinValue b) {
    return new_bool(kin_lt_impl(a, b) || kin_eq_impl(a, b));
}

KinValue kin_gt(KinValue a, KinValue b) {
    return new_bool(kin_gt_impl(a, b));
}

KinValue kin_ge(KinValue a, KinValue b) {
    return new_bool(kin_gt_impl(a, b) || kin_eq_impl(a, b));
}

bin_fn(kin_add);
bin_fn(kin_sub);
bin_fn(kin_mul);
bin_fn(kin_div);
bin_fn(kin_rem);
bin_fn(kin_eq);
bin_fn(kin_neq);
bin_fn(kin_lt);
bin_fn(kin_le);
bin_fn(kin_gt);
bin_fn(kin_ge);

KinValue kin_neg(KinValue val) {
    switch (val.type) {
    case Int: return new_int(-val.data.Int);
    case Real: return new_real(-val.data.Real);
    default:
        kin_unary_type_panic("Attempted to negate %s", val.type);
        return KIN_NIL;
    }
}

KinValue kin_not(uint8_t count, KinValue* args) {
    KinValue val = count >= 1 ? args[0] : KIN_NIL;
    if (val.type == Bool) return new_bool(!val.data.Bool);
    else return new_bool(val.type == Nil);
}

bool kin_is_true(KinValue val) {
    return (val.type == Bool) * val.data.Bool + (val.type != Bool) * (val.type != Nil && val.type != Error);
}

KinValue kin_assert(uint8_t count, KinValue* args) {
    KinValue val = count >= 1 ? args[0] : KIN_NIL;
    if (!kin_is_true(val)) {
        if (count >= 2) kin_panic(count - 1, args + 1);
        else kin_panic(count, args);
    }
    return val;
}

#endif

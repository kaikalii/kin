#ifndef NOOT_VALUE_H
#define NOOT_VALUE_H

#include <math.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>

static char** noot_call_stack = NULL;
static size_t noot_call_stack_len = 0;
static size_t noot_call_stack_capacity = 0;

#ifndef min
#define min(a, b) a < b ? a : b
#endif

void noot_push_call_stack(char* call_string) {
    size_t new_len = noot_call_stack_len + 1;
    if (new_len >= noot_call_stack_capacity) {
        noot_call_stack_capacity = noot_call_stack_capacity == 0 ? 1 : noot_call_stack_capacity * 2;
        noot_call_stack = (char**)realloc(noot_call_stack, noot_call_stack_capacity * sizeof(char*));
    }
    noot_call_stack[noot_call_stack_len] = call_string;
    noot_call_stack_len = new_len;
}

void noot_pop_call_stack() {
    noot_call_stack_len -= 1;
}

void noot_panic_impl(char* message) {
    printf("%s\n", message);
    for (int i = noot_call_stack_len - 1; i >= 0; i--)
        printf("at %s\n", noot_call_stack[i]);
    exit(1);
}

// The type of a byte
typedef unsigned char byte;

// The Noot string representation
typedef struct NootString {
    char* s;
    size_t len;
} NootString;

// Noot types
typedef enum NootType {
    Nil,
    Bool,
    Int,
    Real,
    String,
    List,
    Tree,
    Function,
    Closure,
    Error,
} NootType;

static char* noot_type_names[] = {
    "nil",
    "bool",
    "int",
    "real",
    "string",
    "list",
    "tree",
    "function",
    "function",
    "error",
};

// Foward declarations

typedef struct NootValue NootValue;

// The function pointer type for regular Noot functions
typedef NootValue(*NootFn)(uint8_t, NootValue* args);
// The function pointer type for Noot closures
typedef NootValue(*NootClosureFn)(uint8_t, NootValue* args, NootValue* captures);

// A Noot closure
typedef struct NootFunction {
    NootValue* captures;
    NootClosureFn f;
} NootFunction;

// A Noot list
typedef struct NootList {
    NootValue* head;
    NootValue* tail;
} NootList;

// A Noot tree
typedef struct NootTree {
    NootValue* left;
    NootValue* middle;
    NootValue* right;
} NootTree;

// The data of a Noot value
typedef union NootData {
    bool Bool;
    unsigned long Nat;
    long Int;
    double Real;
    NootString String;
    NootFn Function;
    NootFunction Closure;
    NootList List;
    NootTree Tree;
    struct NootValue* Error;
} NootData;

// A noot value with a type and data
struct NootValue {
    NootType type;
    NootData data;
};

#define new_val(_type, ...) (NootValue) { .type = _type, .data = {._type = __VA_ARGS__} }
#define new_bool(b) new_val(Bool, b)
#define new_int(i) new_val(Int, i)
#define new_real(i) new_val(Real, i)
#define new_function(f) new_val(Function, f)
#define new_closure(function, caps) new_val(Closure, { .f = function, .captures = caps })
#define new_list(_head, _tail) new_val(List, { .head = _head, .tail = _tail })
#define new_tree(_left, _middle, _right) new_val(Tree, { .left = _left, .middle = _middle, .right = _right })
#define new_noot_string(string, l) (NootString) { .s = string, .len = l }
#define new_string(s, len) new_val(String, new_noot_string(s, len))

// The nil Noot value
static NootValue NOOT_NIL = { .type = Nil };
// The true Noot value
static NootValue NOOT_TRUE = { .type = Bool, .data = {.Bool = true } };
// The false Noot value
static NootValue NOOT_FALSE = { .type = Bool, .data = {.Bool = false } };

void noot_binary_type_panic(char* message, NootType a, NootType b) {
    char str[256];
    sprintf(str, message, noot_type_names[a], noot_type_names[b]);
    noot_panic_impl(str);
}

void noot_unary_type_panic(char* message, NootType ty) {
    char str[256];
    sprintf(str, message, noot_type_names[ty]);
    noot_panic_impl(str);
}

// Create a new Noot error from a value
NootValue noot_error(uint8_t count, NootValue* inner) {
    return new_val(Error, inner);
}

// Call a Noot function or closure value
NootValue noot_call(NootValue val, int count, NootValue* args, char* call_site) {
    noot_push_call_stack(call_site);
    NootValue res;
    switch (val.type) {
    case Function:;
        res = (*val.data.Function)(count, args);
        noot_pop_call_stack();
        return res;
    case Closure:;
        res = (*val.data.Closure.f)(count, args, val.data.Closure.captures);
        noot_pop_call_stack();
        return res;
    default:
        noot_unary_type_panic("Attempted to call %s value", val.type);
        return NOOT_NIL;
    }
}

NootValue noot_print(uint8_t count, NootValue* args) {
    NootValue val = count >= 1 ? args[0] : NOOT_NIL;
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
    case List:;
        printf("[");
        bool printed = false;
        NootValue* curr = &val;
        while (curr->type == List && curr->data.List.head) {
            if (printed) printf(" ");
            noot_print(1, curr->data.List.head);
            printed = true;
            curr = curr->data.List.tail;
        }
        if (curr && curr->type != Nil) {
            if (printed) printf(" ");
            noot_print(1, curr);
        }
        printf("]");
        break;
    case Tree:;
        NootTree* tree = &val.data.Tree;
        printf("{");
        if (tree) {
            if (tree->left) noot_print(1, tree->left);
            else printf("_");
            printf(" ");
            noot_print(1, tree->middle);
            printf(" ");
            if (tree->right) noot_print(1, tree->right);
            else printf("_");
        }
        else printf("_ _ _");
        printf("}");
        break;
    case Function:
    case Closure:
        printf("function");
        break;
    case Error:
        printf("Error: ");
        noot_print(1, val.data.Error);
        break;
    }
    return new_bool(true);
}

NootValue noot_println(uint8_t count, NootValue* args) {
    NootValue res = noot_print(count, args);
    printf("\n");
    return res;
}

NootValue noot_panic(uint8_t count, NootValue* args) {
    printf("\nNoot panicked:\n");
    noot_println(count, args);
    noot_panic_impl("");
    return NOOT_NIL;
}

NootValue noot_call_bin_op(NootValue f(NootValue, NootValue), NootValue a, NootValue b, char* call_site) {
    noot_push_call_stack(call_site);
    NootValue res = f(a, b);
    noot_pop_call_stack();
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
    noot_binary_type_panic("Attempted to add incompatible types %s and %s", a.type, b.type);
    return NOOT_NIL;
}

NootValue noot_sub(NootValue a, NootValue b) {
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
    noot_binary_type_panic("Attempted to subtract incompatible types %s and %s", a.type, b.type);
    return NOOT_NIL;
}

NootValue noot_mul(NootValue a, NootValue b) {
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
    noot_binary_type_panic("Attempted to subtract multiply types %s and %s", a.type, b.type);
    return NOOT_NIL;
}

NootValue noot_div(NootValue a, NootValue b) {
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
    noot_binary_type_panic("Attempted to divide incompatible types %s and %s", a.type, b.type);
    return NOOT_NIL;
}

NootValue noot_rem(NootValue a, NootValue b) {
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
    noot_binary_type_panic("Attempted to divide incompatible types %s and %s", a.type, b.type);
    return NOOT_NIL;
}

#define bin_fn(f) NootValue f## _fn(uint8_t count, NootValue* args) {  \
    NootValue left = count >= 1 ? args[0] : NOOT_NIL; \
    NootValue right = count >= 2 ? args[1] : NOOT_NIL; \
    return f(left, right); \
}

bool noot_eq_impl(NootValue a, NootValue b) {
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
    case Error: return b.type == Error && noot_eq_impl(*a.data.Error, *b.data.Error);
    default: return false;
    }
}

bool noot_lt_impl(NootValue a, NootValue b) {
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
    case Error: if (b.type == Error) return noot_eq_impl(*a.data.Error, *b.data.Error); break;
    default: break;
    }
    noot_binary_type_panic("Attempted to compare incompatible types %s and %s", a.type, b.type);
    return false;
}

bool noot_gt_impl(NootValue a, NootValue b) {
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
    case Error: if (b.type == Error) return noot_eq_impl(*a.data.Error, *b.data.Error); break;
    default: break;
    }
    noot_binary_type_panic("Attempted to compare incompatible types %s and %s", a.type, b.type);
    return false;
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

bin_fn(noot_add);
bin_fn(noot_sub);
bin_fn(noot_mul);
bin_fn(noot_div);
bin_fn(noot_rem);
bin_fn(noot_eq);
bin_fn(noot_neq);
bin_fn(noot_lt);
bin_fn(noot_le);
bin_fn(noot_gt);
bin_fn(noot_ge);

NootValue noot_neg(NootValue val) {
    switch (val.type) {
    case Int: return new_int(-val.data.Int);
    case Real: return new_real(-val.data.Real);
    default:
        noot_unary_type_panic("Attempted to negate %s", val.type);
        return NOOT_NIL;
    }
}

NootValue noot_not(uint8_t count, NootValue* args) {
    NootValue val = count >= 1 ? args[0] : NOOT_NIL;
    if (val.type == Bool) return new_bool(!val.data.Bool);
    else return new_bool(val.type == Nil);
}

bool noot_is_true(NootValue val) {
    return (val.type == Bool) * val.data.Bool + (val.type != Bool) * (val.type != Nil && val.type != Error);
}

NootValue noot_assert(uint8_t count, NootValue* args) {
    NootValue val = count >= 1 ? args[0] : NOOT_NIL;
    if (!noot_is_true(val)) {
        if (count >= 2) noot_panic(count - 1, args + 1);
        else noot_panic(count, args);
    }
    return val;
}

#endif
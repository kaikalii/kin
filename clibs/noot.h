#ifndef NOOT_VALUE_H
#define NOOT_VALUE_H

#include <math.h>
#include <stdio.h>
#include <stdbool.h>
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
    Closure,
    Error,
} NootType;

typedef struct NootValue NootValue;

typedef struct NootListEntry NootListEntry;

typedef NootValue(*NootFn)(uint8_t, NootValue* args);
typedef NootValue(*NootClosureFn)(uint8_t, NootValue* args, NootValue* captures);

typedef struct NootClosure {
    NootValue* captures;
    NootClosureFn f;
} NootClosure;

typedef struct NootListShared {
    size_t capacity;
    NootListEntry* buffer;
    int next_id;
} NootListShared;

typedef struct NootList {
    int id;
    size_t len;
    NootListShared* shared;
} NootList;

typedef union NootData {
    byte Bool;
    unsigned long Nat;
    long Int;
    double Real;
    NootString String;
    NootFn Function;
    NootClosure Closure;
    NootList List;
    struct NootValue* Error;
} NootData;

struct NootValue {
    NootType type;
    NootData data;
};

typedef struct NootListEntry {
    int id;
    NootValue val;
    struct NootListEntry* next;
} NootListEntry;

const NootValue NOOT_NIL = { .type = Nil };
const NootValue NOOT_EMPTY_LIST = {
    .type = List,
    .data = {.List = {.id = 0, .len = 0, .shared = NULL }},
};

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

NootValue new_closure(NootClosureFn f, NootValue* captures) {
    NootValue val = {
        .type = Closure,
        .data = {.Closure = {
            .f = f,
            .captures = captures
        }}
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

NootValue new_list(NootList list) {
    NootValue val = { .type = List, .data = {.List = list} };
    return val;
}

NootValue noot_call(NootValue val, int count, NootValue* args) {
    switch (val.type) {
    case Function:
        return (*val.data.Function)(count, args);
    case Closure:
        return (*val.data.Closure.f)(count, args, val.data.Closure.captures);
    }
}

NootValue noot_list_get(NootList list, int i) {
    if (i < 0 || i >= list.len) return NOOT_NIL;
    NootListEntry* buffer = list.shared->buffer;
    NootListEntry entry = buffer[i];
    while (entry.id > list.id) entry = *entry.next;
    return entry.val;
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
        printf("%d", val.data.Int);
        break;
    case Real:
        printf("%f", val.data.Real);
        break;
    case String:;
        size_t len = val.data.String.len;
        printf("%*.*s", len, len, val.data.String.s);
        break;
    case List:;
        NootList list = val.data.List;
        printf("{");
        for (int i = 0; i < list.len; i++) {
            if (i > 0) printf(", ");
            NootValue item = noot_list_get(list, i);
            noot_print(1, &item);
        }
        printf("}");
        break;
    }
    return NOOT_NIL;
}

NootValue noot_println(uint8_t count, NootValue* args) {
    NootValue res = noot_print(count, args);
    printf("\n");
    return res;
}

NootValue noot_list(uint8_t count, NootValue* values) {
    NootValue val = NOOT_EMPTY_LIST;
    val.data.List.shared = (NootListShared*)tgc_calloc(&noot_gc, 1, sizeof(NootListShared));
    val.data.List.shared->buffer = (NootListEntry*)tgc_calloc(&noot_gc, count, count * sizeof(NootListEntry));
    NootListShared* shared = val.data.List.shared;
    shared->capacity = count;
    val.data.List.len = count;
    NootListEntry* buffer = shared->buffer;
    for (uint8_t i = 0; i < count; i++) {
        buffer[i].val = values[i];
    }
    return val;
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

bool noot_lt_impl(NootValue a, NootValue b) {
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

bool noot_gt_impl(NootValue a, NootValue b) {
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

bool noot_is_true(NootValue val) {
    if (val.type == Bool) return val.data.Bool;
    else return val.type != Nil;
}

NootList noot_list_push(NootList old, NootValue val) {
    if (!old.shared) old.shared = (NootListShared*)tgc_calloc(&noot_gc, 1, sizeof(NootListShared));
    NootListShared* shared = old.shared;
    // Increase capacity if necessary
    if (old.len == shared->capacity) {
        size_t new_capacity = shared->capacity == 0 ? 1 : shared->capacity * 2;
        shared->buffer = (NootListEntry*)tgc_realloc(&noot_gc, shared->buffer, new_capacity * sizeof(NootListEntry));
        shared->capacity = new_capacity;
    }
    // Increment next id
    int new_id = ++shared->next_id;
    // Create the new entry
    size_t i = old.len;
    NootListEntry* buffer = shared->buffer;
    // Keep track of old entry
    NootListEntry old_entry = buffer[i];
    // Replace old old entry fields with new entry fields
    buffer[i].id = new_id;
    buffer[i].val = val;
    buffer[i].next = (NootListEntry*)tgc_alloc(&noot_gc, sizeof(NootListEntry));
    *buffer[i].next = old_entry;
    // Create new list
    NootList list = {
        .id = new_id,
        .len = old.len + 1,
        .shared = shared,
    };
    return list;
}

NootList noot_list_replace(NootList old, size_t index, NootValue val) {
    // Push if index == len
    if (index == old.len) return noot_list_push(old, val);
    if (index > old.len); // panic
    NootListShared* shared = old.shared;
    // Insert
    // Increment next id
    int new_id = ++shared->next_id;
    // Create the new entry
    size_t i = index;
    NootListEntry* buffer = shared->buffer;
    // Keep track of old entry
    NootListEntry old_entry = buffer[i];
    // Replace old old entry fields with new entry fields
    buffer[i].id = new_id;
    buffer[i].val = val;
    buffer[i].next = (NootListEntry*)tgc_alloc(&noot_gc, sizeof(NootListEntry));
    *buffer[i].next = old_entry;
    // Create new list
    NootList list = {
        .id = new_id,
        .len = old.len,
        .shared = shared,
    };
    return list;
}

NootValue noot_list_last(NootList list) {
    return noot_list_get(list, list.len - 1);
}

NootList noot_list_pop(NootList old, NootValue* popped) {
    // Set popped
    *popped = noot_list_get(old, old.len - 1);
    if (old.len == 0) return old;
    NootListShared* shared = old.shared;
    // Increment next id
    int new_id = ++shared->next_id;
    // Create new list
    NootList list = {
        .id = new_id,
        .len = old.len - 1,
        .shared = shared,
    };
    return list;
}

NootValue noot_insert(NootValue con, NootValue key, NootValue val) {
    switch (con.type) {
    case List:;
        int index;
        switch (key.type) {
        case Int: index = key.data.Int; break;
        case Real: index = key.data.Real; break;
        default: break; // panic
        }
        return new_list(noot_list_replace(con.data.List, index, val));
    default: return NOOT_NIL;
    }
}

NootValue noot_get(NootValue con, NootValue key) {
    switch (con.type) {
    case List:
        switch (key.type) {
        case Int: return noot_list_get(con.data.List, key.data.Int);
        case Real: return noot_list_get(con.data.List, key.data.Real);
        default: return NOOT_NIL;
        }
    default: return NOOT_NIL;
    }
}

NootValue noot_len(uint8_t count, NootValue* args) {
    switch (args[0].type) {
    case String: return new_int(args[0].data.String.len);
    case List: return new_int(args[0].data.List.len);
    default: return NOOT_NIL;
    }
}

#endif
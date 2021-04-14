#ifndef NOOT_VALUE_H
#define NOOT_VALUE_H

#include <math.h>
#include <stdio.h>
#include <stdbool.h>
#include "utf8.h"
#include "tgc.h"

// The garbage collector
static tgc_t noot_gc;

static char** noot_call_stack = NULL;
static size_t noot_call_stack_len = 0;
static size_t noot_call_stack_capacity = 0;

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

void noot_panic(char* message) {
    printf("Noot panicked!\n");
    printf("%s\n", message);
    for (int i = noot_call_stack_len - 1; i >= 0; i--)
        printf("in %s\n", noot_call_stack[i]);
    exit(1);
}

// The type of a byte
typedef unsigned char byte;

// The Noot string representation
typedef struct NootString {
    byte* s;
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
    Table,
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
    "table",
    "function",
    "function",
    "error",
};

// Foward declarations

typedef struct NootValue NootValue;
typedef struct NootListEntry NootListEntry;
typedef struct NootTableEntry NootTableEntry;

// The function pointer type for regular Noot functions
typedef NootValue(*NootFn)(uint8_t, NootValue* args);
// The function pointer type for Noot closures
typedef NootValue(*NootClosureFn)(uint8_t, NootValue* args, NootValue* captures);

// A Noot closure
typedef struct NootClosure {
    NootValue* captures;
    NootClosureFn f;
} NootClosure;

// The shared part of a Noot List
typedef struct NootListShared {
    size_t capacity;
    NootListEntry* buffer;
    int next_id;
} NootListShared;

// The shared part of a Noot table
typedef struct NootTableShared {
    size_t capacity;
    NootTableEntry** buffer;
    int next_id;
} NootTableShared;

// A Noot list
typedef struct NootList {
    int id;
    size_t len;
    NootListShared* shared;
} NootList;

// A Noot table
typedef struct NootTable {
    int id;
    size_t len;
    NootTableShared* shared;
} NootTable;

// The data of a Noot value
typedef union NootData {
    bool Bool;
    unsigned long Nat;
    long Int;
    double Real;
    NootString String;
    NootFn Function;
    NootClosure Closure;
    NootList List;
    NootTable Table;
    struct NootValue* Error;
} NootData;

// A noot value with a type and data
struct NootValue {
    NootType type;
    NootData data;
};

// The entry of Noot lists
typedef struct NootListEntry {
    int id;
    NootValue val;
    struct NootListEntry* next;
} NootListEntry;

// The entry of Noot tables
typedef struct NootTableEntry {
    NootValue key;
    NootListEntry vals;
    struct NootTableEntry* next;
} NootTableEntry;

// The nil Noot value
const NootValue NOOT_NIL = { .type = Nil };

// An empty Noot list
const NootValue NOOT_EMPTY_LIST = {
    .type = List,
    .data = {.List = {.id = 0, .len = 0, .shared = NULL }},
};

// An empty Noot table
const NootValue NOOT_EMPTY_TABLE = {
    .type = Table,
    .data = {.Table = {.id = 0, .len = 0, .shared = NULL }},
};

#define new_bool(b) (NootValue) { .type = Bool, .data = { .Bool = b } }
#define new_int(i) (NootValue) { .type = Int, .data = { .Int = i } }
#define new_real(i) (NootValue) { .type = Real, .data = { .Real = i } }
#define new_function(f) (NootValue) { .type = Function, .data = { .Function = f } }
#define new_closure(f, captures) (NootValue) { .type = Closure, .data = { .Closure = { .f = f, .captures = captures } } }
#define new_list(list) (NootValue) { .type = List, .data = { .List = list } }
#define new_table(table) (NootValue) { .type = Table, .data = { .Table = table } }
#define new_noot_string(s, len) (NootString) { .s = s, .len = len }
#define new_string(s, len) (NootValue) { .type = String, .data = { .String = new_noot_string(s, len) } }

void noot_binary_type_panic(char* message, NootType a, NootType b) {
    char str[256];
    sprintf(str, "Attempted to add incompatible types %s and %s", noot_type_names[a], noot_type_names[b]);
    noot_panic(str);
}

// Create a new Noot error from a value
NootValue noot_error(uint8_t count, NootValue* inner) {
    NootValue val = { .type = Error };
    val.data.Error = (NootValue*)tgc_alloc(&noot_gc, sizeof(NootValue));
    *val.data.Error = inner ? *inner : NOOT_NIL;
    return val;
}

// Call a Noot function or closure value
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
    case Real:;
        byte str[50];
        sprintf(str, "%f", val.data.Real);
        int i = strlen(str);
        if (i == 0) break;
        i -= 1;
        while (str[i] == '0' || str[i] == '.') i--;
        printf("%*.*s", i + 1, i + 1, str);
        break;
    case String:;
        size_t len = val.data.String.len;
        printf("%*.*s", len, len, val.data.String.s);
        break;
    case List:;
        NootList list = val.data.List;
        printf("[");
        for (int i = 0; i < list.len; i++) {
            if (i > 0) printf(" ");
            NootValue item = noot_list_get(list, i);
            noot_print(1, &item);
        }
        printf("]");
        break;
    case Table:;
        NootTable table = val.data.Table;
        printf("{");
        if (table.shared) {
            int i = 0;
            for (int j = 0; j < table.shared->capacity; j++) {
                NootTableEntry* key_entry = table.shared->buffer[j];
                while (key_entry) {
                    NootListEntry* val_entry = &key_entry->vals;
                    while (val_entry) {
                        if (val_entry->id <= table.id) {
                            if (i > 0) printf(" ");
                            noot_print(1, &key_entry->key);
                            printf(":");
                            noot_print(1, &val_entry->val);
                            i++;
                            break;
                        }
                        val_entry = val_entry->next;
                    }
                    key_entry = key_entry->next;
                }
            }
        }
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
    noot_binary_type_panic("Attempted to add incompatible types %s and %s", a.type, b.type);
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
    case Closure: return b.type == Closure && a.data.Closure.f == b.data.Closure.f;
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
    case Closure: return b.type == Closure && a.data.Closure.f < b.data.Closure.f;
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
    case Closure: return b.type == Closure && a.data.Closure.f > b.data.Closure.f;
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
    return (val.type == Bool) * val.data.Bool + (val.type != Bool) * (val.type != Nil);
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

NootList noot_list_set(NootList old, size_t index, NootValue val) {
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

typedef struct HashState {
    uint64_t hash;
    size_t i;
} HashState;

void bad_hash(HashState* state, byte* bytes, size_t count) {
    for (size_t i = 0; i < count; i++, state->i++) {
        state->hash ^= bytes[i] << (state->i % 57);
    }
}

const HashState DEFAULT_HASH_STATE = {
    .hash = 0,
    .i = 0,
};

void noot_bad_hash(HashState* state, NootValue val) {
    bad_hash(state, (byte*)&val.type, sizeof(NootType));
    switch (val.type) {
    case Nil: break;
    case Bool: bad_hash(state, (byte*)&val.data.Bool, sizeof(bool)); break;
    case Int: bad_hash(state, (byte*)&val.data.Int, sizeof(long)); break;
    case Real: bad_hash(state, (byte*)&val.data.Real, sizeof(double)); break;
    case String: bad_hash(state, val.data.String.s, val.data.String.len); break;
    case List:
        bad_hash(state, (byte*)&val.data.List.len, sizeof(size_t));
        for (size_t i = 0; i < val.data.List.len; i++)
            noot_bad_hash(state, noot_list_get(val.data.List, i));
        break;
    case Function: bad_hash(state, (byte*)val.data.Function, sizeof(NootFn)); break;
    case Closure: bad_hash(state, (byte*)val.data.Closure.f, sizeof(NootClosureFn)); break;
    case Error: noot_bad_hash(state, *val.data.Error); break;
    }
}

NootValue noot_table_get(NootTable table, NootValue key) {
    // Hash the key
    HashState state = DEFAULT_HASH_STATE;
    noot_bad_hash(&state, key);
    uint64_t hash = state.hash;
    size_t index = hash % table.shared->capacity;
    // Look for an existing matching key and value entry
    NootTableEntry* key_entry = table.shared->buffer[index];
    while (key_entry) {
        if (noot_eq_impl(key_entry->key, key)) {
            NootListEntry* val_entry = &key_entry->vals;
            while (val_entry) {
                if (val_entry->id <= table.id)
                    return val_entry->val;
                val_entry = val_entry->next;
            }
            break;
        }
        key_entry = key_entry->next;
    }
    // Return nil if the entry was not found
    return NOOT_NIL;
}

// Rehash a Noot table to have the given capacity
void noot_table_rehash(NootTable* table, size_t capacity) {
    // Allocate new entries
    NootTableEntry** new_key_entries = (NootTableEntry**)tgc_calloc(&noot_gc, capacity, capacity * sizeof(NootTableEntry*));
    // For each of the old buckets ...
    for (size_t old_index = 0; old_index < table->shared->capacity; old_index++) {
        // For each of the entries in the old bucket ...
        NootTableEntry* curr_old_key_entry = table->shared->buffer[old_index];
        while (curr_old_key_entry) {
            // Allocate a new entry
            NootTableEntry* new_key_entry = (NootTableEntry*)tgc_alloc(&noot_gc, sizeof(NootTableEntry));
            // Hash the key to determine a new index
            HashState hash_state = DEFAULT_HASH_STATE;
            noot_bad_hash(&hash_state, curr_old_key_entry->key);
            size_t new_index = hash_state.hash % capacity;
            // Initialize the value entry
            NootListEntry new_val_entry = {
                .id = 0,
                .next = NULL,
                .val = curr_old_key_entry->vals.val,
            };
            new_key_entry->vals = new_val_entry;
            // Initialize the new key entry
            new_key_entry->key = curr_old_key_entry->key;
            new_key_entry->next = new_key_entries[new_index];
            new_key_entries[new_index] = new_key_entry;
            // Insert the new entry
            curr_old_key_entry = curr_old_key_entry->next;
        }
    }
    // Create a new shared
    NootTableShared* shared = (NootTableShared*)tgc_calloc(&noot_gc, 1, sizeof(NootTableShared));
    // Initialize the new shared
    shared->capacity = capacity;
    shared->next_id = 0;
    shared->buffer = new_key_entries;
    // Update the table
    table->id = 0;
    table->shared = shared;
}

NootTable noot_table_insert(NootTable old, NootValue key, NootValue val) {
    // Create new table by copying old
    NootTable new = old;
    // Hash the key
    HashState state = DEFAULT_HASH_STATE;
    noot_bad_hash(&state, key);
    uint64_t hash = state.hash;
    // Check the current capacity
    if (new.shared) {
        // Look for an existing matching key
        size_t index = hash % new.shared->capacity;
        NootTableEntry* key_entry = new.shared->buffer[index];
        NootListEntry* val_entry = NULL;
        while (key_entry) {
            if (noot_eq_impl(key_entry->key, key)) {
                val_entry = &key_entry->vals;
                while (val_entry) {
                    if (val_entry->id <= old.id) {
                        goto end_search;
                    }
                    val_entry = val_entry->next;
                }
                break;
            }
            key_entry = key_entry->next;
        }
    end_search:;
        // Increment the id
        int new_id = ++new.shared->next_id;
        new.id = new_id;
        // Create the new value entry
        NootListEntry new_val_entry = {
            .id = new_id,
            .val = val,
            .next = NULL,
        };
        // Insert 
        if (key_entry) {
            // Allocate memory for the child value entry
            NootListEntry* child_val_entry = (NootListEntry*)tgc_alloc(&noot_gc, sizeof(NootListEntry));
            *child_val_entry = key_entry->vals;
            // Put the new val entry on top
            new_val_entry.next = child_val_entry;
            key_entry->vals = new_val_entry;
        }
        else {
            // Create the new key entry
            NootTableEntry* new_key_entry = (NootTableEntry*)tgc_calloc(&noot_gc, 1, sizeof(NootTableEntry));
            new_key_entry->key = key;
            new_key_entry->vals = new_val_entry;
            // Insert the entry
            new_key_entry->next = new.shared->buffer[index];
            new.shared->buffer[index] = new_key_entry;
        }
        // Rehash if necessary
        if (!val_entry) {
            new.len++;
            if ((float)new.len / (float)new.shared->capacity > 0.7) {
                size_t new_capacity = (float)new.len / 0.3;
                noot_table_rehash(&new, new_capacity);
            }
        }
        return new;
    }
    else {
        // Allocate the initial table
        const size_t INITIAL_CAPACITY = 4;
        NootTableShared* shared = (NootTableShared*)tgc_calloc(&noot_gc, 1, sizeof(NootTableShared));
        shared->capacity = INITIAL_CAPACITY;
        shared->buffer = (NootTableEntry**)tgc_calloc(&noot_gc, INITIAL_CAPACITY, INITIAL_CAPACITY * sizeof(NootTableEntry*));
        shared->next_id = 1;
        // Create the new value entry
        NootListEntry new_val_entry = {
            .id = 1,
            .val = val,
            .next = NULL,
        };
        // Create the new key entry
        NootTableEntry* new_key_entry = (NootTableEntry*)tgc_calloc(&noot_gc, 1, sizeof(NootTableEntry));
        new_key_entry->key = key;
        new_key_entry->vals = new_val_entry;
        // Insert the entry
        size_t index = hash % INITIAL_CAPACITY;
        shared->buffer[index] = new_key_entry;
        new.shared = shared;
        new.len = 1;
        new.id = 1;
        return new;
    }
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
        return new_list(noot_list_set(con.data.List, index, val));
    case Table:
        return new_table(noot_table_insert(con.data.Table, key, val));
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
    case Table: return noot_table_get(con.data.Table, key);
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
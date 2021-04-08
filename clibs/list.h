#include "noot.h"

typedef struct NootListEntry {
    int id;
    NootValue val;
    struct NootListEntry* next;
} NootListEntry;

typedef struct NootListShared {
    size_t capacity;
    NootListEntry** buffer;
    int next_id;
} NootListShared;

typedef struct NootList {
    int id;
    size_t len;
    NootListShared* shared;
} NootList;

NootList new_list_inner() {
    NootListShared* shared = (NootListShared*)tgc_calloc(&noot_gc, 1, sizeof(NootListShared));
    NootList list = {
        .id = 0,
        .len = 0,
        .shared = shared,
    };
    return list;
}

NootList noot_list_push(NootList old, NootValue val) {
    // Increase capacity if necessary
    NootListShared* shared = old.shared;
    if (old.len == shared->capacity) {
        size_t new_capacity = shared->capacity == 0 ? 1 : shared->capacity * 2;
        shared->buffer = (NootListEntry**)tgc_realloc(&noot_gc, shared->buffer, new_capacity * sizeof(NootListEntry*));
    }
    // Increment next id
    int new_id = ++shared->next_id;
    // Create the new entry
    NootListEntry* old_entry = shared->buffer[old.len];
    NootListEntry* new_entry = (NootListEntry*)tgc_alloc(&noot_gc, sizeof(NootListEntry));
    new_entry->id = new_id;
    new_entry->val = val;
    new_entry->next = shared->buffer[old.len];
    // Insert the new entry
    shared->buffer[old.len] = new_entry;
    // Create new list
    NootList list = {
        .id = new_id,
        .len = old.len + 1,
        .shared = shared,
    };
    return list;
}

NootValue noot_list_get(NootList list, int i) {
    if (i < 0 || i >= list.len) return NOOT_NIL;
    NootListEntry** buffer = list.shared->buffer;
    NootListEntry* entry = buffer[i];
    while (entry->id > list.id) entry = entry->next;
    return entry->val;
}
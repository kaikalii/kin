#ifndef NOOT_VALUE_H
#define NOOT_VALUE_H

#include "utf8.h"

typedef void* NootString;

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

typedef union NootData {
    unsigned char Bool;
    unsigned long Nat;
    long Int;
    double Real;
    NootString String;
    struct NootValue* Error;
} NootData;

typedef struct NootValue {
    NootType type;
    NootData data;
} NootValue;

#define new_nat(i) { .type = Nat, .data = { .Nat = i } }
#define new_int(i) { .type = Int, .data = { .Int = i } }
#define new_real(i) { .type = Real, .data = { .Real = i } }

void print(NootValue val) {
    switch (val.type) {
        case Nil: break;
        case Nat: printf("%d", val.data.Nat); break;
        case Int: printf("%d", val.data.Int); break;
        case Real: printf("%f", val.data.Real); break;
    }
}

#endif
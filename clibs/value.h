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

typedef union NootData {
    unsigned char Bool;
    unsigned long Nat;
    long Int;
    double Real;
    NootString String;
    NootValue Error;
} NootData;

typedef struct NootValue {
    NootType type;
    NootData data;
} NootValue;

#define new_nat(i) { .type = Nat, .data = { .Nat = i } }

#endif
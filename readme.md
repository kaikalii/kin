# Introduction

Noot is a minimalist, dynamically-typed, pure-functional programming language without a garbage collector.

Noot is designed to be easy to learn, read, write, and even implement.

# Overview

## Hello World

Here is a simple Noot hello world program:

```
println "Hello World!"
```

## Comments

Single-line comments are denoted by `--`

Multi-line comments are surrounded by `'''`s

```
-- This is a single-line comment

'''
This is a
multi-line comment
'''
```

## Values

Values are bound with `=`

```
a = 5
b = true
c = "wow"
```

## Types

Noot is a dynamically-typed language.

Each value can have one of 8 types:

### **nil**

The basic type representing nothing

```
x = nil
```

### **bool**

`true` or `false`

```
x = true
y = false
```

### **int** and **real**

Basic integer and floating-point number values.

All basic math operations can use `int`s and `real`s interchangably.

```
a = 5 -- int
b = -12 -- int
c = 2.3 -- real

d = a + b -- int
e = a + c -- real
```

### **string**

An immutable buffer of characters

```
s = "Neato!"
hello_world = "👋🏼🌎"
```

### **list**

An immutable singly-linked list of values

```
xs = [1 2 3]

-- Lists can be composed and decomposed with the `:` operator

head:tail = xs -- head = 1, tail = [2 3]

xs = 4:xs -- [4 1 2 3]
```

### **tree**

An immutable tree node with an inner value as well as left and right child values.

```
a = {1 2 3} -- inner = 2, left = 1, right = 3

b = {a 4 {5 6 7}}
'''
b = 
     4
   /   \
  2     6
 / \   / \
1   3 5   7
'''

{_ _ right_tree} = b -- right_tree = {5 6 7}
```

### **function**

A function with an arbitrary number of parameters.

Functions are called without `()` except around arguments when necessary.

Function application binds tighter than all other operators.

Functions with multiple expressions must have a space after the `=` and end with `end`.

Functions return the value of their last expression. The is no `return` keyword.

```
dist x1 y1 x2 y2 = pow (pow (x1 - x2) 2 + pow (y1 - y2) 2) 0.5

dbg x =
    println x
    x
end
```

Anonymous functions are created with `|`. Anonymous functions with multiple or zero arguments require a leading `|`.

```
-- These functions are identical
add1 a b = a + b
add2 = |a b| a + b

double_and_print = x| print (2 * x)
```

Functions can be chained with the `,` operator. This passes the result of the function on the left as the first argument of the function on the right.

```
-- Get the suare of all even numbers in a list
squared_evens list = list, filter (x| x % 2 == 0), map (x| x * x)
```

Chained function calls can go onto the next line for readability

```
-- Print all multiples of 3 between 0 and 100
range 0 100,
filter (x| x % 3 == 0),
println
```

### **error**

An value that represents a failue in some process. It contains an inner value.

```
e = err "Something went wrong!"

message = get_err e
```

## Operators

### Arithmetic Operators

Noot has 5 binary arthimetic operators `+`, `-`, `*`, `/`, and `%` and 1 unary arithmetic operator `-`.

These operators only work on int and real values.

```
1 + 2 -- 3
2 - 1 -- 1
2 * 3 -- 6
12 / 3 -- 4
12 / 5 -- 2
12.0 / 5 -- 2.4
13 % 3 -- 1
13 % 3.5 -- 2.5
-(1 - 2) -- 1
```

### Comparison Operators


Noot has 6 binary comparison operators `==`, `!=`, `<`, `>`, `<=`, and `>=` and an inversion operator `not`.

`==`, `!=`, and `not` work on values of all types. Inequality operators only work on int and real types.

```
1 < 2 -- true
2 < 2 -- false,
2 <= 2 -- true
1 == 1 -- true
1 == "hi" -- false
1 < "hi" -- runtime panic
```

## Control Flow

Noot has only 2 control-flow operators, `or` and `and`, which also function as comparison operators.

Each Noot value has a "truthiness". For the purposes of control flow, `nil`, `false`, and all error values are considered false. All other values are considered true.

`and` binds tighter than `or`.

The `or` operator first evaluates the expression on the left. If the resulting value is truthy, the `or` expression evaluates to that value. Otherwise, the right expression is evaluated, and the `or`expression evaluates to that.

```
false or false -- false
true or false -- true
true or true -- true
1 or 2 -- 1
nil or 3 -- 3
0 < 5 or "hi" -- true
0 > 5 or 27 -- 27
```

The `and` operator is the opposite of `or` operator. It evaluates to the left expression if it is false and to the right expression otherwise

```
false and false -- false
true and false -- false
true and true -- true
1 and 2 -- 2
nil and 3 -- nil
0 < 5 and "hi" -- "hi"
0 > 5 and 27 -- false
```

The `and` and `or` operators allow you to create if-else control flow.

```
foo x = x < 10 and println x or println "too big!"
```

For better readability, the above function can be written as

```
foo x = x < 10
    and println x
    or println "too big!"
```

`and` allows for nil and error checking

```
-- `open_file` can fail, so `file` might be an error value
file = open_file("my_file.txt") 

-- Only do something with `file` if it was successfully opened
file and do_something_wth_file(file)
```

`or` allows a default value to be provided

```
-- `parse_json` can fail, so `my_value` might be an error value
my_value = parse_json("my_file.json") 

-- Provide a default value if `my_value` is an error
my_value = my_value or (1 2 3)
```
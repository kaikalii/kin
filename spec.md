# Types

Kin is a dynamically-typed language. Types are evaluated at runtime to determine operations

The possible types for a Kin value are as follows:

- Nil
    - Equivalent to null in other languages
    - Has no value
- Bool
    - `true` or `false`
- Int
    - 64-bit signed integer
- Real
    - 64-bit double-precision floating-point point number
- String
    - An immutable text buffer
- List
    - Holds exactly two values
    - Can be chained together to hold more
- Tree
    - Holds exactly three values
    - Can be chained together to hold more
- Function
    - A function with any number of arguments
- Error
    - Indicates a failure
    - Holds an internal value

# Call expressions

Values of most Kin types can be called as if they are functions. The value at the beginning of the call expression is used as the caller.

The rules for calls are as follows:

- A function caller simply calls the function
- Bool, Int, Real, and String values create a chained list with themselves and all arguments
- Nil values create a list without themselves
- TODO: Lists and trees

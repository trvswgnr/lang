The "scampi" language is a simple dynamically typed language that takes
inspiration from Rust, Python, and JavaScript.

Here is a the proposed syntax:

```rust
// single-line comments are denoted by a double slash
/*
    multi-line comments are denoted by a slash and asterisk
    and end with an asterisk and slash
*/
// VARIABLES
// variables are declared without "var" or "let"
some_int=69 // integers are 64-bit
some_string="foo" // strings are UTF-8
some_arr=[1,2,3,] // arrays are dynamically sized

// ARRAYS
// arrays are zero-indexed
some_arr[0]; // 1

// arrays are mutable
some_arr[0]=4; // [4,2,3]

// arrays are homogeneous (all elements must be the same type)
some_arr[0]="foo"; // error

// arrays are dynamically sized
some_arr[4]=5; // [4,2,3,0,5]

// CLOSURES
{
    x=5; // closures have their own scope
}

some_function_def(x) { // functions are first-class
    x; // return values are implicit
} // functions are closures

// LOOPS
i=0; // variables are mutable
loop { // infinite loop
    if x>9 { // if statements are expressions
        break; // break is a keyword
    }
    i = i + 1; // increment i
}
```
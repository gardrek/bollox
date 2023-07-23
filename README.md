`bollox` is my attempt at a Lox interpreter, written in Rust. It is based on the first interpreter described by the book Crafting Interpreters. That interpreter was written in Java, so a good deal of changes have been made to the structure to make it more "Rusty".

# Major differences compared to standard Lox #

`if` and `while` statements have been made more Rust-like; you have to use brackets around the body, even if it's just one expression, but you don't have to use parenthesis around the condition.

Closures make a copy of the environment rather than taking reference. Practically this is equivalent to captures being by-value instead of by-reference. Assignment no longer mutates the outer state. In order to mutate the outer state, you have to use an instance.

There is no static analysis pass, so some things that would have been compile-time errors are now runtime errors. For instance, a class still cannot inherit from itself, simply because the variable is not yet defined in the environment in which the superclass is first looked up.

# Minor differences #

Some things that are errors in Lox are allowed by `bollox`, such as `return` from outside any function, or redefining a variable that is the parameter of the current function. These two are intentional changes. I plan to add an include system where returning from the main file gives the return value of the `include` call. And it just didn't make sense to me that redefining a variable is *sometimes* an error.

Many error messages are quite different. Errors are not currently in the best shape.


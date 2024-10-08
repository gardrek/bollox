`bollox` is a Lox interpreter, written in Rust, with various feature extensions. It is based on the first interpreter described by the book Crafting Interpreters. That interpreter was written in Java, so a good deal of changes have been made to the structure to make it more "Rusty".



# Usage #

`cargo run` - for interactive REPL.

`cargo run --release -- path/to/script.lox` - to run a script.

`cargo run --release -- --compatibility path/to/script.lox` - for better compatibility with standard Lox. Makes variable declarations in the "global" scope of a file default to being global variables. Same for function declarations and class declarations.



# Differences compared to standard Lox #

One important thing to note: bollox is not fully garbage collected. It uses reference counting with *no* cycle detection, so if you create a reference cycle, that memory can and will be leaked.

Strings support many of the same escape sequences as Rust:
- `\n` - new line
- `\r` - carriage return
- `\t` - tab
- `\"` - literal double quote
- `\'` - literal single quote
- `\\` - literal backslash
- `\0` - nul character

Several new keywords are included. These keywords are disabled in compatibility mode, to allow for maximum compatibility. These are the added keywords: `break`, `const`, `global`, `in`, `local`, `mut`, and `switch`.

A file can return from the top level. A call to `require` returns whatever the file returned.

Warnings for unused variables, except if the variable starts with `_`. Also warning for initializing a variable and then immediately overwriting it.

Hexadecimal integers as in `0xf3`

All variable declarations are local by default, but can be annotated with the `global` decorator as in `global var x` to make them global. This can be applied to class and function declarations, too, which also default to being local. In compatibility mode, variable, function, and class declarations in the top-level scope of a file default to being global.

All variable declarations are reassignable by default but you can add the `const` decorator as inf `const var x` or `global const var x` to make it an warning to reassign. This can be done for class and function declarations as well. In addition, function parameters are declared const as well.

`if` and `while` statements are able to be more Rust-like; you either have to use brackets around the body, even if it's just one expression, *or* use parenthesis around the condition.

Remainder operator `a % b`

Combined operator syntax. e.g. `a += b`

I've added an iterator syntax to `for` loops. This is essentially syntactic sugar that breaks down like this:
```js
for i in [iter] { [body] }
==>
{
    var f = [iter];
    var i = true;
    while i {
        i = f();
        if i {
            [body]
        }
    }
}
```

Switch statement syntax: It's just sugar for a chain of `if ... else` and it behaves much the same. Each case works just like an if statement without the if, and the default case is handled by `else`.
```js
switch input {
    "dup" {
        var a = stack.pop();
        stack.push(a);
        stack.push(a);
    }
    "pop" {
        stack.pop();
    }
    "add", "+" {
        var b = stack.pop();
        var a = stack.pop();
        stack.push(a + b);
    }
    else {
        print_stack(stack);
    }
}

==>

if input == "dup" {
    var a = stack.pop();
    stack.push(a);
    stack.push(a);
} else if input == "pop" {
    stack.pop();
} else if input == "add" or input == "+" {
    var b = stack.pop();
    var a = stack.pop();
    stack.push(a + b);
} else {
    print_stack(stack);
}
```

Basic array support. Currently arrays can only be grown or shrunk using push and pop methods.
```js
var a = [-1, 0, 10 + 40, 10000, 0.4];

print a[1]; // prints 0;

a[1] = 5;

var x = [0; 100]; // one hundred element array filled with zeroes
```

Indexing an array or string with a negative number or past the end returns nil. Indexing with a fractional number uses the floor of the number. Indexing with NaN or Inf returns nil.


## Book Challenge Extensions ##

Supports:
- block comments `/* comment */` with nesting
- anonymous functions with the syntax `fun(...) { ... }`.
- break statements with simple `break;` syntax.
- associated functions on classes with `class func_name(...) {...}` and `ClassName.func_name(...)`
- warnings for unused variables




## Minor differences ##

Many error messages are quite different. Errors are not currently in the best shape.



## Included Library Functions ##

- `clock()` - Returns the number of seconds since unix epoch.

- `read()` - Reads a single line from stdin, trims the whitespace (including newlines) from the start and end, and returns it.

- `to_string(value)` - Returns a string representing whatever value is passed in.

- `to_number(value)` - If the value is a number, returns it. if the value is a string which it can parse as a float, it returns the parsed number. otherwise, returns `nil`. Numbers are parsed according to [this Rust standard library function](https://doc.rust-lang.org/stable/std/primitive.f64.html#method.from_str).

- `require(filename)` - Run a script and return whatever that script returns (using a return statement in the top level)

- `getc()` - Read a single byte from stdin and return it as an integer.

- `putc(ch)` - Write a single byte to stdout.

- `chr(ch)` - Convert given character code number to a single-character string.

- `exit(status)` - Exit with given status code.

- `print_error(message)` - Print message string on stderr.

- `typeof(obj)` - Returns a string describing the built-in type of any object `obj`. Returns one of "Nil", "Boolean", "Number", "String", "NativeFunction", "Function", "Class", "Instance", or "Array".



### Methods ###

In addition to the above global functions, the following methods are defined on the built-in types:

Arrays:
- `push(obj)` - Push `obj` onto the end of the array.
- `pop()` - Pop an object off of the end of the array and returns it, or return nil if the array is empty.
- `len()` - Returns the length of the array.
- `clone()` - Returns a new array that is a shallow copy of this array.
- `bytes_to_str()` - If the array is entirely numbers from 0 to 255 (when floored) and is valid UTF-8, returns it as a string. Otherwise returns nil.

Strings:
- `len()` - Returns the length of the string.
- `to_number()` - Returns the string parsed as a number, or `nil` if it can't be parsed as such. String is parsed according to [this Rust standard library function](https://doc.rust-lang.org/stable/std/primitive.f64.html#method.from_str).

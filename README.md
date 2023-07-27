`bollox` is my attempt at a Lox interpreter, written in Rust. It is based on the first interpreter described by the book Crafting Interpreters. That interpreter was written in Java, so a good deal of changes have been made to the structure to make it more "Rusty".



# Usage #

`cargo run` - for interactive REPL.

`cargo run --release -- path/to/script.lox` - to run a script.

`cargo run --release -- --compatibility path/to/script.lox` - for better compatibility with standard Lox. Switches back to C-style `if` and `while` statements instead of Rust-style. In the future this may also change how closures capture their environment.



# Differences compared to standard Lox #

`if` and `while` statements are able to be more Rust-like; you either have to use brackets around the body, even if it's just one expression, *or* use parenthesis around the condition. For compatibility reasons, if the condition begins with parenthesis, you may have to put extra parenthesis around it.

Except in compatibility mode, closures make a copy of the environment rather than taking reference. This eliminates the ability to use variables that haven't been declared yet when the function is declared.

There is no static analysis pass, so some things that would have been compile-time errors are now runtime errors. For instance, a class still cannot inherit from itself, simply because the variable is not yet defined in the environment in which the superclass is first looked up.

Modulo operator `a % b`

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
    "pop" => {
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


## Book Challenge Extensions ##

Supports:
- anonymous functions with the syntax `fun(...) { ... }`.
- break statements with simple `break;` syntax.
- associated functions on classes with `class func_name(...) {...}` and `ClassName.func_name(...)`




## Minor differences ##

Some things that are errors in Lox are allowed by `bollox`, such as `return` from outside any function, or redefining a variable that is the parameter of the current function. These two are intentional changes. `require` allows you to use the returned value from the top level of a file. And it just didn't make sense to me that redefining a variable is *sometimes* an error.

Many error messages are quite different. Errors are not currently in the best shape.



## Included Library Functions ##

`clock()` - Returns the number of seconds since unix epoch.

`read()` - Reads a single line from stdin, trims the whitespace (including newlines) from the start and end, and returns it.

`to_string(value)` - Returns a string representing whatever value is passed in.

`to_number(value)` - If the value is a number, returns it. if the value is a string which it can parse as a float, it returns the parsed number. otherwise, returns `nil`. Numbers are parsed according to [this Rust standard library function](https://doc.rust-lang.org/stable/std/primitive.f64.html#method.from_str).

`require(filename)` - Run a script and return whatever that script returns (using a return statement in the top level)

`getc()` - Read a single byte from stdin and return it as an integer.

`chr(ch)` - Convert given character code number to a single-character string.

`exit(status)` - Exit with given status code.

`print_error(message)` - Print message string on stderr.

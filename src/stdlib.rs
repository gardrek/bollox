use std::cell::RefCell;
use std::rc::Rc;

use crate::interpreter::ControlFlow;
use crate::interpreter::Environment;
use crate::interpreter::Interpreter;
use crate::interpreter::NativeClass;
use crate::interpreter::RuntimeError;
use crate::object::Object;
use crate::source::SourceLocation;

use crate::INTERNER;

pub fn init_global_environment(env: &mut Environment) {
    env.define_native_function(
        "clock",
        0,
        |_interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            use std::time::{SystemTime, UNIX_EPOCH};

            Ok(Object::Number(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("Time went backwards")
                    .as_millis() as f64
                    / 1000.0,
            ))
        },
    );

    env.define_native_function(
        "read",
        0,
        |_interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            use std::io::{self, BufRead};

            let mut line = String::new();
            let stdin = io::stdin();
            stdin.lock().read_line(&mut line).unwrap();
            let line = line.trim().to_string();

            Ok(Object::String(crate::object::StringKind::Dynamic(line)))
        },
    );

    env.define_native_function(
        "to_string",
        1,
        |_interpreter: &mut Interpreter, args: Vec<Object>| -> Result<Object, ControlFlow> {
            let obj = &args[0];

            let s = format!("{}", obj);

            Ok(Object::dynamic_string(s))
        },
    );

    env.define_native_function(
        "to_number",
        1,
        |_interpreter: &mut Interpreter, args: Vec<Object>| -> Result<Object, ControlFlow> {
            let obj = &args[0];

            Ok(match obj {
                Object::Number(_) => obj.clone(),
                Object::String(s) => match s.to_string().parse::<f64>() {
                    Ok(n) => Object::Number(n),
                    Err(_) => Object::Nil,
                },
                _ => Object::Nil,
            })
        },
    );

    env.define_native_function(
        "getc",
        0,
        |_interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            use std::io::Read;
            match std::io::stdin().bytes().next() {
                Some(r) => match r {
                    Ok(n) => Ok(Object::Number(n as f64)),
                    Err(_e) => Ok(Object::Number(-1.0)),
                },
                None => Ok(Object::Number(-1.0)),
            }
        },
    );

    env.define_native_function(
        "putc",
        1,
        |_interpreter: &mut Interpreter, args: Vec<Object>| -> Result<Object, ControlFlow> {
            let obj = &args[0];

            use std::io::Write;
            match obj {
                Object::Number(c) => {
                    if *c >= 0.0 && *c < 256.0 {
                        let c = *c as u8 as char;
                        print!("{}", c);
                        std::io::stdout().flush().unwrap();
                        Ok(Object::nil())
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        },
    );

    env.define_native_function(
        "chr",
        1,
        |_interpreter: &mut Interpreter, args: Vec<Object>| -> Result<Object, ControlFlow> {
            let obj = &args[0];

            Ok(match obj {
                Object::Number(n) => {
                    Object::dynamic_string(std::str::from_utf8(&[*n as u8]).unwrap().to_string())
                }
                _ => Object::Nil,
            })
        },
    );

    env.define_native_function(
        "exit",
        1,
        |_interpreter: &mut Interpreter, args: Vec<Object>| -> Result<Object, ControlFlow> {
            let obj = &args[0];

            match obj {
                Object::Number(n) => std::process::exit(*n as i32),
                _ => std::process::exit(0),
            }
        },
    );

    env.define_native_function(
        "print_error",
        1,
        |_interpreter: &mut Interpreter, args: Vec<Object>| -> Result<Object, ControlFlow> {
            let message = &args[0];

            eprintln!("{}", message);

            Ok(Object::Nil)
        },
    );

    env.define_native_function(
        "require",
        1,
        |_interpreter: &mut Interpreter, args: Vec<Object>| -> Result<Object, ControlFlow> {
            let filename_obj = &args[0];

            match filename_obj {
                Object::String(s) => {
                    let filename = std::path::PathBuf::from(s.to_string());

                    // does it make sense to run an include in compatibility mode?
                    // old scripts aren't even going to return anything, and global scope isn't shared (yet?)
                    /*
                    let compatibility = match filename.extension().map(|f| f.to_str()) {
                        Some(Some("lox")) => true,
                        _ => false,
                    };
                    //~ */

                    let compatibility = false;

                    let source = std::fs::read_to_string(filename).unwrap();

                    let result = crate::run_string(source.clone(), 0, compatibility);

                    match result {
                        Ok(obj) => Ok(obj.unwrap_or(Object::Nil)),
                        Err(e) => {
                            eprintln!(
                                "error on line {:?}",
                                crate::source::SourceLocation::error_line_number(&e, &source)
                            );
                            panic!()
                        }
                    }
                }
                _ => Ok(Object::Nil),
            }
        },
    );

    env.define_native_function(
        "typeof",
        1,
        |_interpreter: &mut Interpreter, args: Vec<Object>| -> Result<Object, ControlFlow> {
            let obj = &args[0];

            use Object::*;

            Ok(Object::dynamic_string(
                match obj {
                    Nil => "Nil",
                    Boolean(_) => "Bool",
                    Number(_) => "Number",
                    String(_) => "String",

                    NativeFunc(_) => "NativeFunction",
                    LoxFunc(_) => "Function",
                    Class(_) => "Class",
                    Instance(_) => "Instance",
                    Array(_) => "Array",
                }
                .to_string(),
            ))
        },
    );

    /*
    env.define_native_function("test", 0, |
        _interpreter: &mut Interpreter,
        _args: Vec<Object>,
    | -> Result<Object, ControlFlow> {
        let s = "string";

        let b = Object::dynamic_string(s.to_string()) == Object::static_string_from_str(s);

        Ok(Object::Boolean(b))
    });
    */
}

pub fn init_number_native_methods(interpreter: &mut Interpreter) {
    let class_name = {
        let mut interner = INTERNER.write().unwrap();
        interner.get_or_intern("Number")
    };

    let mut native_methods = NativeClass::default();

    // number.round()

    native_methods.add_method(
        "round",
        0,
        |interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            let this_name = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern("this")
            };

            let this = match interpreter.get_binding(&this_name) {
                Some(o) => o,
                None => {
                    return Err(RuntimeError::ice(
                        "`this` not defined for method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            };

            match this {
                Object::Number(n) => Ok(Object::Number(n.round())),
                _ => Err(RuntimeError::ice(
                    "`this` not a string for string method",
                    SourceLocation::bullshit(),
                )
                .into()),
            }
        },
    );

    // number.to_number()

    native_methods.add_method(
        "to_number",
        0,
        |interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            let this_name = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern("this")
            };

            let this = match interpreter.get_binding(&this_name) {
                Some(o) => o,
                None => {
                    return Err(RuntimeError::ice(
                        "`this` not defined for method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            };

            Ok(match this {
                Object::Number(_) => this,
                _ => {
                    return Err(RuntimeError::ice(
                        "`this` not a string for string method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            })
        },
    );

    //

    interpreter
        .native_methods
        .insert(class_name, native_methods);
}

pub fn init_string_native_methods(interpreter: &mut Interpreter) {
    let class_name = {
        let mut interner = INTERNER.write().unwrap();
        interner.get_or_intern("String")
    };

    let mut native_methods = NativeClass::default();

    // string.len()

    native_methods.add_method(
        "len",
        0,
        |interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            let this_name = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern("this")
            };

            let this = match interpreter.get_binding(&this_name) {
                Some(o) => o,
                None => {
                    return Err(RuntimeError::ice(
                        "`this` not defined for method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            };

            match this {
                Object::String(s) => Ok(Object::Number(s.to_string().len() as f64)),
                _ => Err(RuntimeError::ice(
                    "`this` not a string for string method",
                    SourceLocation::bullshit(),
                )
                .into()),
            }
        },
    );

    // string.to_number()

    native_methods.add_method(
        "to_number",
        0,
        |interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            let this_name = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern("this")
            };

            let this = match interpreter.get_binding(&this_name) {
                Some(o) => o,
                None => {
                    return Err(RuntimeError::ice(
                        "`this` not defined for method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            };

            Ok(match this {
                Object::String(s) => match s.to_string().parse::<f64>() {
                    Ok(n) => Object::Number(n),
                    Err(_) => Object::Nil,
                },
                _ => {
                    return Err(RuntimeError::ice(
                        "`this` not a string for string method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            })
        },
    );

    //

    interpreter
        .native_methods
        .insert(class_name, native_methods);
}

pub fn init_array_native_methods(interpreter: &mut Interpreter) {
    let class_name = {
        let mut interner = INTERNER.write().unwrap();
        interner.get_or_intern("Array")
    };

    let mut native_methods = NativeClass::default();

    // Array.len()

    native_methods.add_method(
        "len",
        0,
        |interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            let this_name = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern("this")
            };

            let this = match interpreter.get_binding(&this_name) {
                Some(o) => o,
                None => {
                    return Err(RuntimeError::ice(
                        "`this` not defined for method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            };

            match this {
                Object::Array(v) => Ok(Object::Number(v.borrow().len() as f64)),
                _ => Err(RuntimeError::ice(
                    "`this` not an array for array method",
                    SourceLocation::bullshit(),
                )
                .into()),
            }
        },
    );

    // Array.push(obj)

    native_methods.add_method(
        "push",
        1,
        |interpreter: &mut Interpreter, args: Vec<Object>| -> Result<Object, ControlFlow> {
            let obj = &args[0];

            let this_name = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern("this")
            };

            let this = match interpreter.get_binding(&this_name) {
                Some(o) => o,
                None => {
                    return Err(RuntimeError::ice(
                        "`this` not defined for method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            };

            match this {
                Object::Array(v) => {
                    v.borrow_mut().push(obj.clone());
                    Ok(Object::nil())
                }
                _ => Err(RuntimeError::ice(
                    "`this` not an array for array method",
                    SourceLocation::bullshit(),
                )
                .into()),
            }
        },
    );

    // Array.pop()

    native_methods.add_method(
        "pop",
        0,
        |interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            let this_name = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern("this")
            };

            let this = match interpreter.get_binding(&this_name) {
                Some(o) => o,
                None => {
                    return Err(RuntimeError::ice(
                        "`this` not defined for method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            };

            match this {
                Object::Array(v) => Ok(match v.borrow_mut().pop() {
                    Some(o) => o,
                    None => Object::nil(),
                }),
                _ => Err(RuntimeError::ice(
                    "`this` not an array for array method",
                    SourceLocation::bullshit(),
                )
                .into()),
            }
        },
    );

    // Array.clone()

    native_methods.add_method(
        "clone",
        0,
        |interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            let this_name = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern("this")
            };

            let this = match interpreter.get_binding(&this_name) {
                Some(o) => o,
                None => {
                    return Err(RuntimeError::ice(
                        "`this` not defined for method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            };

            match this {
                Object::Array(v) => Ok(Object::Array(Rc::new(RefCell::new(v.borrow().clone())))),
                _ => Err(RuntimeError::ice(
                    "`this` not an array for array method",
                    SourceLocation::bullshit(),
                )
                .into()),
            }
        },
    );

    // Array.bytes_to_str()

    native_methods.add_method(
        "bytes_to_str",
        0,
        |interpreter: &mut Interpreter, _args: Vec<Object>| -> Result<Object, ControlFlow> {
            let this_name = {
                let mut interner = INTERNER.write().unwrap();
                interner.get_or_intern("this")
            };

            let this = match interpreter.get_binding(&this_name) {
                Some(o) => o,
                None => {
                    return Err(RuntimeError::ice(
                        "`this` not defined for method",
                        SourceLocation::bullshit(),
                    )
                    .into())
                }
            };

            match this {
                Object::Array(arr) => {
                    let arr = arr.borrow();
                    let mut bytes = Vec::with_capacity(arr.len());
                    for v in arr.iter() {
                        match v {
                            Object::Number(n) => {
                                let int = n.floor();
                                if (&0.0..&256.0).contains(&n) {
                                    bytes.push(int as u8);
                                } else {
                                    return Ok(Object::Nil);
                                };
                            }
                            _ => return Ok(Object::Nil),
                        }
                    }
                    match std::str::from_utf8(&bytes) {
                        Ok(s) => Ok(Object::dynamic_string(s.to_string())),
                        _ => Ok(Object::Nil),
                    }
                }
                _ => Err(RuntimeError::ice(
                    "`this` not an array for array method",
                    SourceLocation::bullshit(),
                )
                .into()),
            }
        },
    );

    //

    interpreter
        .native_methods
        .insert(class_name, native_methods);
}

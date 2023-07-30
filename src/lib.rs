
use std::fs;

use std::io;
use std::io::BufRead;
use std::io::Write;

use std::sync::RwLock;
use string_interner::DefaultStringInterner;

type GenericResult<T = ()> = std::result::Result<T, Box<dyn std::error::Error>>;

mod arg;
mod ast;
mod interpreter;
mod object;
mod parser;
mod result;
mod scanner;
mod source;
mod token;

use interpreter::ErrorOrReturn;
use interpreter::Interpreter;
use parser::Parser;
use result::Result;
use scanner::Scanner;
use source::SourceId;


#[macro_use]
extern crate lazy_static;

extern crate string_interner;

lazy_static! {
    pub static ref INTERNER: RwLock<DefaultStringInterner> =
        RwLock::new(DefaultStringInterner::new());
    //~ pub static ref SOURCE: RwLock<String> = RwLock::new(String::new());
    /* pub static ref SOURCE: RwLock<HashMap<PathBuf, String>> =
        RwLock::new(HashMap::new()); */
}

pub fn run() -> GenericResult {
    let clargs = {
        use clap::Parser;
        arg::ArgStruct::parse()
    };

    let mut stdout = io::BufWriter::new(io::stdout());

    let result: std::result::Result<(), crate::result::Error> = match clargs.script {
        // If no arguments, run interactively
        None => {
            let mut stdin = io::BufReader::new(io::stdin());
            let mut parser = Parser::new(Scanner::new("", SourceId(0)));
            let mut interpreter = Interpreter::new(clargs.compatibility);

            interpreter.init_global_environment();
            interpreter.init_native_methods();

            loop {
                write!(stdout, "> ")?;
                stdout.flush()?;
                let mut input = String::new();
                stdin.read_line(&mut input)?;
                parser.push_source_string(&input);

                // TODO: handle errors instead of crashing
                let statements = parser.parse_all()?;

                if parser.errors.is_empty() {
                    match interpreter.interpret_slice(&statements[..]) {
                        Ok(_o) => (), //writeln!(stdout, "=> {}", o)?,
                        Err(eor) => match eor {
                            ErrorOrReturn::RuntimeError(e) => writeln!(stdout, "{}", e)?,
                            ErrorOrReturn::Return(o) => writeln!(stdout, "=> {}", o)?,
                            ErrorOrReturn::Break(_) => writeln!(stdout, "error: unexpected break")?,
                        },
                    }
                }

                while !parser.errors.is_empty() {
                    let e = parser.errors.pop().unwrap();
                    eprintln!("{}", e);
                }

                stdout.flush()?;
            }
        }

        // If a filename is given, run it as a script
        Some(script) => {
            let source = fs::read_to_string(script)?;

            let result = run_string(source.clone(), 0, clargs.compatibility);

            if let Err(e) = &result {
                eprintln!(
                    "error on line {:?}",
                    crate::source::SourceLocation::error_line_number(e, &source)
                )
            }

            stdout.flush()?;

            match result {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            }
        }
    };

    match result {
        Ok(_o) => Ok(()),
        Err(e) => {
            match e.get_location() {
                Some(loc) => eprintln!("{} at {}", e, loc),
                _ => eprintln!("{:?}", e),
            }
            Ok(())
        }
    }
}

pub fn run_string(
    source: String,
    id: usize,
    compatibility_mode: bool,
) -> Result<Option<object::Object>> {
    let mut parser = Parser::new(Scanner::new(&source, SourceId(id)));
    let statements = parser.parse_all()?;

    let had_error = !parser.errors.is_empty();

    if had_error {
        //~ if true {
        for e in parser.errors {
            eprintln!(
                "error on line {:?}: {}",
                crate::source::SourceLocation::error_line_number(
                    &result::Error::Parser(e.clone()),
                    &source
                ),
                e,
            )
        }

        //~ /*
        eprintln!();
        for s in &statements {
            //~ eprint!("{} ", s);
            eprintln!("{}", s);
        }
        eprintln!();
        //~ */
        return Ok(None);
    }

    let mut interpreter = Interpreter::new(compatibility_mode);

    interpreter.init_global_environment();
    interpreter.init_native_methods();

    let obj = match interpreter.interpret_slice(&statements[..]) {
        Ok(obj) => obj,
        Err(eor) => match eor {
            ErrorOrReturn::RuntimeError(e) => {
                let src = source::Source::new(&source);
                eprintln!("error on line {:?}", src.get_line_number(&e.location),);

                return Err(e.into());
            }
            ErrorOrReturn::Return(v) => v,
            ErrorOrReturn::Break(_v) => return Err(result::Error::BreakOutsideLoop),
        },
    };

    Ok(Some(obj))
}

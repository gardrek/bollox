/*
Derivables:
Eq, PartialEq,
Ord, PartialOrd
Clone, Copy,
Hash,
Default,
Debug,
*/

#[macro_use]
extern crate lazy_static;

extern crate string_interner;

use std::fs;

use std::io;
use std::io::BufRead;
use std::io::Write;

use std::sync::RwLock;
use string_interner::DefaultStringInterner;

//~ use std::collections::HashMap;
//~ use std::path::PathBuf;

lazy_static! {
    pub static ref INTERNER: RwLock<DefaultStringInterner> =
        RwLock::new(DefaultStringInterner::new());
    pub static ref SOURCE: RwLock<String> = RwLock::new(String::new());
    /* pub static ref SOURCE: RwLock<HashMap<PathBuf, String>> =
        RwLock::new(HashMap::new()); */
}

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

fn run() -> GenericResult {
    let clargs = {
        use clap::Parser;
        arg::ArgStruct::parse()
    };

    let mut stdout = io::BufWriter::new(io::stdout());

    let result: std::result::Result<(), crate::result::Error> = match clargs.script {
        // If no arguments, run interactively
        None => {
            let mut stdin = io::BufReader::new(io::stdin());
            let mut next_source_id = 0;
            loop {
                write!(stdout, "> ")?;
                stdout.flush()?;
                let mut input = String::new();
                stdin.read_line(&mut input)?;
                next_source_id += 1;
                match run_string(input, next_source_id, clargs.compatibility) {
                    Ok(Some(res)) => writeln!(stdout, "=> {}", res)?,
                    Ok(None) => (),
                    Err(res) => writeln!(stdout, "eyoooo {:?}", res)?,
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

fn main() {
    match run() {
        Ok(_) => (),
        Err(e) => {
            eprintln!("main error: {}", e);
        }
    }
}

pub fn run_string(source: String, id: usize, compatibility_mode: bool) -> Result<Option<object::Object>> {
    // past me sure did this nonsense
    // FIXME: not sure why i am trying to do this at this point
    {
        let mut source_ref = SOURCE.write().unwrap();
        source_ref.push_str(&source);
    }

    let mut sc = Scanner::new(&source, SourceId(id));
    //~ let tokens = sc.collect_or_first_error()?;

    //~ let (tokens, errors) = sc.collect_all_tokens_and_errors();

    let (tokens, errors) = match sc.collect_all_tokens() {
        Ok(tokens) => (tokens, None),
        Err((tokens, errors)) => (tokens, Some(errors)),
    };

    if let Some(errors) = errors {
        if !errors.is_empty() {
            return Err(if errors.len() == 1 {
                errors[0].clone()
            } else {
                crate::result::Error::ManyErrors(errors)
            });
        }
    }

    /*
    for tk in &tokens {
        eprint!("{} ", tk);
        //~ eprintln!();
        //~ eprintln!("len: {}", tk.location().length());
        //~ eprintln!();
    }
    eprintln!();
    //~ */

    let mut parser = Parser::new(tokens);
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

    let obj = match interpreter.interpret_slice(&statements[..]) {
        Ok(obj) => obj,
        Err(eor) => match eor {
            ErrorOrReturn::RuntimeError(e) => return Err(e.into()),
            ErrorOrReturn::Return(v) => v,
            ErrorOrReturn::Break(_v) => todo!(),
        },
    };

    Ok(Some(obj))
}

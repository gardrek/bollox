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

use std::env;
use std::fs;
use std::path::Path;

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

//mod interpreter;
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
    let args: Vec<String> = env::args().collect();

    let mut stdout = io::BufWriter::new(io::stdout());

    let result: std::result::Result<(), crate::result::Error> = match args.len() {
        // If no arguments, run interactively
        1 => {
            let mut stdin = io::BufReader::new(io::stdin());
            let mut next_source_id = 0;
            loop {
                write!(stdout, "> ")?;
                stdout.flush()?;
                let mut input = String::new();
                stdin.read_line(&mut input)?;
                next_source_id += 1;
                match run_string(input, next_source_id) {
                    Ok(Some(res)) => writeln!(stdout, "eyoooo? {}", res)?,
                    Ok(None) => (),
                    Err(res) => writeln!(stdout, "eyoooo {:?}", res)?,
                }
                stdout.flush()?;
            }
        }

        // If a filename is given, run it as a script
        2 => {
            let source = fs::read_to_string(Path::new(&args[1]))?;

            let result = run_string(source.clone(), 0);

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

        _ => Err(result::Error::Usage),
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

fn run_string(source: String, id: usize) -> Result<Option<String>> {
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

    let had_error = parser.errors.len() != 0;

    if had_error {
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
        for s in &statements {
            //~ eprint!("{} ", s);
            eprintln!("{}", s);
        }
        eprintln!();
        //~ */
        return Ok(None);
    }

    let mut interpreter = Interpreter::new();

    interpreter.init_global_environment();

    let obj = match interpreter.interpret_slice(&statements[..]) {
        Ok(obj) => obj,
        Err(eor) => match eor {
            ErrorOrReturn::RuntimeError(e) => return Err(e.into()),
            ErrorOrReturn::Return(v) => v,
        },
    };

    let s = format!("{}", obj);

    Ok(Some(s))
}

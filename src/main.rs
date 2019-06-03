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

//use std::collections::HashMap;
//use std::path::PathBuf;

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
//mod store;

fn run() -> GenericResult {
    let args: Vec<String> = env::args().collect();

    let mut stdout = io::BufWriter::new(io::stdout());

    match args.len() {
        // If no arguments, run interactively
        1 => {
            let mut stdin = io::BufReader::new(io::stdin());
            loop {
                write!(stdout, "> ")?;
                stdout.flush()?;
                let mut input = String::new();
                stdin.read_line(&mut input)?;
                match run_string(input) {
                    Ok(Some(res)) => writeln!(stdout, "{}", res)?,
                    Ok(None) => (),
                    Err(res) => writeln!(stdout, "{:?}", res)?,
                }
                stdout.flush()?;
            }
        }

        // If a filename is given, run it as a script
        2 => {
            let source = fs::read_to_string(Path::new(&args[1]))?;
            run_string(source)?;
            stdout.flush()?;
            Ok(())
        }

        _ => Err(result::Error::Usage.into()),
    }
}

fn main() -> () {
    match run() {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{}", e);
            ()
        }
    }
}

use interpreter::Interpreter;
use scanner::Scanner;
use parser::Parser;
use result::Result;

pub fn run_string(source: String) -> Result<Option<String>> {
    {
        let mut source_ref = SOURCE.write().unwrap();
        source_ref.push_str(&source);
    }

    let mut sc = Scanner::new();
    let tokens = sc.collect_or_first_error()?;

    let mut parser = Parser::new(tokens);
    let statements = parser.parse()?;

    let mut interpreter = Interpreter::new();

    if let Some(obj) = interpreter.interpret(statements)? {
        let s = format!("{}", obj);

        Ok(Some(s.into()))
    } else {
        Ok(None)
    }
}


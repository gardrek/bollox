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

use interpreter::Interpreter;
use parser::Parser;
use result::Result;
use scanner::Scanner;
use source::SourceId;

fn run() -> GenericResult {
    let args: Vec<String> = env::args().collect();

    let mut stdout = io::BufWriter::new(io::stdout());

    match args.len() {
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
            run_string(source, 0)?;
            stdout.flush()?;
            Ok(())
        }

        _ => Err(result::Error::Usage.into()),
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

    let mut sc = Scanner::new(SourceId(id));
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

    for tk in &tokens {
        eprint!("{} ", tk);
        //~ eprintln!();
        //~ eprintln!("len: {}", tk.location().length());
        //~ eprintln!();
    }
    eprintln!();

    let mut parser = Parser::new(tokens);
    let statements = parser.parse_all()?;

    let mut interpreter = Interpreter::new();

    if let Some(obj) = interpreter.interpret(statements)? {
        let s = format!("{}", obj);

        Ok(Some(s))
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn exploration() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    #[should_panic]
    fn another() {
        panic!("Make this test fail");
    }
}

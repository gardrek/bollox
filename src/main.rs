/*
Derivables:
Eq, PartialEq,
Ord, PartialOrd
Clone, Copy,
Hash,
Default,
Debug,
*/

use std::env;
use std::fs;
use std::path::Path;

use std::io;
use std::io::BufRead;
use std::io::Write;

type GenericResult<T = ()> = Result<T, Box<dyn std::error::Error>>;

//mod interpreter;
mod ast;
mod interpreter;
mod object;
mod parser;
mod result;
mod scanner;
mod store;

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
                match interpreter::run_string(input) {
                    Ok(res) => writeln!(stdout, "{}", res)?,
                    Err(res) => writeln!(stdout, "{:?}", res)?,
                }
                stdout.flush()?;
            }
        }

        // If a filename is given, run it as a script
        2 => {
            let source = fs::read_to_string(Path::new(&args[1]))?;
            writeln!(stdout, "{}", interpreter::run_string(source)?)?;
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

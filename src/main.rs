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
use std::io::Write;

type GenericResult<T = ()> = Result<T, Box<dyn std::error::Error>>;

mod interpreter;
mod result;
mod scanner;

fn run(source: &str) -> GenericResult {
    let sc = scanner::Scanner::new(source);
    let tokens = sc.tokens();
    for t in tokens.iter() {
        eprintln!("{:?}", t);
    }
    //Err(Box::new(result::Error::Other(Box::new(io::Error::new(io::ErrorKind::Other, "ohnoes"))))
    Ok(())
}

fn run_file(path: &Path) -> GenericResult {
    let script = fs::read_to_string(path)?;
    run(&script)
}

fn run_prompt() -> GenericResult {
    // TODO: make this work and loop and stuff.
    // probably do need to keep persistent state in a struct
    // TODO: use io::BufReader because it is better for this use case
    let mut out = io::LineWriter::new(io::stdout());
    loop {
        write!(out, "λ ")?;
        out.flush()?;
        let mut line = String::new();
        io::stdin().read_line(&mut line)?;
        match run(&line) {
            Ok(_) => (),
            Err(_) => break,
        }
    }
    Ok(())
}

fn main() -> GenericResult {
    let args: Vec<String> = env::args().collect();
    let r: Result<_, Box<dyn std::error::Error>> = match args.len() {
        1 => run_prompt(),                  // If no arguments, run interactively
        2 => run_file(Path::new(&args[1])), // If a filename is given, run it as a script
        //_ => Err(Error::Usage),                                      // Print usage
        _ => Err(Box::new(result::Error::Usage)),
    };
    match r {
        Ok(_) => Ok(()),
        //Err(e) => Err(e.into()),
        Err(e) => {
            println!("{}", e);
            Err(e)
        }
    }
}

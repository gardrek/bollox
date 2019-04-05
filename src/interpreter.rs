use super::scanner::Scanner;
//use super::result::Result as LoxResult;
use super::result::Error as LoxError;

type LoxResult<T = ()> = Result<T, LoxError>;

/*
Derivables:
Eq, PartialEq,
Ord, PartialOrd
Clone, Copy,
Hash,
Default,
Debug,
*/

use std::fs;
use std::path::Path;

use std::io;
use std::io::Write;

pub struct Vm<'a> {
    // Needs a scanner so it can scan new lines of source in interactive mode
    scanner: Scanner<'a>,
}

impl<'a> Vm<'a> {
    fn from_source(source: &'a str) -> Self {
        Self {
            scanner: Scanner::new(source),
        }
    }

    //fn init_from_file(path: &Path) -> LoxResult<Self> {
    //let source = fs::read_to_string(path)?;
    //Ok((source, Vm::from_source(source)))
    //}

    fn run(&mut self) -> LoxResult {
        let mut sc: &Scanner<'_> = &self.scanner;
        for t in sc {
            eprintln!("{:?}", t);
        }
        //Err(Box::new(result::Error::Other(Box::new(io::Error::new(io::ErrorKind::Other, "ohnoes"))))
        Ok(())
    }

    fn run_prompt(&mut self) -> LoxResult {
        // TODO: make this work and loop and stuff.
        // probably do need to keep persistent state in a struct
        // TODO: use io::BufReader because it is better for this use case
        let mut out = io::LineWriter::new(io::stdout());
        loop {
            write!(out, "λ ")?;
            out.flush()?;
            let mut line = String::new();
            io::stdin().read_line(&mut line)?;
            match self.run() {
                Ok(_) => (),
                Err(_) => break,
            }
        }
        Ok(())
    }
}

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

//use std::io;
//use std::io::Write;

type GenericResult<T = ()> = Result<T, Box<dyn std::error::Error>>;

//mod interpreter;
mod result;
mod scanner;

fn main() -> GenericResult {
    let args: Vec<String> = env::args().collect();
    let r = match args.len() {
        // If no arguments, run interactively
        1 => {
            scanner::test_run("".into());
            Ok(())
            //unimplemented!()
        }

        // If a filename is given, run it as a script
        2 => {
            let source = fs::read_to_string(Path::new(&args[1]))?;
            scanner::test_run(source);
            Ok(())
        }

        _ => Err(result::Error::Usage),
    };
    match r {
        Ok(_) => Ok(()),
        Err(e) => {
            println!("{}", e);
            Err(Box::new(e))
        }
    }
}

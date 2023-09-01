use std::fs;

use std::io;
use std::io::BufRead;
use std::io::Write;

use bollox::*;

use interpreter::ControlFlow;
use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;
use source::SourceId;

use bollox::GenericResult;

fn main() -> Result<(), u8> {
    match run() {
        Ok(_) => Ok(()),
        Err(_) => std::process::exit(1),
    }
}

fn run() -> GenericResult {
    let clargs = {
        use clap::Parser;
        arg::ArgStruct::parse()
    };

    let mut stdout = io::BufWriter::new(io::stdout());

    let result: std::result::Result<(), result::Error> = match clargs.script {
        // If no arguments, run interactively
        None => {
            let mut stdin = io::BufReader::new(io::stdin());
            let mut parser = Parser::new(
                Scanner::new("", SourceId(0), clargs.compatibility),
                clargs.compatibility,
            );
            let mut interpreter = Interpreter::new_with_stdlib();

            let mut resolver = bollox::resolver::Resolver::default();

            resolver.resolve_start();

            loop {
                write!(stdout, "> ")?;
                stdout.flush()?;
                let mut input = String::new();
                stdin.read_line(&mut input)?;
                parser.push_source_string(&input);

                // TODO: handle errors instead of crashing
                let mut statements = parser.parse_all()?;

                resolver.resolve_list(&mut statements[..]);

                if resolver.errors.is_empty() && parser.errors().is_empty() {
                    match interpreter.interpret_slice(&statements[..]) {
                        Ok(_o) => (), //writeln!(stdout, "=> {}", o)?,
                        Err(eor) => match eor {
                            ControlFlow::RuntimeError(e) => writeln!(stdout, "{}", e)?,
                            ControlFlow::Return(o) => writeln!(stdout, "=> {}", o)?,
                            ControlFlow::Break(_) => writeln!(stdout, "error: unexpected break")?,
                        },
                    }
                }

                while let Some(e) = resolver.errors.pop() {
                    eprintln!("{}", e);
                }

                for e in parser.errors().iter() {
                    eprintln!("{}", e);
                }

                parser.clear_errors();

                stdout.flush()?;
            }
        }

        // If a filename is given, run it as a script
        Some(script) => {
            let source = fs::read_to_string(script)?;

            let result = bollox::run_string(source.clone(), 0, clargs.compatibility);

            /*
            if let Err(e) = &result {
                eprintln!(
                    "error on line {:?}: `{}`",
                    source::SourceLocation::error_line_number(e, &source),
                    match e.get_location() {
                        Some(l) => &source[l.range.clone()],
                        None => "",
                    }
                )
            }
            */

            stdout.flush()?;

            match result {
                Ok(_) => Ok(()),
                Err(e) => Err(e),
            }
        }
    };

    match &result {
        Ok(_o) => Ok(()),
        Err(_e) => Ok(result?),
    }
}

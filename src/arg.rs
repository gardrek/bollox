use clap::Parser;

#[derive(Parser)]
#[command(name = "bollox")]
//#[command(author = "Nonymous A. <admin@gmail.com>")]
#[command(version = "1.0")]
#[command(about = "Lox interpreter", long_about = None)]
pub struct ArgStruct {
    /// Enable compatibility mode for better compatibility with standard Lox
    #[arg(long)]
    pub compatibility: bool,

    /// The script to run
    pub script: Option<std::path::PathBuf>,
}

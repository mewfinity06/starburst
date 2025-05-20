// Rust
use std::fs::File;
use std::io::{BufReader, Read};

// Vendor
use clap::{Parser as ClapParser, Subcommand};

// Starburst
use lexer::Lexer;
use parser::Parser;

#[derive(Subcommand, Debug)]
enum Command {
    /// TODO:
    /// Build the program
    Build,
    /// TODO:
    /// Build and run the program
    BuildRun,
    /// TODO:
    /// Runs the program
    Run,
    /// Print IR (lexed tokens) | to be deprecated
    IR,
    /// TODO:
    /// Prints all the types in your terminal
    Docs,
}

#[derive(ClapParser, Debug)]
struct Cli {
    #[command(subcommand)]
    cmd: Command,

    path: String,
}

fn main() -> Result<(), &'static str> {
    let cli = Cli::parse();

    let content = read_entire_file_buffered(&cli.path);

    let lex = Lexer::new(&content);
    let parser = Parser::new(lex);

    match &cli.cmd {
        Command::IR => {
            println!("{}", content);

            let parsed: Vec<_> = parser.collect();

            dbg!(parsed);
        }
        Command::Build => todo!(),
        Command::BuildRun => todo!(),
        Command::Run => todo!(),
        Command::Docs => todo!(),
    }

    Ok(())
}

fn read_entire_file_buffered(path: &str) -> String {
    let file = File::open(path).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut contents = String::new();
    reader
        .read_to_string(&mut contents)
        .expect("Failed to read file");
    contents
}

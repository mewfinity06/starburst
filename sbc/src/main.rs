use std::fs::File;
use std::io::{BufReader, Read};

use lexer::Lexer;

use clap::{Parser, Subcommand};

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

#[derive(Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    cmd: Command,

    path: String,
}

fn main() -> Result<(), &'static str> {
    let cli = Cli::parse();

    let content = read_entire_file_buffered(&cli.path);

    let lex = Lexer::new(&content);

    match &cli.cmd {
        Command::IR => {
            use lexer::tokens::TokenKind::*;

            println!("{}\n\n", content);

            for (i, token) in lex.enumerate() {
                print!("{}: ", i);
                match token.kind {
                    Comment | DocComment => continue,
                    Identifier | BuiltinType | StringLiteral | CharLiteral | IntLiteral => {
                        println!("'{}' ", &content[token.span.start..token.span.end])
                    }
                    _ => println!("{:?} ", token.kind),
                }
            }
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

// use lexer::Lexer;
// use lexer::tokens::TokenKind::*;

// fn main() {
//     let content = r#"--functions
// const add | func [a: Int, b: Int] -> Int : {
//     a + b
// }

// const sub | func [a, b: Int] -> Int : a - b;

// val v1 |: add[a: 2, b: 8]
// val v2 |: sub[3, 6]
//         "#;
//     let lexer = Lexer::new(content);

//     println!("{}", content);

//     let mut counter: usize = 0;
//     for token in lexer {
//         if counter % 3 == 0 {
//             println!();
//         }
//         match token.kind {
//             DocComment | Comment => continue,
//             Identifier | BuiltinType | StringLiteral | CharLiteral | IntLiteral => {
//                 print!("'{}' ", &content[token.span.start..token.span.end])
//             }
//             _ => print!("{:?} ", token.kind),
//         }
//         counter += 1;
//     }
//     println!();
// }

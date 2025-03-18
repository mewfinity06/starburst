use clap::Parser;

mod config_parser;
use config_parser::*;

fn main() {
    // let cli = Cli::parse();
    // println!("{:?}", cli);
    let config = Config::new();
    println!("{:?}", config);
}

#[derive(Debug, Parser)]
struct Cli {
    #[clap(subcommand)]
    cmd: SubCommand,
}

#[derive(Debug, clap::Subcommand)]
enum SubCommand {
    Init,
    Build,
    Run,
}

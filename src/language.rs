use std::fs::File;
use std::io::{BufRead, BufReader, Read};
use std::path::Path;

use anyhow::Context;
use anyhow::anyhow;

use crate::config_parser::Config;
use crate::lexer::*;
use crate::parser::*;

pub fn get_hello_world() -> String {
    String::from(
        r#"use std::io::println;

func main : {
    println("Hello world!");
}
"#,
    )
}

pub fn compile(config: Config) -> anyhow::Result<()> {
    let lines = read_file_to_lines(config.input_file.as_str(), &config)?;

    if let Ok(tokens) = Lexer::lex(lines) {
        for token in tokens {
            println!("{:?}", token);
        }
    }

    Ok(())
}

fn read_file_to_lines(path: &str, config: &Config) -> anyhow::Result<Vec<String>> {
    let mut buffer: Vec<String> = vec![];
    let full_file_path: String = format!("{}/{}", config.project_dir, path);
    let file = File::open(full_file_path.clone())
        .context(format!("file being read: {}", full_file_path))?;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line?;
        buffer.push(line);
    }

    Ok(buffer)
}

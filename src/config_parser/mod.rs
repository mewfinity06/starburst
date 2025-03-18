use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

const CONFIG_FILE_EXTENSION: &str = ".config";

fn find_config_file() -> Option<PathBuf> {
    let current_dir = std::env::current_dir().ok()?;
    for entry in fs::read_dir(current_dir).ok()? {
        let entry = entry.ok()?; // Handle potential errors
        let path = entry.path();
        if let Some(filename) = path.file_name().and_then(|f| f.to_str()) {
            if filename.ends_with(CONFIG_FILE_EXTENSION) {
                return Some(path);
            }
        }
    }
    None
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Config {
    output_file: String,
    debug: bool,
}

impl Config {
    pub fn new() -> anyhow::Result<Self> {
        enum Mode {
            Bin,
            None,
        }

        if let Some(path) = find_config_file() {
            let mut config = Self::default();
            // Read config file!
            let f = File::open(path)?;
            let f = BufReader::new(f);

            // Build the config
            let mut mode = Mode::None;

            for line in f.lines() {
                let line = line?;

                if line.starts_with("--") {
                    continue;
                }

                let mut splits = line.split([' ', '=']);

                while let Some(word) = splits.next() {
                    if word == "[bin]" {
                        mode = Mode::Bin;
                    }

                    match mode {
                        Mode::Bin => {
                            if word == "name" {
                                let name = splits.next().unwrap();
                                config.output_file = match name {
                                    x if x.is_empty()
                                        || x.contains([
                                            ' ', '"', '\'', '!', '@', '#', '$', '%', '^', '&', '*',
                                            '+', '+', '{', '}', '[', ']', '\\', '|', ';', ':', ',',
                                            '.', '<', '>',
                                        ]) =>
                                    {
                                        return Err(anyhow::anyhow!("Malformated output name"));
                                    }
                                    _ => name.to_string(),
                                };
                                continue;
                            }

                            if word == "debug" {
                                config.debug = match splits.next().unwrap() {
                                    "true" => true,
                                    "false" => false,
                                    _ => return Err(anyhow::anyhow!("Invalid value for debug")),
                                };
                                continue;
                            }
                        }
                        Mode::None => {}
                    }
                }
            }

            // Return the config
            Ok(config)
        } else {
            Ok(Self::default())
        }
    }

    fn default() -> Self {
        Self {
            output_file: String::from("/bin/main"),
            debug: false,
        }
    }
}

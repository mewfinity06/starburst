use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

const CONFIG_FILE_EXTENSION: &str = ".config";

fn find_config_file(dir: &Path) -> Option<PathBuf> {
    let current_dir = dir;
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
    pub project_dir: String,
    pub output_exe: String,
    pub input_file: String,
    pub debug: bool,
}

impl Config {
    pub fn new(dir: &Path) -> anyhow::Result<Self> {
        enum Mode {
            Bin,
            Project,
            None,
        }

        if let Some(path) = find_config_file(dir) {
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
                    match word {
                        "[bin]" => mode = Mode::Bin,
                        "[project]" => mode = Mode::Project,
                        _ => {}
                    }

                    match mode {
                        Mode::Bin => {
                            if word == "name" {
                                let name = splits.next().unwrap();
                                config.output_exe = match name {
                                    x if x.is_empty()
                                        || x.contains([
                                            ' ', '"', '\'', '!', '@', '#', '$', '%', '^', '&', '*',
                                            '+', '{', '}', '[', ']', '\\', '|', ';', ':', ',', '<',
                                            '>',
                                        ]) =>
                                    {
                                        return Err(anyhow::anyhow!(
                                            "Malformated output name : {:?}",
                                            name
                                        ));
                                    }
                                    _ => name.to_string(),
                                };
                            } else if word == "input" {
                                let name = splits.next().unwrap();
                                config.input_file = match name {
                                    x if x.is_empty()
                                        || x.contains([
                                            ' ', '"', '\'', '!', '@', '#', '$', '%', '^', '&', '*',
                                            '+', '{', '}', '[', ']', '\\', '|', ';', ':', ',', '<',
                                            '>',
                                        ]) =>
                                    {
                                        return Err(anyhow::anyhow!(
                                            "Malformated input name : {:?}",
                                            name
                                        ));
                                    }
                                    _ => name.to_string(),
                                };
                                continue;
                            } else if word == "debug" {
                                config.debug = match splits.next().unwrap() {
                                    "true" => true,
                                    "false" => false,
                                    _ => return Err(anyhow::anyhow!("Invalid value for debug")),
                                };
                            }
                        }
                        Mode::Project => {
                            if word == "project_dir" {
                                let dir = splits.next().unwrap();
                                config.project_dir = match dir {
                                    x if !x.is_empty() => x.to_string(),
                                    _ => {
                                        return Err(anyhow::anyhow!(
                                            "Malformatted dir : {:?}",
                                            dir
                                        ));
                                    }
                                }
                            }
                        }
                        Mode::None => {}
                    }
                }
            }

            // if the project_dir is not set, it should be the path to the dir built
            if config.project_dir.is_empty() {
                config.project_dir = dir.to_string_lossy().to_string();
            }

            // Return the config
            Ok(config)
        } else {
            Ok(Self::default())
        }
    }

    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        let mut res = String::new();

        // Print all project options
        res.push_str("[project]\n");
        res.push_str(&format!("project_dir={}\n", self.project_dir));
        // Print all bin options
        res.push_str("[bin]\n");
        res.push_str(&format!("name={}\n", self.output_exe));
        res.push_str(&format!("input={}\n", self.input_file));
        res.push_str(&format!("debug={}\n", self.debug));

        res
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            project_dir: String::new(),
            output_exe: String::from("/build/main"),
            input_file: String::from("main.sbl"),
            debug: false,
        }
    }
}

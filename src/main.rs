use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use clap::*;

pub mod config_parser;
use config_parser::*;

pub mod language;

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    // println!("{:?}", cli);
    let dir = Path::new(&cli.dir);

    let config = Config::new(dir)?;
    // println!("{:?}", config);

    match cli.cmd.clone() {
        // Init the repo
        SubCommand::Init { proj_name } => cli.handle_init(dir, proj_name)?,
        SubCommand::Build => cli.handle_build(config)?,
        SubCommand::Run => cli.handle_run(config)?,
    }

    Ok(())
}

#[derive(Debug, Parser)]
#[command(
    version,
    about,
    long_about = r#"Starburst is a language that attempts to build off of Rust!
Starburst will be able to interop with Rust code, being able to call Rust (and C) functions and code externally.
To see (or aid!) in development, go to the github page: github.com/mewfinity06/starburst"#
)]
pub struct Cli {
    #[clap(subcommand)]
    cmd: SubCommand,
    dir: PathBuf,
}

impl Cli {
    /// TODO:
    /// - Have an interactive session?
    ///     - Instead of providing dir, proj_name in command line, we should have a prompt and build the new project accordingly
    /// - Do not overwrite files if they already exist, just create missing files
    ///
    /// This function initializes a new starburst project
    fn handle_init(&self, dir: &Path, proj_name: String) -> anyhow::Result<()> {
        // Check if the directory exists, if it doesn't, create it
        if !dir.exists() {
            fs::create_dir_all(dir)?;
        }

        println!("Creating new project : {}", proj_name);

        // Load the file paths (config, main respectively) and create them
        let config_file_path = dir.join(format!("{}.config", proj_name));
        let main_file_path = dir.join(format!("{}.star", proj_name));

        let mut config_file = File::create(&config_file_path)?;
        let mut main_file = File::create(&main_file_path)?;

        println!("Created config       : {}", config_file_path.display());
        println!("Created main file    : {}", main_file_path.display());

        // Get the build directory and create it
        let build_dir = dir.join("build");
        fs::create_dir_all(&build_dir)?;

        println!("Created build dir    : {}", build_dir.display());

        let basic_config_file = Config::default().to_string();
        let hello_world = language::get_hello_world();

        config_file.write_all(basic_config_file.as_bytes())?;
        main_file.write_all(hello_world.as_bytes())?;

        Ok(())
    }

    /// This function builds the current starburst project
    fn handle_build(&self, _config: Config) -> anyhow::Result<()> {
        Err(anyhow::anyhow!("Not implemented"))
    }

    /// This fucntion builds (if the current build is not up-to-date) and runs it
    fn handle_run(&self, _config: Config) -> anyhow::Result<()> {
        Err(anyhow::anyhow!("Not implemented"))
    }
}

#[derive(Debug, Subcommand, Clone)]
pub enum SubCommand {
    /// Initialize a new Startburst Project!
    Init {
        /// The name of your project!
        /// This will define your config name and the name of your final executable
        proj_name: String,
    },
    /// Build an executable for the current project!
    /// Does not build if the executable is up-to-date
    Build,
    /// Build and run the current project!
    /// Does not build if the executable is up-to-date
    Run,
}

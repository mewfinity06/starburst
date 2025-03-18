#![allow(unused_imports)]
use std::fs::OpenOptions;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use chrono::Local;
use clap::*;
use fern::Dispatch;
use log::{error, info, warn};

pub mod config_parser;
use config_parser::*;

pub mod language;
pub mod lexer;
pub mod parser;

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    // println!("{:?}", cli);
    let dir = Path::new(&cli.dir);

    let config = Config::new(dir)?;
    // println!("{:?}", config);

    // Set up fern for logging
    let log_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("starburst.log")?;

    Dispatch::new()
        .format(|out, message, record| {
            let now = Local::now();

            out.finish(format_args!(
                "[{}] [{}] {}: {}",
                now.format("%Y-%m-%d %H:%M:%S"),
                record.level(),
                record.target(),
                message
            ))
        })
        .level(log::LevelFilter::Info)
        .chain(std::io::stdout())
        .chain(log_file)
        .apply()?;

    match cli.cmd.clone() {
        // Init the repo
        SubCommand::Init { proj_name } => cli.handle_init(dir, proj_name)?,
        SubCommand::Build => cli.handle_build(config)?,
        SubCommand::Run => cli.handle_run(config)?,
        SubCommand::ReadConfig => info!("{}", config.to_string()),
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
    /// This function initializes a new starburst project
    ///
    /// TODO:
    /// - Have an interactive session?
    ///     - Instead of providing dir, proj_name in command line, we should have a prompt and build the new project accordingly
    /// - Do not overwrite files if they already exist, just create missing files
    /// - Tidy up code into smaller functions
    ///
    fn handle_init(&self, dir: &Path, proj_name: String) -> anyhow::Result<()> {
        // Check if the directory exists, if it doesn't, create it
        fs::create_dir_all(dir)?;

        info!("Creating new project         : {}", proj_name);

        // Load the file paths (config, main respectively)
        let config_file_path = dir.join(format!("{}.config", proj_name));
        let main_file_path = dir.join(format!("{}.star", proj_name));

        // Create config file if it doesn't exist
        if !config_file_path.exists() {
            let mut config_file = File::create(&config_file_path)?;
            let basic_config_file = Config::default().to_string();
            config_file.write_all(basic_config_file.as_bytes())?;
            info!(
                "Created config               : {}",
                config_file_path.display()
            );
        }

        // Create main file if it doesn't exist
        if !main_file_path.exists() {
            let mut main_file = File::create(&main_file_path)?;
            let hello_world = language::get_hello_world();
            main_file.write_all(hello_world.as_bytes())?;
            info!(
                "Created main file            : {}",
                main_file_path.display()
            );
        }

        // Get the build directory and create it
        let build_dir = dir.join("build");
        if !build_dir.exists() {
            fs::create_dir_all(&build_dir)?;
        }

        info!("Created build dir            : {}", build_dir.display());

        // .starburst files, such as project.lock, etc
        let starburst_file_dir = dir.join(".starburst");
        if !starburst_file_dir.exists() {
            fs::create_dir_all(&starburst_file_dir)?;
        }

        // .lock file should always be rewritten
        let starburst_lock_file = starburst_file_dir.join("starburst.lock");
        let mut starburst_lock_file = File::create(&starburst_lock_file)?;
        starburst_lock_file.write_all(b"last_time_write:\n")?;

        info!(
            "Created .starburst dir       : {}",
            starburst_file_dir.display()
        );

        info!("Successfully created project : {}", proj_name);

        Ok(())
    }

    /// This function builds the current starburst project
    ///
    /// TODO: Check if last copiletime is current.
    /// Currently we always build and that is inefficient and makes the compiler slow and annoying to use.
    /// handle_build is essentially a wraper for language::compile right now
    fn handle_build(&self, config: Config) -> anyhow::Result<()> {
        language::compile(config)
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
    /// Print the config to the console
    ReadConfig,
}

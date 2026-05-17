use clap::{Parser, Subcommand};
use colored::*;
use std::time::Instant;

use error::CliError;
use threader::Threadpool;

#[macro_use]
mod error;
mod byte_parsing;
mod threader;
mod tokenizing;
mod type_parsing;
#[macro_use]
mod utilities;
mod codegen;
mod grapher;
mod literal_parsing;
mod parser;
/*
___    __
|#/    `"\_
 \\\      ||
  ¯\\_   ||
,   '\|\//
`^\\==/"|,_
 //     '"\\
 ¯         `¯
opensource
*/
#[derive(Parser)]
#[command(author = "mikatschuh", version = "0.1", about = ABOUT.clone(), long_about = None, name = "rux")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
    #[arg(long)]
    mute: bool,
}

#[derive(Subcommand)]
enum Commands {
    Build { path: Option<String> },
}
fn main() {
    match Cli::try_parse() {
        Ok(parsed) => match parsed.command {
            Some(Commands::Build { path }) => {
                #[allow(unused)]
                let now = Instant::now();
                if let Err(e) = (|| -> Result<(), CliError> {
                    let mut threadpool = Threadpool::new();
                    threadpool.start_up(path)?;
                    threadpool.launch(None);
                    threadpool.drop()
                })() {
                    println!("{e}");
                } else if !parsed.mute {
                    // print_time(now.elapsed().as_nanos(), "compiling", "no optimizations");
                }
            }
            None => println!("{}", *ABOUT),
        },
        Err(e) => {
            use clap::error::ErrorKind;
            match e.kind() {
                ErrorKind::InvalidSubcommand => {
                    println!(
                        "\n{}{} unrecognized subcommand, maybe you meant {}?\n",
                        "error".red().bold(),
                        ":".bold(),
                        "build".bold()
                    );
                }
                _ => e.exit(), // Für alle anderen Fehler trotzdem rausballern
            }
        }
    }
}

#[allow(unused)]
fn print_time(time_past: u128, task: &str, attributes: &str) {
    println!(
        "\n{}  {} with {} in: {}\n",
        "Finished".bold(),
        task,
        attributes,
        format_time(time_past)
    )
}
fn format_time(time: u128) -> String {
    let mut time = time as f64;

    let mut unit_number: usize = 0;
    loop {
        if time < 1000.0 {
            break;
        }
        time /= 1000.0;
        unit_number += 1;
    }
    let mut number_as_string = time.to_string();
    if !number_as_string.contains(".") {
        number_as_string += ".0";
    }

    format!(
        "{} {}",
        number_as_string,
        ["ns", "µs", "ms", "s"][if unit_number < 4 { unit_number } else { 3 }]
    )
}
fn _print_communism() {
    println!(
        "{}\n{}{}\n{}{}\n{}{}\n{}{}{}\n{}{}\n{}{}\n{}{}\n  {}",
        "      __".red(),
        ".+#||>".bold(),
        " `\"\\_".red(), // 2rd
        "  \\\\\\".bold(),
        "     ||".red(), // 3th
        "   ¯\\\\_".bold(),
        "   ||".red(), // 4th
        "\\\\".red(),
        "   '\\|\\".bold(),
        "//".red(), // 5th
        " `^\\\\==/".red(),
        "\"|,".bold(), // 6th
        "  //".red(),
        "     '\\\\".bold(), // 7th
        "  ¯".red(),
        "        `¯".bold(), // 8th
        "opensource".red().bold()
    );
}

static ABOUT: std::sync::LazyLock<String> = std::sync::LazyLock::new(|| {
    format!("\n{}\n| {}\n|  {}\t{}\n",
    "The Rux programming language.\n•--------------------------------------------------------------------------------".bold(),
    "Usage:",
    "rux build \tpath_to_your_project",
    "to build an executable from your project"
)
});

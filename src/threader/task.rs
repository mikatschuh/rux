use super::files::*;
use crate::{
    error::{CliError, Errors},
    format_time,
    parser::{tokenizing::Tokenizer, tree::TreeDisplay, typing::TypeParser, Parser},
    utilities::Rc,
};
use std::{
    ffi::OsString,
    fs::*,
    io::Read,
    path::{Path, PathBuf},
    time::{Instant, UNIX_EPOCH},
};

#[derive(Clone, Debug)]
pub enum Task {
    ReadDir { path: PathBuf },
    Parse { path: &'static Path },
}

impl Task {
    pub(super) fn run<F>(self, mut push: F) -> Result<(), CliError>
    where
        F: FnMut(Task),
    {
        match self {
            Task::ReadDir { path } => {
                let mut relevant_files = Directory::new();
                for entry in read_dir(&path)? {
                    let Ok(entry) = entry else { continue };
                    let path = entry.path();
                    let name: String;
                    if let Some(os_str) = path.file_name() {
                        if let Some(str_name) = os_str.to_str() {
                            match str_name.strip_prefix(".") {
                                Some(str) => name = String::from(str),
                                None => name = String::from(str_name),
                            }
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }

                    if path.is_dir() {
                        push(Task::ReadDir { path });
                    } else if let Some(extension) = path.extension() {
                        let Ok(last_changed) =
                            path.metadata().and_then(|metadata| metadata.modified())
                        else {
                            continue;
                        };

                        if extension == COMPILED_FILE_EXTENSION {
                            relevant_files.insert_object_file(
                                name.strip_suffix(&COMPILED_FILE_EXTENSION)
                                    .unwrap()
                                    .to_string(),
                                last_changed,
                            )
                        } else if extension == FILE_EXTENSION {
                            relevant_files.insert_project_file(
                                name.strip_suffix(&FILE_EXTENSION).unwrap().to_string(),
                                last_changed,
                            )
                        }
                    }
                }
                for file in relevant_files.files.into_iter() {
                    if file.1 != UNIX_EPOCH {
                        println!(
                            "wants to remove: {:?} name: {}",
                            path.join(String::from(".") + &file.0 + COMPILED_FILE_EXTENSION),
                            file.0
                        );
                        // match fs::remove_file(&path.join(file)) {
                        //     Ok(_) => {},
                        //     Err(_) => println!("You shall not remove that file! file: {:?}", path)
                        // }
                    } else {
                        push(Task::Parse {
                            path: Box::leak(
                                path.join(PathBuf::from(file.0 + FILE_EXTENSION))
                                    .into_boxed_path(),
                            ),
                        });
                    }
                }
            }
            Task::Parse { path } => {
                if path.file_name() != Some(&OsString::from("inter.rx")) {
                    return Ok(());
                }
                let mut file = OpenOptions::new().read(true).open(path)?;
                let mut content = String::new();
                file.read_to_string(&mut content)?;

                let now = Instant::now();

                let parsing_errors = Rc::new(Errors::empty(path));

                // lazy tokenizing
                let type_parser = TypeParser::new();
                let mut tokenizer = Tokenizer::new(&content, parsing_errors.clone(), type_parser);

                let ast = Parser::new(parsing_errors.clone()).parse(&mut tokenizer);

                // Debug Print
                let time = now.elapsed().as_nanos();
                for (sym, const_id) in ast.sym_const_table {
                    let name = ast.internalizer.resolve(sym);
                    let const_id_print = const_id.to_string();
                    println!(
                        "{} {} {}",
                        name,
                        const_id_print,
                        ast.consts[const_id].display(
                            &ast.internalizer,
                            &(name.chars().map(|_| " ").collect::<String>()
                                + "  "
                                + &const_id_print.chars().map(|_| " ").collect::<String>())
                        )
                    )
                }
                println!("\n{}", *parsing_errors);
                println!("\n\n{}", format_time(time))
            }
        }
        Ok(())
    }

    pub fn read_dir(path: Option<String>) -> Result<Task, CliError> {
        let path = match path {
            // determine path:
            Some(path) => {
                let path = PathBuf::from(path);
                if !path.exists() {
                    return Err(CliError::CommandLine(
                        "the given path seems to be not existing",
                    ));
                }
                if !path.is_dir() {
                    return Err(CliError::CommandLine(
                        "the given path seems to not be a directory",
                    ));
                }
                path
            }
            None => PathBuf::from("."),
        };
        Ok(Task::ReadDir { path })
    }
}

use super::files::*;
use crate::{
    error::{CliError, Errors},
    grapher, parser,
    ref_count::Rc,
    tokenizing,
};
use std::{
    ffi::OsString,
    fs::*,
    io::Read,
    path::{Path, PathBuf},
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
                let (files_to_compile, files_to_remove) = relevant_files.get_resulting();
                for file in files_to_compile {
                    push(Task::Parse {
                        path: Box::leak(
                            path.join(PathBuf::from(file + FILE_EXTENSION))
                                .into_boxed_path(),
                        ),
                    });
                }
                for file in files_to_remove {
                    println!(
                        "wants to remove: {:?} name: {}",
                        path.join(String::from(".") + &file + COMPILED_FILE_EXTENSION),
                        file
                    );

                    // match fs::remove_file(&path.join(file)) {
                    //     Ok(_) => {},
                    //     Err(_) => println!("You shall not remove that file! file: {:?}", path)
                    // }
                }
            }
            Task::Parse { path } => {
                if path.file_name() != Some(&OsString::from("test.rx")) {
                    return Ok(());
                }
                let mut file = OpenOptions::new().read(true).open(path)?;
                let mut content = String::new();
                file.read_to_string(&mut content)?;
                let content = content.leak();

                // let now = Instant::now();

                let errors = Rc::new(Errors::empty(path));

                // lazy tokenizing
                let mut tokenizer = tokenizing::Tokenizer::new(content, errors.clone(), 64);
                let mut parser = parser::Parser::new(&mut tokenizer, errors.clone());
                parser.parse_file();
                let parser_output = parser.symbol_table();
                let (graph_dump, interner) =
                    grapher::build_graph_debug(parser_output, "main", errors.clone())
                        .expect("graph");

                // Debug Print
                // let time = now.elapsed().as_nanos();

                if errors.is_empty() {
                    println!("{}", graph_dump);
                } else {
                    println!("{}", errors.display(&interner));
                }
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

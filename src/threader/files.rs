use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

pub(super) const FILE_EXTENSION: &str = "rx";
pub(super) const COMPILED_FILE_EXTENSION: &str = "o";

pub(super) struct Directory {
    pub(super) files: HashMap<String, SystemTime>,
}
impl Directory {
    pub fn new() -> Directory {
        Directory {
            files: HashMap::new(),
        }
    }
    pub fn insert_object_file(&mut self, name: String, created: SystemTime) {
        match self.files.get(&name) {
            // the corresponding project file was already found
            Some(entry) => {
                if *entry >= created {
                    // the project file is older

                    // insert the object file - the file has to be recompiled
                    self.files.insert(name, UNIX_EPOCH)
                } else {
                    // the project is younger

                    // remove the file it was compiled recently
                    self.files.remove(&name)
                }
            }
            // corresponding project file wasnt found
            None => self.files.insert(name, created),
        };
    }
    pub fn insert_project_file(&mut self, name: String, created: SystemTime) {
        match self.files.get(&name) {
            // the corresponding object file was already found
            Some(entry) => {
                if *entry < created {
                    // the object is older then the project file

                    // confirm the object file - it is there with good reason
                    self.files.insert(name, UNIX_EPOCH)
                } else {
                    // the object file is younger then the project file

                    self.files.remove(&name) // remove the entry as the project file was already compiled recently
                }
            }

            // the corresponding object file wasnt found, but could
            None => self.files.insert(name, UNIX_EPOCH),
        };
    }
}

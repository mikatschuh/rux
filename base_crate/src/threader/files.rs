use std::collections::HashMap;
use std::time::SystemTime;

pub(super) const FILE_EXTENSION: &str = "rx";
pub(super) const COMPILED_FILE_EXTENSION: &str = "o";

pub(super) struct Directory {
    project_files: HashMap<String, SystemTime>,
    object_files: HashMap<String, SystemTime>,
    files_to_recompile: Vec<String>,
}

impl Directory {
    pub fn new() -> Directory {
        Directory {
            project_files: HashMap::new(),
            object_files: HashMap::new(),
            files_to_recompile: vec![],
        }
    }

    pub fn get_resulting(mut self) -> (Vec<String>, Vec<String>) {
        self.files_to_recompile
            .append(&mut self.project_files.into_keys().collect());
        (
            self.files_to_recompile,
            self.object_files.into_keys().collect(),
        )
    }

    pub fn insert_object_file(&mut self, name: String, obj_file_creation: SystemTime) {
        match self.project_files.get(&name) {
            Some(project_file_edit) => {
                if project_file_edit.duration_since(obj_file_creation).is_ok() {
                    self.project_files.remove(&name);
                    self.object_files.remove(&name);

                    self.files_to_recompile.push(name);
                }
            }
            None => {
                self.object_files.insert(name, obj_file_creation);
            }
        };
    }

    pub fn insert_project_file(&mut self, name: String, project_file_edit: SystemTime) {
        match self.object_files.get(&name) {
            Some(obj_file_creation) => {
                if project_file_edit.duration_since(*obj_file_creation).is_ok() {
                    self.project_files.remove(&name);
                    self.object_files.remove(&name);

                    self.files_to_recompile.push(name);
                }
            }
            None => {
                self.project_files.insert(name, project_file_edit);
            }
        };
    }
}

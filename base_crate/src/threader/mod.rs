mod files;
mod task;

use crate::{error::CliError, *};
use crossbeam::deque::Injector;
use std::{
    sync::Arc,
    thread::{Builder, JoinHandle},
};
use task::Task;

#[derive(Debug)]
pub struct Threadpool {
    workers: Vec<JoinHandle<Result<(), CliError>>>,
    task_queue: Arc<Injector<Task>>,
}
impl Threadpool {
    pub fn new() -> Self {
        Self {
            workers: Vec::new(),
            task_queue: Arc::new(Injector::new()),
        }
    }
    pub fn launch(&mut self, num_threads: Option<usize>) {
        for i in 0..num_threads.unwrap_or_else(num_cpus::get) {
            let task_queue = self.task_queue.clone();

            let Ok(join_handle) = Builder::new().name(format!("{i}")).spawn(move || {
                while let Some(task) = task_queue.steal().success() {
                    task.run(|new_task| task_queue.push(new_task))?;
                }
                Ok(())
            }) else {
                println!("thread {i} couldnt been spawned");
                continue;
            };

            self.workers.push(join_handle);
        }
    }
    pub fn start_up(&mut self, path: Option<String>) -> Result<(), CliError> {
        self.task_queue.push(Task::read_dir(path)?);
        Ok(())
    }
    pub fn drop(self) -> Result<(), error::CliError> {
        for thread in self.workers.into_iter() {
            thread.join()??
        }
        Ok(())
    }
}

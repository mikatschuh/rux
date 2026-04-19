use std::collections::HashMap;

use nonempty::NonEmpty;

use crate::{error::Span, grapher::graph::ValueID};

pub type Overwrites<'src> = HashMap<&'src str, (ValueID<'src>, NonEmpty<Span>)>;

pub struct Branches<'src> {
    overwrites: NonEmpty<Overwrites<'src>>,
    scopes_within_branch: NonEmpty<usize>,
}

impl<'src> Branches<'src> {
    pub fn new() -> Self {
        Self {
            overwrites: NonEmpty::new(HashMap::new()),
            scopes_within_branch: NonEmpty::new(1),
        }
    }

    pub fn open_branch(&mut self) {
        self.overwrites.push(HashMap::new());
        self.scopes_within_branch.push(0);
    }

    #[must_use]
    pub fn close_branch(&mut self) -> Overwrites<'src> {
        self.scopes_within_branch.pop();
        self.overwrites.pop().unwrap()
    }

    pub fn open_scope(&mut self) {
        *self.scopes_within_branch.last_mut() += 1;
    }

    pub fn close_scope(&mut self) {
        *self.scopes_within_branch.last_mut() -= 1;
    }

    pub fn scopes_within_branch(&self) -> usize {
        *self.scopes_within_branch.last()
    }

    pub fn add(&mut self, assignment: Span, name: &'src str, value: ValueID<'src>) {
        if let Some((prev_value, assignments)) = self.overwrites.last_mut().get_mut(name) {
            *prev_value = value;
            assignments.push(assignment);
        } else {
            self.overwrites
                .last_mut()
                .insert(name, (value, NonEmpty::new(assignment)));
        }
    }
}

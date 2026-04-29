use nonempty::NonEmpty;

use crate::grapher::graph::CtrlID;

pub struct JumpTableStack<'src, T> {
    branches: NonEmpty<(Vec<CtrlID<'src>>, Vec<T>)>,
}

impl<'src, T> JumpTableStack<'src, T> {
    pub fn new() -> Self {
        Self {
            branches: NonEmpty::new((vec![], vec![])),
        }
    }

    pub fn open_branch(&mut self) {
        self.branches.push((vec![], vec![]))
    }

    #[must_use]
    pub fn close_branch(&mut self) -> (Vec<CtrlID<'src>>, Vec<T>) {
        self.branches.pop().unwrap()
    }

    pub fn add_jump(&mut self, ctrl: CtrlID<'src>, data: T) {
        self.branches.last_mut().0.push(ctrl);
        self.branches.last_mut().1.push(data);
    }
}

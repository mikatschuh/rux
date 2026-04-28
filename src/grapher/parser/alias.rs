use std::ptr::NonNull;

use crate::grapher::graph::DataID;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Alias<'src> {
    inner: NonNull<DataID<'src>>,
}

impl<'src> Alias<'src> {
    pub fn new(symbol_ref: &DataID<'src>) -> Self {
        Self {
            inner: NonNull::new(symbol_ref as *const DataID<'src> as *mut DataID<'src>).unwrap(),
        }
    }

    pub fn read_current_value(&self) -> DataID<'src> {
        unsafe { (&*self.inner.as_ptr()).clone() }
    }

    pub fn over_write(self, new_value: DataID<'src>) {
        unsafe {
            self.inner.write(new_value);
        }
    }
}

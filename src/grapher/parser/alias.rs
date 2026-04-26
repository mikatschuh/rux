use std::ptr::NonNull;

use crate::grapher::graph::ValueID;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Alias<'src> {
    inner: NonNull<ValueID<'src>>,
}

impl<'src> Alias<'src> {
    pub fn new(symbol_ref: &ValueID<'src>) -> Self {
        Self {
            inner: NonNull::new(symbol_ref as *const ValueID<'src> as *mut ValueID<'src>).unwrap(),
        }
    }

    pub fn read_current_value(&self) -> ValueID<'src> {
        unsafe { self.inner.read() }
    }

    pub fn over_write(self, new_value: ValueID<'src>) {
        unsafe {
            self.inner.write(new_value);
        }
    }
}

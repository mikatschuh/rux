use std::{marker::PhantomData, slice::from_raw_parts};

pub struct TokenSlice<'src> {
    phantom: PhantomData<&'src ()>,

    ptr: *const u8,
    len: usize,
}

impl<'src> TokenSlice<'src> {
    pub fn new(slice: &'src [u8]) -> Self {
        Self {
            phantom: PhantomData::default(),

            ptr: slice.as_ptr(),
            len: slice.len(),
        }
    }

    pub fn push_byte_over(&mut self, b: &mut &'src [u8]) {
        self.len += 1;
        *b = &b[1..];
    }

    pub fn to_slice(self) -> &'src [u8] {
        unsafe { from_raw_parts(self.ptr, self.len) }
    }

    pub fn to_str(self) -> &'src str {
        unsafe { str::from_utf8_unchecked(self.to_slice()) }
    }
}

pub fn is_unicode_payload_byte(byte: u8) -> bool {
    byte & 0b1100_0000 == 0b1000_0000
}

pub fn whitespace_at_start_or_empty(slice: &[u8]) -> bool {
    if slice.is_empty() {
        return true;
    }

    match slice[0] {
        0x09..=0x0D | 0x20 => true,

        0xC2 => {
            if slice.len() < 2 {
                return true;
            }
            matches!(slice[1], 0x85 | 0xA0)
        }
        0xE1 => {
            if slice.len() < 3 {
                return true;
            }
            slice[1] == 0x9A && slice[2] == 0x80
        }
        0xE2 => {
            if slice.len() < 3 {
                return true;
            }
            (slice[1] == 0x80 && matches!(slice[2], 0x80..=0x8A | 0xA8 | 0xA9 | 0xAF))
                || (slice[1] == 0x81 && slice[2] == 0x9F)
        }
        0xE3 => {
            if slice.len() < 3 {
                return true;
            }
            slice[1] == 0x80 && slice[2] == 0x80
        }
        _ => false,
    }
}

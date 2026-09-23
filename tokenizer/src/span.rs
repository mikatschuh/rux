use std::ops::{Add, AddAssign, Sub, SubAssign};
use std::path::Path;

fn remove_quotes(path: &Path) -> String {
    String::from(
        format!("{:?}", path.as_os_str())
            .strip_prefix("\"")
            .unwrap()
            .strip_suffix("\"")
            .unwrap(),
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Position {
    collum: usize,
    line: usize,
}
impl Position {
    pub(crate) fn beginning() -> Self {
        Position { collum: 1, line: 1 }
    }
    pub(crate) fn next_line(&mut self) {
        self.line += 1;
        self.collum = 1;
    }
}

impl Add<usize> for Position {
    type Output = Self;
    fn add(mut self, rhs: usize) -> Self::Output {
        self.collum += rhs;
        self
    }
}
impl Sub<usize> for Position {
    type Output = Self;
    fn sub(mut self, rhs: usize) -> Self::Output {
        self.collum -= rhs;
        self
    }
}
impl AddAssign<usize> for Position {
    fn add_assign(&mut self, rhs: usize) {
        self.collum += rhs;
    }
}
impl SubAssign<usize> for Position {
    fn sub_assign(&mut self, rhs: usize) {
        self.collum -= rhs;
    }
}

impl Sub<Position> for Position {
    type Output = Span;
    /// subtracts the two positions
    fn sub(self, rhs: Position) -> Self::Output {
        Span {
            start: self,
            end: rhs,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub(crate) start: Position,
    pub(crate) end: Position,
}

impl Span {
    pub fn get(self) -> (usize, usize, usize, usize) {
        (
            self.start.collum,
            self.start.line,
            self.end.collum,
            self.end.line,
        )
    }
}

impl From<Position> for Span {
    fn from(pos: Position) -> Self {
        Span {
            start: pos,
            end: pos,
        }
    }
}

impl Sub<Span> for Span {
    type Output = Span;
    /// combines the two spans
    fn sub(mut self, rhs: Span) -> Self::Output {
        self.end.line = rhs.end.line;
        self.end.collum = rhs.end.collum;
        self
    }
}

impl Span {
    pub fn to_string(self, path: &Path) -> String {
        match self.start.line == self.end.line {
            true => match self.start.collum == self.end.collum {
                true => format!(
                    "at {}:{}:{}",
                    remove_quotes(path),
                    self.start.line,
                    self.start.collum
                ),
                false => format!(
                    "at {}:{}:{} - {}",
                    remove_quotes(path),
                    self.start.line,
                    self.start.collum,
                    self.end.collum
                ),
            },
            false => format!(
                "at {}:{}:{}-{}:{}",
                remove_quotes(path),
                self.start.line,
                self.start.collum,
                self.end.line,
                self.end.collum
            ),
        }
    }
}

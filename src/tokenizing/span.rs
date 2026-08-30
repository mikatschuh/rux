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
pub struct Position {
    pub collum: usize,
    pub line: usize,
}
impl Position {
    #[inline]
    pub fn beginning() -> Self {
        Position { collum: 1, line: 1 }
    }
    #[inline]
    pub fn at(collum: usize, line: usize) -> Self {
        Position { collum, line }
    }
    #[inline]
    pub fn next_line(&mut self) {
        self.line += 1;
        self.collum = 1;
    }
    #[inline]
    pub fn at_next_line(mut self) -> Position {
        self.line += 1;
        self.collum = 1;
        self
    }
}

impl Add<usize> for Position {
    type Output = Self;
    #[inline]
    fn add(mut self, rhs: usize) -> Self::Output {
        self.collum += rhs;
        self
    }
}
impl Sub<usize> for Position {
    type Output = Self;
    #[inline]
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
    #[inline]
    fn sub(self, rhs: Position) -> Self::Output {
        Span {
            start: self,
            end: rhs,
        }
    }
}
impl Sub<Span> for Position {
    type Output = Span;
    fn sub(self, mut rhs: Span) -> Self::Output {
        rhs.start = self;
        rhs
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}
impl Span {
    #[inline]
    pub fn beginning() -> Self {
        Span {
            start: Position::beginning(),
            end: Position::beginning(),
        }
    }
    #[inline]
    pub fn at(start_collum: usize, start_line: usize, end_collum: usize, end_line: usize) -> Self {
        Self {
            start: Position::at(start_collum, start_line),
            end: Position::at(end_collum, end_line),
        }
    }
    #[inline]
    pub fn end(mut self) -> Self {
        self.start.line = self.end.line;
        self.start.collum = self.end.collum;
        self
    }
    #[inline]
    pub fn end_mut(&mut self) -> &mut Position {
        &mut self.end
    }
}
impl Add<usize> for Span {
    type Output = Self;
    #[inline]
    fn add(mut self, rhs: usize) -> Self::Output {
        self.end.collum += rhs;
        self
    }
}
impl Sub<usize> for Span {
    type Output = Self;
    #[inline]
    fn sub(mut self, rhs: usize) -> Self::Output {
        self.end.collum -= rhs;
        self
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
    #[inline]
    fn sub(mut self, rhs: Span) -> Self::Output {
        self.end.line = rhs.end.line;
        self.end.collum = rhs.end.collum;
        self
    }
}
impl SubAssign<Span> for Span {
    /// combines the two spans and stores the result in the first
    #[inline]
    fn sub_assign(&mut self, rhs: Span) {
        self.end = rhs.end
    }
}

impl Sub<Position> for Span {
    type Output = Self;
    fn sub(mut self, rhs: Position) -> Self::Output {
        self.end = rhs;
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

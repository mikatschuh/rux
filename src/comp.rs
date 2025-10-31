use std::{
    ops::{Index, IndexMut},
    option::IntoIter,
    slice::{Iter, IterMut},
    vec,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Vec<T, const MIN: usize>(vec::Vec<T>);

impl<T, const MIN: usize> Vec<T, MIN> {
    #[inline]
    pub fn new<const N: usize>(content: [T; N]) -> Self {
        assert!(N >= MIN);
        Self(vec::Vec::from(content))
    }
    #[inline]
    pub fn last(&self) -> &T {
        self.0.last().unwrap()
    }
    #[inline]
    pub fn last_mut(&mut self) -> &mut T {
        self.0.last_mut().unwrap()
    }
    #[inline]
    pub fn first(&self) -> &T {
        self.0.first().unwrap()
    }
    #[inline]
    pub fn first_mut(&mut self) -> &mut T {
        self.0.first_mut().unwrap()
    }
    #[inline]
    pub fn nth(&self, nth: usize) -> &T {
        &self.0[nth]
    }
    #[inline]
    pub fn nth_mut(&mut self, nth: usize) -> &mut T {
        &mut self.0[nth]
    }
    #[inline]
    pub fn penultimate(&self) -> Option<&T> {
        let len = self.0.len();
        if len > MIN {
            Some(&self.0[len - 2])
        } else {
            None
        }
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    #[inline]
    pub fn push(&mut self, val: T) {
        self.0.push(val)
    }
    #[inline]
    pub fn pop(&mut self) -> T {
        assert!(self.0.len() > MIN);
        self.0.pop().unwrap()
    }
    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        self.0.iter()
    }
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        self.0.iter_mut()
    }
}
impl<T, const N: usize, const MIN: usize> From<[T; N]> for Vec<T, MIN> {
    fn from(value: [T; N]) -> Self {
        assert!(N >= MIN);
        Self(vec::Vec::from(value))
    }
}
impl<'a, T, const MIN: usize> IntoIterator for &'a Vec<T, MIN> {
    type IntoIter = std::slice::Iter<'a, T>;
    type Item = &'a T;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl<'a, T: Clone, const MIN: usize> From<Iter<'a, T>> for Vec<T, MIN> {
    fn from(value: Iter<T>) -> Self {
        let mut vec = std::vec::Vec::new();
        value.for_each(|value| vec.push(value.clone()));
        assert!(vec.len() >= MIN);
        Self(vec)
    }
}
impl<T, const MIN: usize> From<IntoIter<T>> for Vec<T, MIN> {
    fn from(value: IntoIter<T>) -> Self {
        let mut vec = std::vec::Vec::new();
        value.for_each(|value| vec.push(value));
        assert!(vec.len() >= MIN);
        Self(vec)
    }
}
impl<T, const MIN: usize> FromIterator<T> for Vec<T, MIN> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut vec = std::vec::Vec::new();
        iter.into_iter().for_each(|value| vec.push(value));
        assert!(vec.len() >= MIN);
        Self(vec)
    }
}
impl<T, const MIN: usize> From<Vec<T, MIN>> for vec::Vec<T> {
    fn from(value: Vec<T, MIN>) -> Self {
        value.0
    }
}
impl<T, const MIN: usize> Index<usize> for Vec<T, MIN> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}
impl<T, const MIN: usize> IndexMut<usize> for Vec<T, MIN> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}
#[macro_export]
macro_rules! res_vec {
    [$($arg:expr),*] => {
        $crate::comp::Vec::from([$($arg), *])
    };
}

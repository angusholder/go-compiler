use std::fmt::{ self, Debug };
use std::iter::IntoIterator;
use std::ops::{ Deref };
use std::vec::IntoIter;

pub type P<T> = Box<T>;

#[allow(non_snake_case)]
pub fn P<T>(t: T) -> P<T> {
    Box::new(t)
}

#[derive(Clone, PartialEq)]
pub struct List<T> {
    inner: Box<[T]>,
}

impl<T> From<Vec<T>> for List<T> {
    fn from(t: Vec<T>) -> Self {
        List {
            inner: t.into_boxed_slice()
        }
    }
}

impl<T> Deref for List<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.inner[..]
    }
}

impl<T> IntoIterator for List<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_vec().into_iter()
    }
}

impl<T: Debug> Debug for List<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.inner[..], f)
    }
}


use std::fmt::{ self, Debug };
use std::ops::Deref;

pub struct P<T> {
    inner: Box<T>,
}

#[allow(non_snake_case)]
pub fn P<T>(t: T) -> P<T> {
    P {
        inner: Box::new(t)
    }
}

impl<T> Deref for P<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Debug> Debug for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

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

impl<T: Debug> Debug for List<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.inner[..], f)
    }
}
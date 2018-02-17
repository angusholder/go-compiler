use std::cell::RefCell;
use std::cmp::{ PartialEq, PartialOrd, Ordering };
use std::fmt::{ self, Display, Debug };
use std::hash::{ Hash, Hasher };
use std::marker::PhantomData;
use std::mem;
use std::ops::{ Deref, DerefMut };

use fnv::FnvBuildHasher;
use string_interner::{ StringInterner as StringInterner_, Symbol };

type StringInterner = StringInterner_<Atom, FnvBuildHasher>;

thread_local! {
    static STRING_INTERNER: RefCell<StringInterner>
        = RefCell::new(StringInterner::with_hasher(FnvBuildHasher::default()));
}

fn with_interner<F, R>(f: F) -> R
where F: Fn(&StringInterner) -> R {
    STRING_INTERNER.with(|interner| {
        let interner = interner.borrow();
        f(interner.deref())
    })
}

fn with_interner_mut<F, R>(f: F) -> R
    where F: Fn(&mut StringInterner) -> R {
    STRING_INTERNER.with(|interner| {
        let mut interner = interner.borrow_mut();
        f(interner.deref_mut())
    })
}

#[derive(Clone, Copy, Eq, Ord)]
pub struct Atom {
    index: u32,
    _phantom: PhantomData<*const str>,
}

pub fn Atom(s: &str) -> Atom {
    Atom::from(s)
}

impl Atom {
    pub fn as_str(&self) -> &str {
        with_interner(|interner| {
            let s = interner.resolve(*self).unwrap();
            unsafe {
                mem::transmute::<_, &'static str>(s)
            }
        })
    }
}

impl Symbol for Atom {
    fn from_usize(val: usize) -> Self {
        Atom {
            index: val as u32,
            _phantom: PhantomData,
        }
    }

    fn to_usize(self) -> usize {
        self.index as usize
    }
}

impl<'a> From<&'a str> for Atom {
    fn from(s: &'a str) -> Self {
        with_interner_mut(|interner| {
            interner.get_or_intern(s)
        })
    }
}

impl Deref for Atom {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl AsRef<str> for Atom {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Hash for Atom {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let s: &str = self.as_ref();
        s.hash(state);
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl PartialOrd for Atom {
    fn partial_cmp(&self, other: &Atom) -> Option<Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl<'a> PartialEq<&'a str> for Atom {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Atom({})", self)
    }
}


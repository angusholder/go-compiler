use std::cell::RefCell;
use std::cmp::{ PartialEq, PartialOrd, Ordering };
use std::fmt::{ self, Display, Debug };
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
        with_interner(|interner| {
            let s = interner.resolve(*self).unwrap();
            unsafe {
                mem::transmute::<_, &'static str>(s)
            }
        })
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl PartialOrd for Atom {
    fn partial_cmp(&self, other: &Atom) -> Option<Ordering> {
        <Self as AsRef<str>>::as_ref(self).partial_cmp(other)
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


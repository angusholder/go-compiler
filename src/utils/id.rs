use std::hash::Hash;
use std::ops::{ Index, IndexMut };
use std::marker::PhantomData;

use fnv::FnvHashMap;
use vec_map::VecMap;
use std::slice::Iter;
use std::iter::Enumerate;

pub trait Id: Copy + Eq + Hash {
    fn as_usize(self) -> usize;
    fn from_usize(n: usize) -> Self;
}

macro_rules! impl_id {
    ($ty:ident) => {
        impl $crate::utils::id::Id for $ty {
            fn as_usize(self) -> usize {
                self.0 as usize
            }

            fn from_usize(n: usize) -> $ty {
                $ty(n as u32)
            }
        }
    };
}



pub struct IdGenerator<T: Id> {
    count: u32,
    _unused: PhantomData<T>,
}

impl<T: Id> IdGenerator<T> {
    pub fn new() -> IdGenerator<T> {
        IdGenerator {
            count: 0,
            _unused: PhantomData,
        }
    }

    pub fn next(&mut self) -> T {
        let result = T::from_usize(self.count as usize);
        self.count += 1;
        result
    }

    pub fn reset(&mut self) {
        self.count = 0;
    }
}



pub struct IdVecMap<K: Id, V> {
    map: VecMap<V>,
    _unused: PhantomData<K>,
}

impl<K: Id, V> IdVecMap<K, V> {
    pub fn new() -> IdVecMap<K, V> {
        IdVecMap {
            map: VecMap::default(),
            _unused: PhantomData,
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.map.insert(key.as_usize(), value);
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.map.get(key.as_usize())
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.map.get_mut(key.as_usize())
    }

    pub fn next_key(&self) -> K {
        K::from_usize(self.map.keys().last().unwrap_or(0))
    }
}

impl<K: Id, V> Index<K> for IdVecMap<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.map[index.as_usize()]
    }
}

impl<K: Id, V> IndexMut<K> for IdVecMap<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.map[index.as_usize()]
    }
}



pub struct IdVec<K: Id, V> {
    list: Vec<V>,
    _unused: PhantomData<K>,
}

impl<K: Id, V> IdVec<K, V> {
    pub fn new() -> IdVec<K, V> {
        IdVec {
            list: Vec::new(),
            _unused: PhantomData,
        }
    }
}

impl<K: Id, V: Default + Clone> IdVec<K, V> {
    pub fn with_len(len: usize) -> IdVec<K, V> {
        IdVec {
            list: vec![V::default(); len],
            _unused: PhantomData,
        }
    }
}

impl<K: Id, V> IdVec<K, V> {
    pub fn get(&self, key: K) -> Option<&V> {
        self.list.get(key.as_usize())
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.list.get_mut(key.as_usize())
    }

    pub fn append(&mut self, value: V) -> K {
        let key = K::from_usize(self.list.len());
        self.list.push(value);
        key
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn into_vec(self) -> Vec<V> {
        self.list
    }

    pub fn iter(&self) -> IdVecIter<K, V> {
        IdVecIter {
            iter: self.list.iter().enumerate(),
            _unused: PhantomData,
        }
    }
}

pub struct IdVecIter<'a, K, V: 'a> {
    iter: Enumerate<Iter<'a, V>>,
    _unused: PhantomData<K>,
}

impl<'a, K: Id, V: 'a> Iterator for IdVecIter<'a, K, V> {
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(i, v)| (K::from_usize(i), v))
    }
}

impl<K: Id, V> Index<K> for IdVec<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.list[index.as_usize()]
    }
}

impl<K: Id, V> IndexMut<K> for IdVec<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.list[index.as_usize()]
    }
}



pub struct IdHashMap<K: Id, V> {
    map: FnvHashMap<K, V>,
}

impl<K: Id, V> IdHashMap<K, V> {
    pub fn new() -> IdHashMap<K, V> {
        IdHashMap {
            map: FnvHashMap::default(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.map.insert(key, value);
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.map.get(&key)
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.map.get_mut(&key)
    }
}

impl<K: Id, V> Index<K> for IdHashMap<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<K: Id, V> IndexMut<K> for IdHashMap<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}
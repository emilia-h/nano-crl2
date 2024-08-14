
use std::cell::RefCell;
use std::collections::hash_map::HashMap;
use std::hash::{Hash, Hasher};
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::sync::Arc;

#[derive(Clone)]
pub struct HashByAddress<T>(T);

impl<T: Deref> HashByAddress<T> {
    pub fn get_address_value(&self) -> usize {
        self.0.deref() as *const <T as Deref>::Target as *const () as usize
    }
}

impl<T: Deref + Debug> Debug for HashByAddress<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        self.0.fmt(f)
    }
}

impl<T: Deref> PartialEq for HashByAddress<T> {
    fn eq(&self, other: &Self) -> bool {
        self.get_address_value() == other.get_address_value()
    }
}

impl<T: Deref> Eq for HashByAddress<T> {}

impl<T: Deref> Hash for HashByAddress<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.get_address_value())
    }
}

#[derive(Eq, PartialEq)]
pub struct Interned<T>(HashByAddress<Arc<T>>);

impl<T> Interned<T> {
    pub fn new(value: T) -> Self {
        Interned(HashByAddress(Arc::new(value)))
    }
}

impl<T> Clone for Interned<T> {
    fn clone(&self) -> Self {
        Interned(HashByAddress(Arc::clone(&self.0.0)))
    }
}

impl<T: Debug> Debug for Interned<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        self.0.0.fmt(f)
    }
}

impl<T> Deref for Interned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0.0.deref()
    }
}

impl<T> Hash for Interned<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

/// A cache for queries.
/// 
/// This map uses interior mutability, so an immutable reference to it can be
/// passed around and used to lock a key or insert a value for a key.
pub struct QueryHashMap<K, V> {
    map: RefCell<HashMap<K, Lockable<V>>>,
}

impl<K, V> QueryHashMap<K, V> {
    pub fn new() -> Self {
        QueryHashMap {
            map: RefCell::new(HashMap::new())
        }
    }
}

impl<K, V> QueryHashMap<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    /// Gives back the (cloned) value for the given key, or locks the key if
    /// there was no value for that key.
    /// 
    /// If the key already had a value, then this returns a successful result.
    /// 
    /// if the key did not already have an associated value, then this locks
    /// that key and returns a successful but empty value (`Ok(None)`).
    /// 
    /// If the key was already locked, then this returns an error, because that
    /// indicates a cyclic query.
    pub fn get_or_lock(&self, key: &K) -> Result<Option<V>, ()> {
        let mut map = self.map.borrow_mut();
        let value = map.get(key);
        match value {
            None => {
                map.insert(key.clone(), Lockable::Locked);
                Ok(None)
            },
            Some(Lockable::Locked) => Err(()),
            Some(Lockable::Filled(v)) => Ok(Some(v.clone())),
        }
    }
}

impl<K, V> QueryHashMap<K, V>
where
    K: Hash + Eq,
{
    /// Inserts the value for a previously locked key.
    /// 
    /// # Panics
    /// This function requires that the key was locked before calling this
    /// function, and will panic if it wasn't. This is because this situation
    /// indicates either an unhandled cyclic query error or a query that was
    /// not cached while it should be.
    /// 
    /// It also requires that there was not a value for that key before, so it
    /// will panic in that case as well.
    pub fn unlock(&self, key: &K, value: V) {
        let mut map = self.map.borrow_mut();
        if let Some(v) = map.get_mut(key) {
            assert!(matches!(v, Lockable::Locked), "already had a value");
            *v = Lockable::Filled(value);
        } else {
            panic!("not locked");
        }
    }

    /// Will remove a cached entry from the map.
    /// 
    /// If the entry was locked, this will also remove it.
    pub fn invalidate(&self, key: &K) {
        let mut map = self.map.borrow_mut();
        map.remove(key);
    }
}

#[derive(Debug)]
enum Lockable<V> {
    Locked,
    Filled(V),
}

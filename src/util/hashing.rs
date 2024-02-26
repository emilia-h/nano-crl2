
use std::hash::{Hash, Hasher};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

pub trait Addressable: Clone {
    fn to_pointer_value(&self) -> usize;
}

impl<T> Addressable for Rc<T> {
    fn to_pointer_value(&self) -> usize {
        Rc::as_ptr(self) as usize
    }
}

#[derive(Clone)]
pub struct HashByAddress<P: Addressable> {
    value: P,
}

impl<P: Addressable> HashByAddress<P> {
    pub fn new(value: P) -> Self {
        HashByAddress { value }
    }
}

impl<P: Addressable + Debug> Debug for HashByAddress<P> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        self.value.fmt(f)
    }
}

impl<P: Addressable> PartialEq for HashByAddress<P> {
    fn eq(&self, other: &Self) -> bool {
        self.value.to_pointer_value() == other.value.to_pointer_value()
    }
}

impl<P: Addressable> Eq for HashByAddress<P> {}

impl<P: Addressable> Hash for HashByAddress<P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.value.to_pointer_value())
    }
}

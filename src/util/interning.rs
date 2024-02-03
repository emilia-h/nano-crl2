
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub trait Internable: Clone {
    fn to_pointer_value(&self) -> usize;
}

impl<T> Internable for Rc<T> {
    fn to_pointer_value(&self) -> usize {
        Rc::as_ptr(self) as usize
    }
}

#[derive(Clone)]
pub struct Interned<P: Internable> {
    value: P,
}

impl<P: Internable> Interned<P> {
    pub fn new(value: P) -> Self {
        Interned { value }
    }
}

impl<P: Internable> PartialEq for Interned<P> {
    fn eq(&self, other: &Self) -> bool {
        self.value.to_pointer_value() == other.value.to_pointer_value()
    }
}

impl<P: Internable> Eq for Interned<P> {}

impl<P: Internable> Hash for Interned<P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.value.to_pointer_value())
    }
}

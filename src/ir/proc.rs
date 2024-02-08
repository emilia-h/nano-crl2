
use crate::ir::module::ModuleId;

pub struct IrProc {

}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ProcId {
    pub(crate) module: ModuleId,
    pub(crate) value: usize,
}

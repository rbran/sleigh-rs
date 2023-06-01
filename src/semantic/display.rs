use crate::semantic::{
    disassembly, ContextId, InstNext, InstStart, TableId, TokenFieldId,
    VarnodeId,
};

#[derive(Clone, Debug, Default)]
pub struct Display {
    pub mneumonic: Option<String>,
    pub(crate) elements: Box<[DisplayElement]>,
}

impl Display {
    pub fn elements(&self) -> impl Iterator<Item = &DisplayElement> {
        self.elements.iter()
    }
}

#[derive(Clone, Debug)]
pub enum DisplayElement {
    Varnode(VarnodeId),
    Context(ContextId),
    //Bitrange(BitrangeId),
    TokenField(TokenFieldId),
    InstStart(InstStart),
    InstNext(InstNext),
    Table(TableId),
    Disassembly(disassembly::VariableId),
    Literal(String),
    Space,
}

impl DisplayElement {
    pub fn is_space(&self) -> bool {
        matches!(self, Self::Space)
    }
}

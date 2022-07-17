use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    FuncDecl {
        name: String,
        ret_type: String,
        children: Vec<Node>,
    },
    Return(Rc<Node>),
    Constant(Val),
}

/// Returns an empty function declaration with just a name, and a return type.
pub fn empty_func_decl(name: String, ret_type: String) -> Node {
    Node::FuncDecl {
        name,
        ret_type,
        children: Vec::new(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    I16(i16),
    I64(i64),
}

use rustc_data_structures::fx::FxHashMap;
use std::rc::Rc;
use syntax::ast;

pub struct IdTree {
    root: Node,
    id_map: FxHashMap<ast::NodeId, codepoint>,
}

pub struct Node {
    parent: ast::NodeId,
}

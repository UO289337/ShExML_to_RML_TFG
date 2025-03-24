use std::cell::RefCell;

pub struct ASTTree {
    root: ASTNode,
}

pub enum ASTNodeType {
    Program,
    Pefix,
    Source,
}

impl ASTTree {
    pub fn new(root: ASTNode) -> Self {
        ASTTree { 
            root 
        }
    }

    pub fn access_tree(self) -> ASTNode {
        self.root
    }
}

pub struct ASTNode {
    node_type: ASTNodeType,
    children: RefCell<Vec<ASTNode>>,
    data: String,
}

impl ASTNode {
    pub fn new(node_type: ASTNodeType, data: String) -> Self {
        ASTNode { 
            node_type, 
            children: RefCell::new(Vec::new()), 
            data,
        }
    }

    pub fn add_child(&mut self, child: ASTNode) {
        self.children.borrow_mut().push(child);
    }
}
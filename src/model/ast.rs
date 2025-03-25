use std::cell::RefCell;

pub struct ASTTree {
    root: ASTNode,
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

pub trait ASTNodeTrait {
    fn add_child(&self, child: ASTNode);
}

pub enum ASTNode {
    File(FileASTNode),
    Prefix(PrefixASTNode),
    Source(SourceASTNode),
}

pub struct FileASTNode {
    children: RefCell<Vec<ASTNode>>,
}

impl ASTNodeTrait for FileASTNode {
    fn add_child(&self, child: ASTNode) {
        self.children.borrow_mut().push(child);
    }
}

impl FileASTNode {
    pub fn new() -> Self {
        FileASTNode { 
            children: RefCell::new(Vec::new()) 
        }
    }
}

pub struct PrefixASTNode {
    children: RefCell<Vec<ASTNode>>,
    identifier: String,
    uri: String,
}

impl ASTNodeTrait for PrefixASTNode {
    fn add_child(&self, child: ASTNode) {
        self.children.borrow_mut().push(child);
    }
}

impl PrefixASTNode {
    pub fn new(identifier: String, uri: String) -> Self {
        PrefixASTNode {
            children: RefCell::new(Vec::new()),
            identifier,
            uri,
        }
    }
}

pub struct SourceASTNode {
    children: RefCell<Vec<ASTNode>>,
    identifier: String,
    uri: String,
}

impl ASTNodeTrait for SourceASTNode {
    fn add_child(&self, child: ASTNode) {
        self.children.borrow_mut().push(child);
    }
}

impl SourceASTNode {
    pub fn new(identifier: String, uri: String) -> Self {
        SourceASTNode {
            children: RefCell::new(Vec::new()),
            identifier,
            uri,
        }
    }
}
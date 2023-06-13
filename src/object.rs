use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::ast::{Block, Identifier};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Function(Function),
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(b) => *b,
            Object::Null => false,
            _ => true,
        }
    }
}

impl Object {
    pub fn is_integer(&self) -> bool {
        matches!(self, Object::Integer(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Object::Boolean(_))
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Object::Null)
    }

    pub fn is_return_value(&self) -> bool {
        matches!(self, Object::ReturnValue(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Object::Function(_))
    }
}

impl From<bool> for Object {
    fn from(b: bool) -> Self {
        Object::Boolean(b)
    }
}

impl From<i64> for Object {
    fn from(i: i64) -> Self {
        Object::Integer(i)
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Function(_f) => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Environment {
    store: Rc<RefCell<HashMap<String, Object>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.store.borrow().get(name).cloned()
    }

    pub fn set(&mut self, name: &str, obj: Object) {
        self.store.borrow_mut().insert(name.to_string(), obj);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    parameters: Vec<Identifier>,
    body: Block,
    env: Environment,
}

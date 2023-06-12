use std::{collections::HashMap, fmt::Display};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
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
        }
    }
}

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: &str, obj: Object) {
        self.store.insert(name.to_string(), obj);
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

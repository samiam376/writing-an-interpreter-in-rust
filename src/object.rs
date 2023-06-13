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
            Object::Function(fun) => write!(f, "{}", fun),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Environment {
    store: Rc<RefCell<HashMap<String, Object>>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: Rc::new(RefCell::new(HashMap::new())),
            outer: None,
        }
    }

    pub fn to_enclosed(&self) -> Self {
        Self {
            store: Rc::new(RefCell::new(HashMap::new())),
            outer: Some(Rc::new(RefCell::new(self.clone()))),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let obj = self.store.borrow().get(name).cloned();
        if obj.is_some() {
            return obj;
        };

        if let Some(outer) = &self.outer {
            outer.borrow().get(name)
        } else {
            None
        }
    }
    pub fn set(&mut self, name: &str, obj: Object) {
        self.store.borrow_mut().insert(name.to_string(), obj);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: Block,
    pub env: Environment,
}

impl Function {
    pub fn new(parameters: Vec<Identifier>, body: Block, env: Environment) -> Self {
        Self {
            parameters,
            body,
            env,
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let mut body = String::new();
        for stmt in &self.body {
            body.push_str(&format!("{}\n", stmt));
        }
        write!(f, "fn({}) {{\n{}\n}}", params, body)
    }
}

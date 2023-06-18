use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    ast::{Block, Identifier},
    evaluator::EvalReturn,
};
pub trait Apply {
    fn apply(&self, args: Vec<Object>) -> EvalReturn;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BuiltInFunction {
    Len(Len),
    First(First),
    Last(Last),
    Rest(Rest),
    Push(Push),
}

impl Apply for BuiltInFunction {
    fn apply(&self, args: Vec<Object>) -> EvalReturn {
        match self {
            BuiltInFunction::Len(len) => len.apply(args),
            BuiltInFunction::First(first) => first.apply(args),
            BuiltInFunction::Last(last) => last.apply(args),
            BuiltInFunction::Rest(rest) => rest.apply(args),
            BuiltInFunction::Push(push) => push.apply(args),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Len;

impl Apply for Len {
    fn apply(&self, args: Vec<Object>) -> EvalReturn {
        if args.len() != 1 {
            return Err(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        };

        match &args[0] {
            Object::String(s) => Ok(Some(Object::Integer(s.len() as i64))),
            _ => Err(format!("argument to `len` not supported, got={}", args[0])),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct First;

impl Apply for First {
    fn apply(&self, args: Vec<Object>) -> EvalReturn {
        if args.len() != 1 {
            return Err(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        };

        match &args[0] {
            Object::Array(arr) => {
                let first = arr.first();
                if let Some(first) = first {
                    Ok(Some(first.clone()))
                } else {
                    Ok(Some(Object::Null))
                }
            }
            _ => Err(format!(
                "argument to `first` must be ARRAY, got={}",
                args[0]
            )),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Last;

impl Apply for Last {
    fn apply(&self, args: Vec<Object>) -> EvalReturn {
        if args.len() != 1 {
            return Err(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        };

        match &args[0] {
            Object::Array(arr) => {
                let last = arr.last();
                if let Some(last) = last {
                    Ok(Some(last.clone()))
                } else {
                    Ok(Some(Object::Null))
                }
            }
            _ => Err(format!("argument to `last` must be ARRAY, got={}", args[0])),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Rest;

impl Apply for Rest {
    fn apply(&self, args: Vec<Object>) -> EvalReturn {
        if args.len() != 2 {
            return Err(format!(
                "wrong number of arguments. got={}, want=2",
                args.len()
            ));
        };

        match (&args[0], &args[1]) {
            (Object::Array(arr), Object::Integer(n)) => {
                let idx = *n as usize;
                let rest = arr[idx..].to_vec();
                Ok(Some(Object::Array(rest)))
            }
            _ => Err(format!(
                "argument to `rest` must be ARRAY, with valid idx got={}, idx={}",
                args[0], args[1]
            )),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Push;

impl Apply for Push {
    fn apply(&self, args: Vec<Object>) -> EvalReturn {
        if args.len() != 2 {
            return Err(format!(
                "wrong number of arguments. got={}, want=2",
                args.len()
            ));
        };

        match (&args[0], &args[1]) {
            (Object::Array(arr), obj) => {
                let mut new_arr = arr.clone();
                new_arr.push(obj.clone());
                Ok(Some(Object::Array(new_arr)))
            }
            _ => Err(format!(
                "argument to `push` must be ARRAY, with valid idx got={}, idx={}",
                args[0], args[1]
            )),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum HashKey {
    Integer(i64),
    Boolean(bool),
    String(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Function(Function),
    Builtin(BuiltInFunction),
    Array(Vec<Object>),
    Hash { pairs: HashMap<HashKey, HashPair> },
}

impl Object {
    pub fn lookup_builtin(input: &str) -> Option<Object> {
        match input {
            "len" => Some(Object::Builtin(BuiltInFunction::Len(Len))),
            "first" => Some(Object::Builtin(BuiltInFunction::First(First))),
            "last" => Some(Object::Builtin(BuiltInFunction::Last(Last))),
            "rest" => Some(Object::Builtin(BuiltInFunction::Rest(Rest))),
            "push" => Some(Object::Builtin(BuiltInFunction::Push(Push))),
            _ => None,
        }
    }

    pub fn hash_key(&self) -> Result<HashKey, String> {
        match self {
            Object::Integer(i) => Ok(HashKey::Integer(*i)),
            Object::Boolean(b) => Ok(HashKey::Boolean(*b)),
            Object::String(s) => Ok(HashKey::String(s.clone())),
            _ => Err(format!("unusable as hash key: {}", self)),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(b) => *b,
            Object::Null => false,
            _ => true,
        }
    }

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

    pub fn is_array(&self) -> bool {
        matches!(self, Object::Array(_))
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

impl From<String> for Object {
    fn from(s: String) -> Self {
        Object::String(s)
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
            Object::String(s) => write!(f, "{}", s),
            Object::Builtin(b) => write!(f, "{:?}", b),
            Object::Array(arr) => {
                let elements = arr
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{}]", elements)
            }
            Object::Hash { pairs } => {
                let elements = pairs
                    .iter()
                    .map(|(_k, v)| format!("{}: {}", v.key, v.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{}}}", elements)
            }
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

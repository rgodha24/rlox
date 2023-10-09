use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::eval::{RuntimeErrorType, Value};

#[derive(Debug, Default)]
pub struct Environment {
    values: HashMap<Box<str>, Value>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name.into_boxed_str(), value);
    }

    pub fn get(&self, name: &str) -> Result<Value, RuntimeErrorType> {
        match self.values.get(name).cloned() {
            Some(v) => Ok(v),
            None => self
                .parent
                .as_ref()
                .map(|p| p.borrow().get(name).ok())
                .flatten()
                .ok_or(RuntimeErrorType::UndefinedVariable(name.to_string())),
        }
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), RuntimeErrorType> {
        if self.values.contains_key(name) {
            self.values.insert(name.into(), value);
            Ok(())
        } else {
            self.parent
                .as_mut()
                .map(|p| p.borrow_mut().assign(name, value))
                // if parent is None, then we are at the global scope and the variable is not defined
                .unwrap_or_else(|| Err(RuntimeErrorType::UndefinedVariable(name.into())))
        }
    }

    pub fn new_enclosed(parent: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            parent: Some(parent.clone()),
        }
    }
}

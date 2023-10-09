use std::collections::HashMap;

use crate::eval::{RuntimeErrorType, Value};

#[derive(Debug, Clone, Default)]
pub struct Environment {
    values: HashMap<Box<str>, Value>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name.into_boxed_str(), value);
    }

    pub fn get(&self, name: String) -> Result<Value, RuntimeErrorType> {
        self.values
            .get(name.as_str())
            .cloned()
            .ok_or_else(|| RuntimeErrorType::UndefinedVariable(name))
    }
}

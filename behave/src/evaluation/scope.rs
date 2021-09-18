use std::collections::HashMap;

use crate::ast::{ASTTree, Ident, Path};
use crate::diagnostic::{Diagnostic, Label, Level};
use crate::evaluation::expression::Value;

pub struct GlobalResolver<'a> {
	roots: Vec<&'a ASTTree>,
}

impl<'a> GlobalResolver<'a> {
	pub fn new(file: &[&str], root: &'a ASTTree, imports: &[Path]) -> Result<Self, Vec<Diagnostic>> {
		let mut roots = vec![root];
		let mut diagnostics = Vec::new();
		for import in imports {
			match root.get_ast(&import.0) {
				Ok(tree) => roots.push(tree),
				Err(diag) => diagnostics.push(diag.add_label(Label::primary(file, "here", import.1.clone()))),
			}
		}

		if diagnostics.len() == 0 {
			Ok(Self { roots })
		} else {
			Err(diagnostics)
		}
	}
}

struct Scope {
	variables: HashMap<String, Value>,
}

pub struct ScopedResolver<'a> {
	file: &'a [String],
	scopes: Vec<Scope>,
}

impl<'a> ScopedResolver<'a> {
	pub fn new(file: &'a [String]) -> Self {
		Self {
			file,
			scopes: Vec::new(),
		}
	}

	pub fn get_var(&self, name: &Ident) -> Result<Value, Vec<Diagnostic>> {
		let mut depth = self.scopes.len();
		while depth != 0 {
			if let Some(val) = self.scopes[depth - 1].variables.get(&name.0) {
				return Ok(val.clone());
			}
			depth -= 1;
		}

		Err(vec![Diagnostic::new(Level::Error, "undefined variable").add_label(
			Label::primary(self.file, "this variable is not defined", name.1.clone()),
		)])
	}

	pub fn add_scope(&mut self) {
		self.scopes.push(Scope {
			variables: HashMap::new(),
		})
	}

	pub fn create_var(&mut self, name: &Ident, value: Option<Value>) {
		self.scopes
			.last_mut()
			.unwrap()
			.variables
			.insert(name.0.clone(), value.unwrap_or(Value::None));
	}

	pub fn set_var(&mut self, name: &Ident, value: Value) -> Result<Value, Vec<Diagnostic>> {
		let mut depth = self.scopes.len();
		while depth != 0 {
			if let Some(val) = self.scopes[depth - 1].variables.get_mut(&name.0) {
				let v = value.clone();
				*val = value;
				return Ok(v);
			}
			depth -= 1;
		}

		Err(vec![Diagnostic::new(Level::Error, "undefined variable").add_label(
			Label::primary(self.file, "this variable is not defined", name.1.clone()),
		)])
	}
}

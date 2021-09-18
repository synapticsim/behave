use std::collections::HashMap;

use crate::ast::{
	Animation,
	Assignment,
	AssignmentTarget,
	BinaryOperator,
	Block,
	Call,
	Component,
	Expression,
	ExpressionType,
	For,
	Function,
	Ident,
	IfChain,
	Index,
	Path,
	StatementType,
	Switch,
	UnaryOperator,
	While,
};
use crate::diagnostic::{Diagnostic, Label, Level};
use crate::evaluation::scope::ScopedResolver;

fn path_to_str<'a>(mut path: impl Iterator<Item = &'a String>) -> String {
	let mut s = String::new();
	s += &path.next().unwrap();
	while let Some(p) = path.next() {
		s.push('.');
		s += &p;
	}
	s
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
	String(String),
	Number(f64),
	Boolean(bool),
	Function(Function),
	Array(Vec<Value>),
	Object(Object),
	EnumVariant(EnumVariant),
	Code(Block),
	None,
}

impl Value {
	fn type_to_str(&self) -> String {
		use Value::*;
		match self {
			String(_) => "str".to_string(),
			Number(_) => "num".to_string(),
			Boolean(_) => "bool".to_string(),
			Function(_) => "fn".to_string(),
			Array(_) => "array".to_string(),
			Object(o) => path_to_str(o.ty.0.iter().map(|i| &i.0)),
			EnumVariant(e) => path_to_str(e.ty.0.iter().map(|i| &i.0)),
			Code(_) => "code".to_string(),
			None => "none".to_string(),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum TemplateValue {
	Component(Component),
	Animation(Animation),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
	pub ty: Path,
	pub data: HashMap<Ident, Value>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
	pub ty: Path,
	pub value: f64,
}

pub struct ExpressionEvaluator<'a> {
	file: &'a [String],
	stack: ScopedResolver<'a>,
}

impl<'a> ExpressionEvaluator<'a> {
	pub fn new(file: &'a [String]) -> Self {
		Self {
			file,
			stack: ScopedResolver::new(file),
		}
	}

	pub fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value, Vec<Diagnostic>> {
		use ExpressionType::*;
		Ok(match &expr.0 {
			None => Value::None,
			String(str) => Value::String(str.clone()),
			Number(num) => Value::Number(*num),
			Boolean(val) => Value::Boolean(*val),
			Function(func) => Value::Function(func.clone()),
			Code(code) => Value::Code(code.clone()),
			Block(block) => self.evaluate_block(block)?,
			Array(values) => self.evaluate_array(values)?,
			Access(path) => self.evaluate_access(path)?,
			RPNAccess(expr) => self.evaluate_rpn_access(expr)?,
			Index(index) => self.evaluate_index(index)?,
			Assignment(assignment) => self.evaluate_assignment(assignment)?,
			Unary(op, expr) => self.evaluate_unary(*op, expr)?,
			Binary(left, op, right) => self.evaluate_binary(*op, left, right)?,
			Call(call) => self.evaluate_call(call)?,
			IfChain(chain) => self.evaluate_if(chain)?,
			Switch(switch) => self.evaluate_switch(switch)?,
			While(whil) => self.evaluate_while(whil)?,
			For(fo) => self.evaluate_for(fo)?,
			_ => unreachable!(),
		})
	}

	fn evaluate_block(&mut self, block: &Block) -> Result<Value, Vec<Diagnostic>> {
		self.stack.add_scope();

		for statement in block.statements.iter() {
			match &statement.0 {
				StatementType::Declaration(var) => {
					let value = if let Some(expr) = &var.value {
						Some(self.evaluate_expression(expr)?)
					} else {
						None
					};
					self.stack.create_var(&var.name, value);
				},
				StatementType::Expression(expr) => {
					self.evaluate_expression(&Expression(expr.clone(), statement.1.clone()))?;
				},
			}
		}

		if let Some(expr) = &block.expression {
			self.evaluate_expression(expr)
		} else {
			Ok(Value::None)
		}
	}

	fn evaluate_access(&mut self, path: &Path) -> Result<Value, Vec<Diagnostic>> {
		if path.0.len() == 1 {
			self.stack.get_var(&path.0[0])
		} else {
			todo!("Long access not implemented")
		}
	}

	fn evaluate_rpn_access(&mut self, expr: &Expression) -> Result<Value, Vec<Diagnostic>> {
		todo!("RPN access not implemented")
	}

	fn evaluate_array(&mut self, values: &[Expression]) -> Result<Value, Vec<Diagnostic>> {
		let mut errors = Vec::new();
		let array = values
			.iter()
			.map(|expr| self.evaluate_expression(expr))
			.filter_map(|val| match val {
				Ok(val) => Some(val),
				Err(vec) => {
					errors.extend(vec);
					Option::None
				},
			})
			.collect();
		if errors.len() == 0 {
			Ok(Value::Array(array))
		} else {
			Err(errors)
		}
	}

	fn evaluate_index(&mut self, index: &Index) -> Result<Value, Vec<Diagnostic>> {
		let array = self.evaluate_expression(&index.array)?;
		if let Value::Array(array) = array {
			let idx = self.evaluate_expression(&index.index)?;
			if let Value::Number(idx) = idx {
				let len = array.len();
				if let Some(val) = array.into_iter().nth(idx as usize) {
					Ok(val)
				} else {
					Err(vec![Diagnostic::new(Level::Error, "array index out of bounds")
						.add_label(Label::primary(
							self.file,
							format!("array length is {}, but index was {}", len, idx as usize),
							index.index.1.clone(),
						))])
				}
			} else {
				Err(vec![Diagnostic::new(Level::Error, "array index must be a number")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", idx.type_to_str()),
						index.index.1.clone(),
					))])
			}
		} else {
			Err(vec![Diagnostic::new(Level::Error, "can only index arrays").add_label(
				Label::primary(
					self.file,
					format!("expression result is of type `{}`", array.type_to_str()),
					index.array.1.clone(),
				),
			)])
		}
	}

	fn evaluate_assignment(&mut self, assignment: &Assignment) -> Result<Value, Vec<Diagnostic>> {
		match &assignment.target {
			AssignmentTarget::Var(path) => {
				if path.0.len() == 1 {
					let value = self.evaluate_expression(&assignment.value)?;
					self.stack.set_var(&path.0[0], value)
				} else {
					todo!("Long assignment not implemented")
				}
			},
			_ => todo!("Other assignment not implemented"),
		}
	}

	fn evaluate_unary(&mut self, operator: UnaryOperator, expr: &Expression) -> Result<Value, Vec<Diagnostic>> {
		let value = self.evaluate_expression(expr)?;
		match operator {
			UnaryOperator::Negate => {
				if let Value::Number(val) = value {
					Ok(Value::Number(-val))
				} else {
					Err(vec![Diagnostic::new(Level::Error, "can only negate a number")
						.add_label(Label::primary(
							self.file,
							format!("expression result is of type `{}`", value.type_to_str()),
							expr.1.clone(),
						))])
				}
			},
			UnaryOperator::Not => {
				if let Value::Boolean(val) = value {
					Ok(Value::Boolean(!val))
				} else {
					Err(vec![Diagnostic::new(Level::Error, "can only not a boolean").add_label(
						Label::primary(
							self.file,
							format!("expression result is of type `{}`", value.type_to_str()),
							expr.1.clone(),
						),
					)])
				}
			},
		}
	}

	fn evaluate_binary(
		&mut self, operator: BinaryOperator, left: &Expression, right: &Expression,
	) -> Result<Value, Vec<Diagnostic>> {
		let lhs = self.evaluate_expression(left)?;
		let rhs = self.evaluate_expression(right)?;
		match operator {
			BinaryOperator::Add => match (lhs, rhs) {
				(Value::String(lhs), Value::String(rhs)) => Ok(Value::String(lhs + &rhs)),
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot add")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Subtract => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot subtract")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Multiply => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot multiply")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Divide => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot divide")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::And => match (lhs, rhs) {
				(Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs && rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot and")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Or => match (lhs, rhs) {
				(Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs || rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot or")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Equal => match (lhs, rhs) {
				(Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs == rhs)),
				(Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs == rhs)),
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs == rhs)),
				(Value::Array(lhs), Value::Array(rhs)) => Ok(Value::Boolean(lhs == rhs)),
				(Value::None, Value::None) => Ok(Value::Boolean(true)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot equate")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::NotEqual => match (lhs, rhs) {
				(Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs != rhs)),
				(Value::String(lhs), Value::String(rhs)) => Ok(Value::Boolean(lhs != rhs)),
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs != rhs)),
				(Value::Array(lhs), Value::Array(rhs)) => Ok(Value::Boolean(lhs != rhs)),
				(Value::None, Value::None) => Ok(Value::Boolean(false)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Greater => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs > rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Lesser => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs < rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::GreaterThanOrEqual => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs >= rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
			BinaryOperator::LesserThanOrEqual => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Boolean(lhs <= rhs)),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", lhs.type_to_str()),
						left.1.clone(),
					))
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", rhs.type_to_str()),
						right.1.clone(),
					))]),
			},
		}
	}

	fn evaluate_call(&mut self, call: &Call) -> Result<Value, Vec<Diagnostic>> { todo!("Call not implemented") }

	fn evaluate_if(&mut self, if_chain: &IfChain) -> Result<Value, Vec<Diagnostic>> {
		for ifs in if_chain.ifs.iter() {
			let cond = self.evaluate_expression(&ifs.0)?;
			if let Value::Boolean(cond) = cond {
				if cond {
					return self.evaluate_block(&ifs.1);
				}
			} else {
				return Err(vec![Diagnostic::new(Level::Error, "if condition must be a boolean")
					.add_label(Label::primary(
						self.file,
						format!("expression result is of type `{}`", cond.type_to_str()),
						ifs.0 .1.clone(),
					))]);
			}
		}

		if let Some(els) = &if_chain.else_part {
			self.evaluate_block(&els.0)
		} else {
			Ok(Value::None)
		}
	}

	fn evaluate_switch(&mut self, switch: &Switch) -> Result<Value, Vec<Diagnostic>> {
		let on = self.evaluate_expression(&switch.on)?;
		for case in switch.cases.iter() {
			if on == self.evaluate_expression(&switch.on)? {
				return self.evaluate_expression(&case.code);
			}
		}

		Ok(Value::None)
	}

	fn evaluate_while(&mut self, while_loop: &While) -> Result<Value, Vec<Diagnostic>> { todo!() }

	fn evaluate_for(&mut self, for_loop: &For) -> Result<Value, Vec<Diagnostic>> { todo!() }
}

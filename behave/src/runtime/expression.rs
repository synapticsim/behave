use std::collections::HashMap;
use std::ops::{ControlFlow, FromResidual, Try};
use std::process::id;

use crate::ast::{
	Access,
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
	GlobalAccess,
	IfChain,
	Index,
	Location,
	Path,
	ResolvedAccess,
	ResolvedType,
	StatementType,
	StructCreate,
	Switch,
	TypeType,
	UnaryOperator,
	Use,
	While,
};
use crate::diagnostic::{Diagnostic, Label, Level};
use crate::items::ItemMap;
use crate::runtime::value::{CallStack, Object, RuntimeType, Value};

pub enum Flow<'a> {
	Ok(Value<'a>),
	Return(Location<'a>, Value<'a>),
	Break(Location<'a>, Option<Value<'a>>),
	Err(Vec<Diagnostic>),
}

impl<'a> Try for Flow<'a> {
	type Output = Value<'a>;
	type Residual = Flow<'a>;

	fn from_output(output: Self::Output) -> Self { Self::Ok(output) }

	fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
		match self {
			Self::Ok(val) => ControlFlow::Continue(val),
			flow => ControlFlow::Break(flow),
		}
	}
}

impl FromResidual for Flow<'_> {
	fn from_residual(residual: <Self as Try>::Residual) -> Self { residual }
}

pub struct ExpressionEvaluator<'a> {
	stack: CallStack<'a>,
	item_map: &'a ItemMap<'a>,
}

macro_rules! evaluate {
	($self:expr,on $expr:expr,type $ty:ident $expect:literal) => {
		match $self.evaluate_expression($expr) {
			Flow::Ok(value) => match value {
				Value::$ty(s) => Ok(s),
				value => Err(vec![Diagnostic::new(Level::Error, $expect).add_label(Label::primary(
					format!("expression result is of type `{}`", value.get_type($self.item_map)),
					$expr.1.clone(),
				))]),
			},
			Flow::Return(loc, _) => Err(vec![Diagnostic::new(Level::Error, "unexpected return")
				.add_label(Label::primary("return expression here `{}`", loc))]),
			Flow::Break(loc, _) => Err(vec![Diagnostic::new(Level::Error, "unexpected break")
				.add_label(Label::primary("break expression here `{}`", loc))]),
			Flow::Err(err) => Err(err),
		}
	};
}

impl<'a> ExpressionEvaluator<'a> {
	pub fn new(item_map: &'a ItemMap<'a>) -> Self {
		ExpressionEvaluator {
			stack: CallStack::new(),
			item_map,
		}
	}

	pub fn evaluate_as_string(&mut self, expr: &Expression<'a>) -> Result<String, Vec<Diagnostic>> {
		evaluate!(self, on expr, type String "expected string")
	}

	pub fn evaluate_as_number(&mut self, expr: &Expression<'a>) -> Result<f64, Vec<Diagnostic>> {
		evaluate!(self, on expr, type Number "expected string")
	}

	fn evaluate_expression(&mut self, expr: &Expression<'a>) -> Flow<'a> {
		use ExpressionType::*;
		Flow::Ok(match &expr.0 {
			None => Value::None,
			String(str) => Value::String(str.clone()),
			Number(num) => Value::Number(*num),
			Boolean(val) => Value::Boolean(*val),
			Function(func) => Value::Function(func.clone()),
			Code(code) => Value::Code(code.clone()),
			Block(block) => self.evaluate_block(block)?,
			Array(values) => self.evaluate_array(values)?,
			Access(path) => self.evaluate_access(path)?,
			Index(index) => self.evaluate_index(index)?,
			Assignment(assignment) => self.evaluate_assignment(assignment)?,
			Unary(op, expr) => self.evaluate_unary(*op, expr)?,
			Binary(left, op, right) => self.evaluate_binary(*op, left, right)?,
			Call(call) => self.evaluate_call(call)?,
			IfChain(chain) => self.evaluate_if(chain)?,
			Switch(switch) => self.evaluate_switch(switch)?,
			While(whil) => self.evaluate_while(whil)?,
			For(fo) => self.evaluate_for(fo)?,
			StructCreate(s) => self.evaluate_struct(s)?,
			Return(e) => self.evaluate_return(e.as_deref(), expr.1.clone())?,
			Break(e) => self.evaluate_break(e.as_deref(), expr.1.clone())?,
			Use(us) => self.evaluate_use(us)?,
			Component(component) => self.evaluate_component(component)?,
			Animation(animation) => self.evaluate_animation(animation)?,
			_ => unreachable!("Cannot evaluate RPN access"),
		})
	}

	fn evaluate_block(&mut self, block: &Block<'a>) -> Flow<'a> {
		self.stack.scope();

		let mut errors = Vec::new();

		for stmt in block.statements.iter() {
			match &stmt.0 {
				StatementType::Declaration(var) => {
					let value = if let Some(expr) = &var.value {
						match self.evaluate_expression(expr) {
							Flow::Ok(val) => val,
							Flow::Err(err) => {
								errors.extend(err);
								continue;
							},
							flow => return flow,
						}
					} else {
						Value::None
					};
					self.stack.new_var(&var.name, value);
				},
				StatementType::Expression(expr) => {
					match self.evaluate_expression(&Expression(expr.clone(), stmt.1.clone())) {
						Flow::Ok(_) => {},
						Flow::Err(err) => {
							errors.extend(err);
						},
						flow => return flow,
					}
				},
			}
		}

		let ret = if let Some(expr) = &block.expression {
			self.evaluate_expression(expr.as_ref())
		} else {
			Flow::Ok(Value::None)
		};

		self.stack.end_scope();

		if errors.len() == 0 {
			ret
		} else {
			Flow::Err(errors)
		}
	}

	fn evaluate_access(&mut self, access: &Access<'a>) -> Flow<'a> {
		match access.resolved.as_ref().unwrap() {
			ResolvedAccess::Global(g) => match g {
				GlobalAccess::Function(id) => Flow::Ok(Value::Function(self.item_map.get_function(*id).clone())),
				GlobalAccess::Enum(e) => Flow::Ok(Value::Enum(*e)),
			},
			ResolvedAccess::Local => match self.stack.var(&access.path.0[0]) {
				Ok(val) => {
					if access.path.0.len() > 1 {
						let mut fields = access.path.0[1..].iter();
						let mut value = val;
						while let Some(ident) = fields.next() {
							if let Value::Object(ref mut object) = value {
								value = if let Some(field) = object.fields.remove(&ident.0) {
									field
								} else {
									return Flow::Err(vec![Diagnostic::new(Level::Error, "missing field").add_label(
										Label::primary(
											format!(
												"type `{}` does not have a field `{}`",
												RuntimeType::Struct(self.item_map.get_struct(object.id)),
												ident.0
											),
											ident.1.clone(),
										),
									)]);
								}
							} else {
								return Flow::Err(vec![Diagnostic::new(Level::Error, "type does not have fields")
									.add_label(Label::primary(
										format!(
											"the variable has a result of type `{}`, which does not have fields",
											value.get_type(&self.item_map)
										),
										ident.1.clone(),
									))]);
							}
						}
						Flow::Ok(value)
					} else {
						Flow::Ok(val)
					}
				},
				Err(err) => Flow::Err(vec![err]),
			},
		}
	}

	fn evaluate_struct(&mut self, s: &StructCreate<'a>) -> Flow<'a> {
		if let TypeType::User(user) = &s.ty.0 {
			if let ResolvedType::Struct(id) = user.resolved.unwrap() {
				let mut errors = Vec::new();

				let ty = self.item_map.get_struct(id);
				let mut object = Object {
					id,
					fields: HashMap::new(),
				};

				for field in s.values.iter() {
					if let Some(f) = ty.fields.iter().find(|entry| entry.name.0 == field.0 .0) {
						let value = self.evaluate_expression(&field.1)?;
						let ty = value.get_type(self.item_map);
						let should = RuntimeType::from(&self.item_map, &f.ty.0);
						if ty == should {
							object.fields.insert(field.0 .0.clone(), value);
						} else {
							errors.push(
								Diagnostic::new(Level::Error, "field type mismatch")
									.add_label(Label::primary(
										format!("this expression has a result of type `{}`...", ty),
										field.1 .1.clone(),
									))
									.add_label(Label::secondary(
										format!("...but field has a type `{}`", should),
										f.ty.1.clone(),
									)),
							);
							continue;
						}
					} else {
						errors.push(
							Diagnostic::new(Level::Error, "field does not exist").add_label(Label::primary(
								format!(
									"this field does not exist on type `{}`",
									RuntimeType::from(&self.item_map, &s.ty.0)
								),
								field.0 .1.clone(),
							)),
						);
						continue;
					}
				}

				for field in ty.fields.iter() {
					if !object.fields.contains_key(&field.name.0) {
						if let Some(default) = &field.default {
							let value = self.evaluate_expression(default.as_ref())?;
							let ty = value.get_type(self.item_map);
							let should = RuntimeType::from(&self.item_map, &field.ty.0);
							if ty == should {
								object.fields.insert(field.name.0.clone(), value);
							} else {
								errors.push(
									Diagnostic::new(Level::Error, "field type mismatch")
										.add_label(Label::primary(
											format!("this default expression has a result of type `{}`...", ty),
											default.1.clone(),
										))
										.add_label(Label::secondary(
											format!("...but field has a type `{}`", should),
											field.ty.1.clone(),
										)),
								);
								continue;
							}
						} else {
							errors.push(
								Diagnostic::new(Level::Error, "field value missing")
									.add_label(Label::primary("this field is missing", field.name.1.clone())),
							);
							continue;
						}
					}
				}

				if errors.len() == 0 {
					Flow::Ok(Value::Object(object))
				} else {
					Flow::Err(errors)
				}
			} else {
				Flow::Err(vec![Diagnostic::new(
					Level::Error,
					"cannot instantiate enum like struct",
				)
				.add_label(Label::primary("this type is an enum", s.ty.1.clone()))])
			}
		} else {
			unreachable!()
		}
	}

	fn evaluate_return(&mut self, expr: Option<&Expression<'a>>, loc: Location<'a>) -> Flow<'a> {
		Flow::Return(
			loc,
			expr.map(|e| self.evaluate_expression(e))
				.unwrap_or(Flow::Ok(Value::None))?,
		)
	}

	fn evaluate_break(&mut self, expr: Option<&Expression<'a>>, loc: Location<'a>) -> Flow<'a> {
		Flow::Break(
			loc,
			match expr {
				Some(expr) => Some(self.evaluate_expression(expr)?),
				None => None,
			},
		)
	}

	fn evaluate_array(&mut self, values: &[Expression<'a>]) -> Flow<'a> {
		let mut errors = Vec::new();
		let (mut ty, mut ty_loc) = (RuntimeType::None, None);
		let array = values
			.iter()
			.map(|expr| (expr.1.clone(), self.evaluate_expression(expr)))
			.collect::<Vec<_>>()
			.into_iter()
			.filter_map(|val| match val.1 {
				Flow::Ok(ok) => {
					ty = ok.get_type(self.item_map);
					ty_loc = Some(val.0.clone());
					Some((val.0, ok))
				},
				Flow::Return(loc, _) => {
					errors.push(
						Diagnostic::new(Level::Error, "unexpected return")
							.add_label(Label::primary("return expression here `{}`", loc)),
					);
					None
				},
				Flow::Break(loc, _) => {
					errors.push(
						Diagnostic::new(Level::Error, "unexpected break")
							.add_label(Label::primary("break expression here `{}`", loc)),
					);
					None
				},
				Flow::Err(vec) => {
					errors.extend(vec);
					None
				},
			})
			.collect::<Vec<_>>();

		for t in array.iter() {
			let wty = t.1.get_type(self.item_map);
			if wty != ty {
				errors.push(
					Diagnostic::new(Level::Error, "mismatched array types")
						.add_label(Label::primary(format!("expression has type `{}`...", wty), t.0.clone()))
						.add_label(Label::secondary(
							format!("...but expected type `{}`", ty),
							ty_loc.as_ref().unwrap().clone(),
						)),
				);
			}
		}

		if errors.len() == 0 {
			Flow::Ok(Value::Array(ty, array.into_iter().map(|i| i.1).collect()))
		} else {
			Flow::Err(errors)
		}
	}

	fn evaluate_index(&mut self, index: &Index<'a>) -> Flow<'a> {
		let array = self.evaluate_expression(&index.array)?;
		if let Value::Array(_, array) = array {
			let idx = self.evaluate_expression(&index.index)?;
			if let Value::Number(idx) = idx {
				let len = array.len();
				if let Some(val) = array.into_iter().nth(idx as usize) {
					Flow::Ok(val)
				} else {
					Flow::Err(vec![Diagnostic::new(Level::Error, "array index out of bounds")
						.add_label(Label::primary(
							format!("array length is {}, but index was {}", len, idx as usize),
							index.index.1.clone(),
						))])
				}
			} else {
				Flow::Err(vec![Diagnostic::new(Level::Error, "array index must be a number")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", idx.get_type(self.item_map)),
						index.index.1.clone(),
					))])
			}
		} else {
			Flow::Err(vec![Diagnostic::new(Level::Error, "can only index arrays").add_label(
				Label::primary(
					format!("expression result is of type `{}`", array.get_type(self.item_map)),
					index.array.1.clone(),
				),
			)])
		}
	}

	fn evaluate_assignment(&mut self, assignment: &Assignment<'a>) -> Flow<'a> {
		let value = self.evaluate_expression(assignment.value.as_ref())?;
		match &assignment.target {
			AssignmentTarget::Var(access) => match access.resolved.as_ref().unwrap() {
				ResolvedAccess::Global(g) => match g {
					GlobalAccess::Function(_) => {
						Flow::Err(vec![Diagnostic::new(Level::Error, "cannot assign to global function")
							.add_label(Label::primary("tried to assign here", access.path.1.clone()))])
					},
					GlobalAccess::Enum(_) => {
						Flow::Err(vec![Diagnostic::new(Level::Error, "cannot assign to enum variant")
							.add_label(Label::primary("tried to assign here", access.path.1.clone()))])
					},
				},
				ResolvedAccess::Local => match self.stack.set_var(&access.path.0[0]) {
					Ok(val) => {
						let val = if access.path.0.len() > 1 {
							let mut fields = access.path.0[1..].iter();
							let mut val = val;
							while let Some(ident) = fields.next() {
								if let Value::Object(ref mut object) = val {
									val = if let Some(field) = object.fields.get_mut(&ident.0) {
										field
									} else {
										return Flow::Err(vec![Diagnostic::new(Level::Error, "missing field")
											.add_label(Label::primary(
												format!(
													"type `{}` does not have a field `{}`",
													RuntimeType::Struct(self.item_map.get_struct(object.id)),
													ident.0
												),
												ident.1.clone(),
											))]);
									}
								} else {
									return Flow::Err(vec![Diagnostic::new(Level::Error, "type does not have fields")
										.add_label(Label::primary(
											format!(
												"the variable has a result of type `{}`, which does not have fields",
												val.get_type(&self.item_map)
											),
											ident.1.clone(),
										))]);
								}
							}
							val
						} else {
							val
						};
						*val = value.clone();
						Flow::Ok(value)
					},
					Err(err) => Flow::Err(vec![err]),
				},
			},
			AssignmentTarget::Index(access, index) => match access.resolved.as_ref().unwrap() {
				ResolvedAccess::Global(g) => match g {
					GlobalAccess::Function(_) => {
						Flow::Err(vec![Diagnostic::new(Level::Error, "cannot assign to global function")
							.add_label(Label::primary("tried to assign here", access.path.1.clone()))])
					},
					GlobalAccess::Enum(_) => {
						Flow::Err(vec![Diagnostic::new(Level::Error, "cannot assign to enum variant")
							.add_label(Label::primary("tried to assign here", access.path.1.clone()))])
					},
				},
				ResolvedAccess::Local => {
					let idx = self.evaluate_expression(index.as_ref())?;
					match self.stack.set_var(&access.path.0[0]) {
						Ok(val) => {
							let val = if access.path.0.len() > 1 {
								let mut fields = access.path.0[1..].iter();
								let mut val = val;
								while let Some(ident) = fields.next() {
									if let Value::Object(ref mut object) = val {
										val = if let Some(field) = object.fields.get_mut(&ident.0) {
											field
										} else {
											return Flow::Err(vec![Diagnostic::new(Level::Error, "missing field")
												.add_label(Label::primary(
													format!(
														"type `{}` does not have a field `{}`",
														RuntimeType::Struct(self.item_map.get_struct(object.id)),
														ident.0
													),
													ident.1.clone(),
												))]);
										}
									} else {
										return Flow::Err(vec![Diagnostic::new(
											Level::Error,
											"type does not have fields",
										)
										.add_label(Label::primary(
											format!(
												"the variable has a result of type `{}`, which does not have fields",
												val.get_type(&self.item_map)
											),
											ident.1.clone(),
										))]);
									}
								}
								val
							} else {
								val
							};
							if let Value::Array(ty, array) = val {
								if let Value::Number(idx) = idx {
									let len = array.len();
									if let Some(val) = array.into_iter().nth(idx as usize) {
										if *ty == value.get_type(self.item_map) {
											*val = value.clone();
											Flow::Ok(value)
										} else {
											Flow::Err(vec![Diagnostic::new(
												Level::Error,
												"mismatch in array assignment type",
											)
											.add_label(Label::primary(
												format!(
													"this expression has a result of type `{}`...",
													value.get_type(self.item_map)
												),
												assignment.value.1.clone(),
											))
											.add_label(Label::secondary(
												format!("...but array is of type `{}`", ty),
												access.path.1.clone(),
											))])
										}
									} else {
										Flow::Err(vec![Diagnostic::new(Level::Error, "array index out of bounds")
											.add_label(Label::primary(
												format!("array length is {}, but index was {}", len, idx as usize),
												index.1.clone(),
											))])
									}
								} else {
									Flow::Err(vec![Diagnostic::new(Level::Error, "array index must be a number")
										.add_label(Label::primary(
											format!("expression result is of type `{}`", idx.get_type(self.item_map)),
											index.1.clone(),
										))])
								}
							} else {
								Flow::Err(vec![Diagnostic::new(Level::Error, "can only index arrays").add_label(
									Label::primary(
										format!("expression result is of type `{}`", val.get_type(self.item_map)),
										access.path.1.clone(),
									),
								)])
							}
						},
						Err(err) => Flow::Err(vec![err]),
					}
				},
			},
			_ => unreachable!("Cannot assign to RPN variable"),
		}
	}

	fn evaluate_unary(&mut self, operator: UnaryOperator, expr: &Expression<'a>) -> Flow<'a> {
		let value = self.evaluate_expression(expr)?;
		match operator {
			UnaryOperator::Negate => {
				if let Value::Number(val) = value {
					Flow::Ok(Value::Number(-val))
				} else {
					Flow::Err(vec![Diagnostic::new(Level::Error, "can only negate a number")
						.add_label(Label::primary(
							format!("expression result is of type `{}`", value.get_type(self.item_map)),
							expr.1.clone(),
						))])
				}
			},
			UnaryOperator::Not => {
				if let Value::Boolean(val) = value {
					Flow::Ok(Value::Boolean(!val))
				} else {
					Flow::Err(vec![Diagnostic::new(Level::Error, "can only not a boolean").add_label(
						Label::primary(
							format!("expression result is of type `{}`", value.get_type(self.item_map)),
							expr.1.clone(),
						),
					)])
				}
			},
		}
	}

	fn evaluate_binary(&mut self, operator: BinaryOperator, left: &Expression<'a>, right: &Expression<'a>) -> Flow<'a> {
		let lhs = self.evaluate_expression(left)?;
		let rhs = self.evaluate_expression(right)?;
		match operator {
			BinaryOperator::Add => match (lhs, rhs) {
				(Value::String(lhs), Value::String(rhs)) => Flow::Ok(Value::String(lhs + &rhs)),
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Number(lhs + rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot add")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Subtract => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Number(lhs - rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot subtract")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Multiply => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Number(lhs * rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot multiply")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Divide => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Number(lhs / rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot divide")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::And => match (lhs, rhs) {
				(Value::Boolean(lhs), Value::Boolean(rhs)) => Flow::Ok(Value::Boolean(lhs && rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot and")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Or => match (lhs, rhs) {
				(Value::Boolean(lhs), Value::Boolean(rhs)) => Flow::Ok(Value::Boolean(lhs || rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot or")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Equal => match (lhs, rhs) {
				(Value::Boolean(lhs), Value::Boolean(rhs)) => Flow::Ok(Value::Boolean(lhs == rhs)),
				(Value::String(lhs), Value::String(rhs)) => Flow::Ok(Value::Boolean(lhs == rhs)),
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Boolean(lhs == rhs)),
				(Value::Array(_, lhs), Value::Array(_, rhs)) => Flow::Ok(Value::Boolean(lhs == rhs)),
				(Value::None, Value::None) => Flow::Ok(Value::Boolean(true)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot equate")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::NotEqual => match (lhs, rhs) {
				(Value::Boolean(lhs), Value::Boolean(rhs)) => Flow::Ok(Value::Boolean(lhs != rhs)),
				(Value::String(lhs), Value::String(rhs)) => Flow::Ok(Value::Boolean(lhs != rhs)),
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Boolean(lhs != rhs)),
				(Value::Array(_, lhs), Value::Array(_, rhs)) => Flow::Ok(Value::Boolean(lhs != rhs)),
				(Value::None, Value::None) => Flow::Ok(Value::Boolean(false)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Greater => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Boolean(lhs > rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Lesser => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Boolean(lhs < rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::GreaterThanOrEqual => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Boolean(lhs >= rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
			BinaryOperator::LesserThanOrEqual => match (lhs, rhs) {
				(Value::Number(lhs), Value::Number(rhs)) => Flow::Ok(Value::Boolean(lhs <= rhs)),
				(lhs, rhs) => Flow::Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs.get_type(self.item_map)),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs.get_type(self.item_map)),
						right.1.clone(),
					))]),
			},
		}
	}

	fn evaluate_call(&mut self, call: &Call<'a>) -> Flow<'a> {
		let mut errors = Vec::new();
		let callee = self.evaluate_expression(&call.callee)?;
		let args = call
			.args
			.iter()
			.filter_map(|expr| match self.evaluate_expression(expr) {
				Flow::Ok(val) => Some(val),
				Flow::Return(loc, _) => {
					errors.push(
						Diagnostic::new(Level::Error, "unexpected return")
							.add_label(Label::primary("return expression here `{}`", loc)),
					);
					None
				},
				Flow::Break(loc, _) => {
					errors.push(
						Diagnostic::new(Level::Error, "unexpected break")
							.add_label(Label::primary("break expression here `{}`", loc)),
					);
					None
				},
				Flow::Err(err) => {
					errors.extend(err);
					None
				},
			})
			.collect::<Vec<_>>();

		if let Value::Function(f) = callee {
			if errors.len() == 0 {
				if f.args
					.iter()
					.map(|arg| RuntimeType::from(self.item_map, &arg.ty.0))
					.zip(args.iter().map(|arg| arg.get_type(self.item_map)))
					.enumerate()
					.all(|arg_pair| {
						if arg_pair.1 .0 != arg_pair.1 .1 {
							errors.push(
								Diagnostic::new(Level::Error, "mismatched argument types")
									.add_label(Label::primary(
										format!("this expression result is of type `{}`...", arg_pair.1 .1),
										call.args[arg_pair.0].1.clone(),
									))
									.add_label(Label::secondary(
										format!("...but type `{}` is expected", arg_pair.1 .0),
										f.args[arg_pair.0].ty.1.clone(),
									)),
							);
							false
						} else {
							true
						}
					}) {
					self.stack.call(
						args.into_iter()
							.enumerate()
							.map(|arg| (f.args[arg.0].name.0.clone(), arg.1)),
					);
					let block_result = self.evaluate_block(&f.block);
					self.stack.end_call();

					let (loc, ret) = match block_result {
						Flow::Ok(ret) => (
							match f.block.expression {
								Some(ref expr) => expr.1.clone(),
								None => Location {
									file: f.block.loc.file,
									range: f.block.loc.range.end - 1..f.block.loc.range.end,
								},
							},
							ret,
						),
						Flow::Break(loc, _) => {
							errors.push(
								Diagnostic::new(Level::Error, "unexpected break")
									.add_label(Label::primary("break expression here `{}`", loc)),
							);
							return Flow::Err(errors);
						},
						Flow::Return(loc, ret) => (loc, ret),
						Flow::Err(err) => {
							errors.extend(err);
							return Flow::Err(errors);
						},
					};

					let returns = ret.get_type(self.item_map);
					let should = f
						.ret
						.as_ref()
						.map(|ty| (RuntimeType::from(self.item_map, &ty.0), ty.1.clone()))
						.unwrap_or((
							RuntimeType::None,
							Location {
								file: f.block.loc.file,
								range: f.block.loc.range.start..f.block.loc.range.start + 1,
							},
						));
					if should.0 == returns {
						Flow::Ok(ret)
					} else {
						errors.push(
							Diagnostic::new(Level::Error, "mismatched return type")
								.add_label(Label::primary(format!("function returns type `{}`...", returns), loc))
								.add_label(Label::secondary(
									format!("...but should return type `{}`", should.0),
									should.1,
								)),
						);
						Flow::Err(errors)
					}
				} else {
					Flow::Err(errors)
				}
			} else {
				Flow::Err(errors)
			}
		} else {
			errors.push(
				Diagnostic::new(Level::Error, "can only call function").add_label(Label::primary(
					format!("expression result is of type `{}`", callee.get_type(self.item_map)),
					call.callee.1.clone(),
				)),
			);
			Flow::Err(errors)
		}
	}

	fn evaluate_use(&mut self, us: &Use<'a>) -> Flow<'a> { todo!("Use not implemented") }

	fn evaluate_component(&mut self, component: &Component<'a>) -> Flow<'a> { todo!("Component not implemented") }

	fn evaluate_animation(&mut self, animation: &Animation<'a>) -> Flow<'a> { todo!("Animation not implemented") }

	fn evaluate_if(&mut self, if_chain: &IfChain<'a>) -> Flow<'a> {
		for ifs in if_chain.ifs.iter() {
			let cond = self.evaluate_expression(&ifs.0)?;
			if let Value::Boolean(cond) = cond {
				if cond {
					return self.evaluate_block(&ifs.1);
				}
			} else {
				return Flow::Err(vec![Diagnostic::new(Level::Error, "if condition must be a boolean")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", cond.get_type(self.item_map)),
						ifs.0 .1.clone(),
					))]);
			}
		}

		if let Some(els) = &if_chain.else_part {
			self.evaluate_block(&els.0)
		} else {
			Flow::Ok(Value::None)
		}
	}

	fn evaluate_switch(&mut self, switch: &Switch<'a>) -> Flow<'a> {
		let on = self.evaluate_expression(&switch.on)?;
		for case in switch.cases.iter() {
			if on == self.evaluate_expression(&switch.on)? {
				return self.evaluate_expression(&case.code);
			}
		}

		Flow::Ok(Value::None)
	}

	fn evaluate_while(&mut self, while_loop: &While<'a>) -> Flow<'a> { todo!("For not implemented") }

	fn evaluate_for(&mut self, for_loop: &For<'a>) -> Flow<'a> { todo!("While not implemented") }
}

use std::collections::HashMap;

use crate::ast::{
	Access,
	Assignment,
	AssignmentTarget,
	BinaryOperator,
	Block,
	Call,
	EnumType,
	Expression,
	ExpressionType,
	GlobalAccess,
	Ident,
	IfChain,
	InbuiltEnum,
	InbuiltFunction,
	Location,
	MouseEvent,
	ResolvedAccess,
	StatementType,
	UnaryOperator,
};
use crate::diagnostic::{Diagnostic, Label, Level};
use crate::evaluation::runtime::{ExpressionEvaluator, Flow};
use crate::evaluation::value::{Code, FunctionValue, RuntimeEnumType, RuntimeType, Value};
use crate::output::xml::IndentIterator;

#[derive(Clone)]
struct RPNValue<'a> {
	register: u8,
	ty: RuntimeType<'a>,
}

struct RPNScope<'a> {
	vars: HashMap<String, RPNValue<'a>>,
	base_register: u8,
}

struct RPNFrame<'a> {
	scopes: Vec<RPNScope<'a>>,
	base_register: u8,
}

struct RPNStack<'a> {
	frames: Vec<RPNFrame<'a>>,
	next_register: u8,
}

impl<'a> RPNStack<'a> {
	fn new() -> Self {
		Self {
			frames: vec![RPNFrame {
				scopes: Vec::new(),
				base_register: 0,
			}],
			next_register: 0,
		}
	}

	fn new_var(&mut self, name: &Ident, ty: RuntimeType<'a>) -> Result<RPNValue<'a>, Diagnostic> {
		if self.next_register > 50 {
			return Err(Diagnostic::new(Level::Error, "too many local variables in scope")
				.add_note("you can move unused variables above into a different scope"));
		}

		let vars = &mut self.frames.last_mut().unwrap().scopes.last_mut().unwrap().vars;
		if let Some(value) = vars.get_mut(&name.0) {
			value.ty = ty;
			Ok(value.clone())
		} else {
			let ret = RPNValue {
				register: self.next_register,
				ty,
			};

			vars.insert(name.0.clone(), ret.clone());

			self.next_register += 1;

			Ok(ret)
		}
	}

	fn var(&mut self, name: &Ident) -> Option<RPNValue<'a>> {
		for scope in self.frames.last_mut().unwrap().scopes.iter_mut().rev() {
			if let Some(val) = scope.vars.get(&name.0) {
				return Some(val.clone());
			}
		}

		None
	}

	fn scope(&mut self) {
		self.frames.last_mut().unwrap().scopes.push(RPNScope {
			vars: HashMap::new(),
			base_register: self.next_register,
		})
	}

	fn end_scope(&mut self) {
		let scope = self.frames.last_mut().unwrap().scopes.pop().unwrap();
		self.next_register = scope.base_register
	}
}

pub struct RPNCompiler<'a, 'b> {
	evaluator: &'b mut ExpressionEvaluator<'a>,
	stack: RPNStack<'a>,
	indent: isize,
}

impl<'a, 'b> RPNCompiler<'a, 'b> {
	pub fn new(evaluator: &'b mut ExpressionEvaluator<'a>) -> Self {
		Self {
			evaluator,
			stack: RPNStack::new(),
			indent: -1,
		}
	}

	pub fn compile_block(&mut self, block: &Block<'a>) -> Result<Code<'a>, Vec<Diagnostic>> {
		let mut data = String::new();
		let mut errors = Vec::new();

		self.indent += 1;
		self.stack.scope();
		for stmt in block.statements.iter() {
			self.indent_string(&mut data);
			match &stmt.0 {
				StatementType::Declaration(decl) => {
					if let Some(expr) = &decl.value {
						match self.compile_expr(expr) {
							Ok(code) => {
								let var = match self.stack.new_var(&decl.name, code.ty) {
									Ok(value) => value,
									Err(err) => {
										errors.push(err);
										continue;
									},
								};
								data.push_str(&format!("{} sp{}", code.value, var.register))
							},
							Err(err) => errors.extend(err),
						}
					} else {
						match self.stack.new_var(&decl.name, RuntimeType::None) {
							Ok(value) => value,
							Err(err) => {
								errors.push(err);
								continue;
							},
						};
					}
				},
				StatementType::Expression(expr) => match self.compile_expr(&Expression(expr.clone(), stmt.1.clone())) {
					Ok(code) => {
						data.push_str(&code.value);
						if code.ty != RuntimeType::None {
							data.push_str(&" p");
						}
					},
					Err(err) => errors.extend(err),
				},
			}
			data.push('\n');
		}

		let code = if let Some(expr) = &block.expression {
			match self.compile_expr(expr.as_ref()) {
				Ok(code) => {
					self.indent_string(&mut data);
					data.push_str(&code.value);
					data.push('\n');
					Code {
						value: data,
						ty: code.ty,
					}
				},
				Err(err) => {
					errors.extend(err);
					Code {
						value: data,
						ty: RuntimeType::None,
					}
				},
			}
		} else {
			Code {
				value: data,
				ty: RuntimeType::None,
			}
		};

		self.stack.end_scope();
		self.indent -= 1;

		if errors.len() == 0 {
			Ok(code)
		} else {
			Err(errors)
		}
	}

	fn compile_expr(&mut self, expr: &Expression<'a>) -> Result<Code<'a>, Vec<Diagnostic>> {
		Ok(match &expr.0 {
			ExpressionType::String(s) => Code {
				value: format!("'{}'", s),
				ty: RuntimeType::Str,
			},
			ExpressionType::Number(n) => Code {
				value: n.to_string(),
				ty: RuntimeType::Num,
			},
			ExpressionType::Boolean(b) => Code {
				value: if *b { "1" } else { "0" }.to_string(),
				ty: RuntimeType::Bool,
			},
			ExpressionType::Block(b) => self.compile_block(b)?,
			ExpressionType::Access(access) => self.compile_access(access)?,
			ExpressionType::RPNAccess(expr) => self.compile_rpn_access(expr.as_ref())?,
			ExpressionType::Assignment(assignment) => self.compile_assignment(assignment)?,
			ExpressionType::Unary(op, expr) => self.compile_unary(*op, expr.as_ref())?,
			ExpressionType::Binary(left, op, right) => self.compile_binary(*op, left.as_ref(), right.as_ref())?,
			ExpressionType::Call(call) => self.compile_call(call)?,
			ExpressionType::IfChain(chain) => self.compile_if(chain)?,
			ExpressionType::Switch(_) => todo!("RPN switch not implemented"),
			ExpressionType::While(_) => todo!("While not implemented"),
			ExpressionType::For(_) => todo!("For not implemented"),
			ExpressionType::Return(expr) => self.compile_return(expr.as_deref())?,
			ExpressionType::Break(_) => todo!("Break not implemented"),
			_ => unreachable!(),
		})
	}

	fn compile_access(&mut self, access: &Access<'a>) -> Result<Code<'a>, Vec<Diagnostic>> {
		match access.resolved.as_ref().unwrap() {
			ResolvedAccess::Global(g) => match g {
				GlobalAccess::Enum(e) => match e.id {
					EnumType::User(id) => Ok(Code {
						value: format!("{}", e.value),
						ty: RuntimeType::Enum(RuntimeEnumType::User(id, self.evaluator.item_map.get_enum(id))),
					}),
					EnumType::Inbuilt(i) => Ok(Code {
						value: match i {
							InbuiltEnum::MouseEvent => format!("'{}'", MouseEvent::from_num(e.value).to_string()),
							_ => {
								return Err(vec![Diagnostic::new(
									Level::Error,
									"Inbuilt enum other than `MouseEvent` not allowed in this context",
								)
								.add_label(Label::primary("accessed inbuilt type here", access.path.1.clone()))])
							},
						},
						ty: RuntimeType::Enum(RuntimeEnumType::Inbuilt(i)),
					}),
				},
				GlobalAccess::Function(_) => Err(vec![Diagnostic::new(
					Level::Error,
					"cannot access function without calling it",
				)
				.add_label(Label::primary("must call functions in code", access.path.1.clone()))]),
			},
			ResolvedAccess::Local => {
				let mut otherwise = || match self.evaluator.evaluate_access(access) {
					Flow::Ok(val) => Ok(match val {
						Value::String(s) => Code {
							value: format!("'{}'", s),
							ty: RuntimeType::Str,
						},
						Value::Number(n) => Code {
							value: n.to_string(),
							ty: RuntimeType::Num,
						},
						Value::Boolean(b) => Code {
							value: if b { "1" } else { "0" }.to_string(),
							ty: RuntimeType::Bool,
						},
						Value::Enum(e) => match e.id {
							EnumType::User(id) => Code {
								value: format!("{}", e.value),
								ty: RuntimeType::Enum(RuntimeEnumType::User(id, self.evaluator.item_map.get_enum(id))),
							},
							EnumType::Inbuilt(i) => Code {
								value: match i {
									InbuiltEnum::MouseEvent => {
										format!("'{}'", MouseEvent::from_num(e.value).to_string())
									},
									_ => {
										return Err(vec![Diagnostic::new(
											Level::Error,
											"Inbuilt enum other than `MouseEvent` not allowed in this context",
										)
										.add_label(Label::primary(
											"accessed inbuilt type here",
											access.path.1.clone(),
										))])
									},
								},
								ty: RuntimeType::Enum(RuntimeEnumType::Inbuilt(i)),
							},
						},
						Value::Code(c) => c,
						_ => {
							return Err(vec![Diagnostic::new(
								Level::Error,
								"external access has disallowed type",
							)
							.add_label(Label::primary(
								format!("this variable has type `{}`", val.get_type(self.evaluator.item_map)),
								access.path.1.clone(),
							))])
						},
					}),
					Flow::Return(loc, _) => {
						return Err(vec![Diagnostic::new(Level::Error, "unexpected return")
							.add_label(Label::primary("return expression here `{}`", loc))])
					},
					Flow::Break(loc, _) => {
						return Err(vec![Diagnostic::new(Level::Error, "unexpected break")
							.add_label(Label::primary("break expression here `{}`", loc))])
					},
					Flow::Err(err) => return Err(err),
				};

				if access.path.0.len() == 1 {
					if let Some(value) = self.stack.var(&access.path.0[0]) {
						Ok(Code {
							value: format!("l{}", value.register),
							ty: value.ty,
						})
					} else {
						otherwise()
					}
				} else {
					otherwise()
				}
			},
		}
	}

	fn compile_assignment(&mut self, assign: &Assignment<'a>) -> Result<Code<'a>, Vec<Diagnostic>> {
		let value = self.compile_expr(assign.value.as_ref())?;

		match &assign.target {
			AssignmentTarget::Var(access) => match access.resolved.as_ref().unwrap() {
				ResolvedAccess::Global(g) => match g {
					GlobalAccess::Enum(_) => Err(vec![Diagnostic::new(Level::Error, "cannot assign to enum variant")
						.add_label(Label::primary("tried to assign here", access.path.1.clone()))]),
					GlobalAccess::Function(_) => {
						Err(vec![Diagnostic::new(Level::Error, "cannot assign to global function")
							.add_label(Label::primary(
								"tried to assign here",
								access.path.1.clone(),
							))])
					},
				},
				ResolvedAccess::Local => {
					if access.path.0.len() == 1 {
						if let Some(val) = self.stack.var(&access.path.0[0]) {
							Ok(Code {
								value: format!("{} s{}", value.value, val.register),
								ty: value.ty,
							})
						} else {
							Err(vec![Diagnostic::new(Level::Error, "variable does not exist")
								.add_label(Label::primary(
									"this variable does not exist",
									access.path.1.clone(),
								))])
						}
					} else {
						Err(vec![Diagnostic::new(Level::Error, "structs are not supported in RPN")
							.add_label(Label::primary(
								"tried to assign to struct field",
								access.path.1.clone(),
							))])
					}
				},
			},
			AssignmentTarget::RPNVar(expr) => {
				let rpn = self
					.evaluator
					.evaluate_as_string(expr, "RPN variable access must result in a `string`")?;
				if let Some(ty) = rpn.chars().next() {
					match ty {
						'F' => Err(vec![Diagnostic::new(
							Level::Error,
							"cannot access an RPN inbuilt function",
						)
						.add_label(Label::primary(
							"this access tries to call an RPN function",
							expr.1.clone(),
						))]),
						'A' | 'E' | 'P' => {
							let ty = if let Some(byte) = rpn.find(',') {
								if rpn[byte..].to_lowercase().contains("bool") {
									RuntimeType::Bool
								} else {
									RuntimeType::Num
								}
							} else {
								return Err(vec![Diagnostic::new(Level::Error, "missing simvar unit")
									.add_label(Label::primary("simvars and envvars need units", expr.1.clone()))]);
							};
							if ty == value.ty {
								Ok(Code {
									value: format!("{} d (>{})", value.value, rpn),
									ty,
								})
							} else {
								Err(vec![Diagnostic::new(Level::Error, "assignment type mismatch")
									.add_label(Label::primary(
										format!("expression has type `{}`...", value.ty),
										assign.value.1.clone(),
									))
									.add_label(Label::secondary(
										format!("...but RPN variable has type `{}`", ty),
										expr.1.clone(),
									))])
							}
						},
						'B' | 'G' | 'H' | 'I' | 'K' | 'L' | 'O' | 'R' | 'W' | 'Z' => {
							let ty = if let Some(byte) = rpn.find(',') {
								if rpn[byte..].to_lowercase().contains("bool") {
									RuntimeType::Bool
								} else {
									RuntimeType::Num
								}
							} else {
								RuntimeType::Num
							};
							if ty == value.ty {
								Ok(Code {
									value: format!("{} (>{})", value.value, rpn),
									ty,
								})
							} else {
								Err(vec![Diagnostic::new(Level::Error, "assignment type mismatch")
									.add_label(Label::primary(
										format!("expression has type `{}`...", value.ty),
										assign.value.1.clone(),
									))
									.add_label(Label::secondary(
										format!("...but RPN variable has type `{}`", ty),
										expr.1.clone(),
									))])
							}
						},
						'M' => Err(vec![Diagnostic::new(Level::Error, "cannot assign to mouse variable")
							.add_label(Label::primary("tried to assign here", expr.1.clone()))]),
						_ => Err(vec![Diagnostic::new(Level::Error, "unknown RPN variable type")
							.add_label(Label::primary(
								"this access has an unknown variable type",
								expr.1.clone(),
							))]),
					}
				} else {
					Err(vec![Diagnostic::new(
						Level::Error,
						"RPN variable assign string is empty",
					)
					.add_label(Label::primary("this string is empty", expr.1.clone()))])
				}
			},
			AssignmentTarget::Index(a, _) => Err(vec![Diagnostic::new(Level::Error, "unsupported assignment")
				.add_label(Label::primary("tried to assign to array index here", a.path.1.clone()))]),
		}
	}

	fn compile_unary(&mut self, op: UnaryOperator, expr: &Expression<'a>) -> Result<Code<'a>, Vec<Diagnostic>> {
		let code = self.compile_expr(expr)?;
		match op {
			UnaryOperator::Negate => {
				if code.ty == RuntimeType::Num {
					Ok(Code {
						value: format!("{} neg", code.value),
						ty: RuntimeType::Num,
					})
				} else {
					Err(vec![Diagnostic::new(Level::Error, "can only negate a number")
						.add_label(Label::primary(
							format!("expression result is of type `{}`", code.ty),
							expr.1.clone(),
						))])
				}
			},
			UnaryOperator::Not => {
				if code.ty == RuntimeType::Bool {
					Ok(Code {
						value: format!("{} !", code.value),
						ty: RuntimeType::Bool,
					})
				} else {
					Err(vec![Diagnostic::new(Level::Error, "can only not a boolean").add_label(
						Label::primary(format!("expression result is of type `{}`", code.ty), expr.1.clone()),
					)])
				}
			},
		}
	}

	fn compile_binary(
		&mut self, op: BinaryOperator, left: &Expression<'a>, right: &Expression<'a>,
	) -> Result<Code<'a>, Vec<Diagnostic>> {
		let lhs = self.compile_expr(left)?;
		let rhs = self.compile_expr(right)?;

		match op {
			BinaryOperator::Add => match (lhs.ty, rhs.ty) {
				(RuntimeType::Str, RuntimeType::Str) => Ok(Code {
					value: format!("{} {} scat", lhs.value, rhs.value),
					ty: RuntimeType::Str,
				}),
				(RuntimeType::Num, RuntimeType::Num) => Ok(Code {
					value: format!("{} {} +", lhs.value, rhs.value),
					ty: RuntimeType::Num,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot add")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Subtract => match (lhs.ty, rhs.ty) {
				(RuntimeType::Num, RuntimeType::Num) => Ok(Code {
					value: format!("{} {} -", lhs.value, rhs.value),
					ty: RuntimeType::Num,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot subtract")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Multiply => match (lhs.ty, rhs.ty) {
				(RuntimeType::Num | RuntimeType::Bool, RuntimeType::Num) => Ok(Code {
					value: format!("{} {} *", lhs.value, rhs.value),
					ty: RuntimeType::Num,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot multiply")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Divide => match (lhs.ty, rhs.ty) {
				(RuntimeType::Num | RuntimeType::Bool, RuntimeType::Num) => Ok(Code {
					value: format!("{} {} /", lhs.value, rhs.value),
					ty: RuntimeType::Num,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot divide")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::And => match (lhs.ty, rhs.ty) {
				(RuntimeType::Bool, RuntimeType::Bool) => Ok(Code {
					value: format!("{} {} &&", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot and")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Or => match (lhs.ty, rhs.ty) {
				(RuntimeType::Bool, RuntimeType::Bool) => Ok(Code {
					value: format!("{} {} ||", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot or")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Greater => match (lhs.ty, rhs.ty) {
				(RuntimeType::Num, RuntimeType::Num) => Ok(Code {
					value: format!("{} {} >", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::GreaterThanOrEqual => match (lhs.ty, rhs.ty) {
				(RuntimeType::Num, RuntimeType::Num) => Ok(Code {
					value: format!("{} {} >=", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Lesser => match (lhs.ty, rhs.ty) {
				(RuntimeType::Num, RuntimeType::Num) => Ok(Code {
					value: format!("{} {} <", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::LesserThanOrEqual => match (lhs.ty, rhs.ty) {
				(RuntimeType::Num, RuntimeType::Num) => Ok(Code {
					value: format!("{} {} <=", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::Equal => match (lhs.ty, rhs.ty) {
				(RuntimeType::Str, RuntimeType::Str) => Ok(Code {
					value: format!("{} {} scmp", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(RuntimeType::Num, RuntimeType::Num) | (RuntimeType::Bool, RuntimeType::Bool) => Ok(Code {
					value: format!("{} {} ==", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(RuntimeType::Enum(l_e), RuntimeType::Enum(r_e)) if l_e == r_e => match l_e {
					RuntimeEnumType::Inbuilt(e) => match e {
						InbuiltEnum::MouseEvent => Ok(Code {
							value: format!("{} {} scmi", lhs.value, rhs.value),
							ty: RuntimeType::Bool,
						}),
						_ => unreachable!(),
					},
					RuntimeEnumType::User(..) => Ok(Code {
						value: format!("{} {} ==", lhs.value, rhs.value),
						ty: RuntimeType::Bool,
					}),
				},
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot equate")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
			BinaryOperator::NotEqual => match (lhs.ty, rhs.ty) {
				(RuntimeType::Str, RuntimeType::Str) => Ok(Code {
					value: format!("{} {} scmp !", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(RuntimeType::Num, RuntimeType::Num) | (RuntimeType::Bool, RuntimeType::Bool) => Ok(Code {
					value: format!("{} {} !=", lhs.value, rhs.value),
					ty: RuntimeType::Bool,
				}),
				(RuntimeType::Enum(l_e), RuntimeType::Enum(r_e)) if l_e == r_e => match l_e {
					RuntimeEnumType::Inbuilt(e) => match e {
						InbuiltEnum::MouseEvent => Ok(Code {
							value: format!("{} {} scmi !", lhs.value, rhs.value),
							ty: RuntimeType::Bool,
						}),
						_ => unreachable!(),
					},
					RuntimeEnumType::User(..) => Ok(Code {
						value: format!("{} {} !=", lhs.value, rhs.value),
						ty: RuntimeType::Bool,
					}),
				},
				(lhs, rhs) => Err(vec![Diagnostic::new(Level::Error, "cannot compare")
					.add_label(Label::primary(
						format!("expression result is of type `{}`", lhs),
						left.1.clone(),
					))
					.add_label(Label::primary(
						format!("expression result is of type `{}`", rhs),
						right.1.clone(),
					))]),
			},
		}
	}

	fn compile_call(&mut self, call: &Call<'a>) -> Result<Code<'a>, Vec<Diagnostic>> {
		let function = self
			.evaluator
			.evaluate_as_function(&call.callee, "call target must be a function")?;
		if let FunctionValue::Inbuilt(inbuilt) = function {
			match inbuilt {
				InbuiltFunction::Format => self.compile_format(call.callee.1.clone(), &call.args),
			}
		} else {
			Err(vec![Diagnostic::new(
				Level::Error,
				"can only call inbuilt functions in RPN",
			)
			.add_label(Label::primary(
				"tried to call function here",
				call.callee.1.clone(),
			))])
		}
	}

	fn compile_rpn_access(&mut self, expr: &Expression<'a>) -> Result<Code<'a>, Vec<Diagnostic>> {
		let rpn = self
			.evaluator
			.evaluate_as_string(expr, "RPN variable access must result in a `string`")?;
		if let Some(ty) = rpn.chars().next() {
			match ty {
				'F' => Err(vec![Diagnostic::new(
					Level::Error,
					"cannot access an RPN inbuilt function",
				)
				.add_label(Label::primary(
					"this access tries to call an RPN function",
					expr.1.clone(),
				))]),
				'A' | 'E' | 'P' => {
					let ty = if let Some(byte) = rpn.find(',') {
						if rpn[byte..].to_lowercase().contains("bool") {
							RuntimeType::Bool
						} else {
							RuntimeType::Num
						}
					} else {
						return Err(vec![Diagnostic::new(Level::Error, "missing simvar unit")
							.add_label(Label::primary("simvars and envvars need units", expr.1.clone()))]);
					};
					Ok(Code {
						value: format!("({})", rpn),
						ty,
					})
				},
				'B' | 'G' | 'H' | 'I' | 'K' | 'L' | 'O' | 'R' | 'W' | 'Z' => {
					let ty = if let Some(byte) = rpn.find(',') {
						if rpn[byte..].to_lowercase().contains("bool") {
							RuntimeType::Bool
						} else {
							RuntimeType::Num
						}
					} else {
						RuntimeType::Num
					};
					Ok(Code {
						value: format!("({})", rpn),
						ty,
					})
				},
				'M' => Ok(if rpn.to_lowercase() == "m:event" {
					Code {
						value: "(M:Event)".to_string(),
						ty: RuntimeType::Enum(RuntimeEnumType::Inbuilt(InbuiltEnum::MouseEvent)),
					}
				} else {
					Code {
						value: format!("({})", rpn),
						ty: RuntimeType::Num,
					}
				}),
				_ => Err(vec![Diagnostic::new(Level::Error, "unknown RPN variable type")
					.add_label(Label::primary(
						"this access has an unknown variable type",
						expr.1.clone(),
					))]),
			}
		} else {
			Err(vec![Diagnostic::new(
				Level::Error,
				"RPN variable access string is empty",
			)
			.add_label(Label::primary("this string is empty", expr.1.clone()))])
		}
	}

	fn compile_if(&mut self, chain: &IfChain<'a>) -> Result<Code<'a>, Vec<Diagnostic>> {
		let mut errors = Vec::new();
		let mut value = String::new();

		let mut ifs = chain.ifs.iter();
		let first = ifs.next().unwrap();
		let condition = self.compile_expr(first.0.as_ref())?;
		if condition.ty != RuntimeType::Bool {
			errors.push(
				Diagnostic::new(Level::Error, "if condition must have a result of type `bool`").add_label(
					Label::primary(
						format!("this expression has a result of type `{}`", condition.ty),
						first.0 .1.clone(),
					),
				),
			)
		}
		value.push_str(&condition.value);
		value.push_str(" if{\n");
		self.indent_string(&mut value);
		let orig_block = self.compile_block(&first.1)?;
		value.push_str(&orig_block.value);
		value.push_str("}");

		let mut else_if_count = 0;
		for else_if in ifs {
			else_if_count += 1;
			let condition = self.compile_expr(else_if.0.as_ref())?;
			if condition.ty != RuntimeType::Bool {
				errors.push(
					Diagnostic::new(Level::Error, "if condition must have a result of type `bool`").add_label(
						Label::primary(
							format!("this expression has a result of type `{}`", condition.ty),
							else_if.0 .1.clone(),
						),
					),
				)
			}
			value.push_str(" els{ ");
			value.push_str(&condition.value);
			value.push_str(" if{\n");
			self.indent_string(&mut value);
			let block = self.compile_block(&first.1)?;
			value.push_str(&block.value);
			value.push_str("}");

			if block.ty != orig_block.ty {
				errors.push(
					Diagnostic::new(Level::Error, "mismatched expression types")
						.add_label(Label::primary(
							format!("this block has a result of type `{}`...", block.ty),
							else_if.1.loc.clone(),
						))
						.add_label(Label::secondary(
							format!("...but a value of type `{}` was expected", orig_block.ty),
							first.1.loc.clone(),
						)),
				)
			}
		}

		let ty = if let Some(els) = &chain.else_part {
			value.push_str(" els{\n");
			self.indent_string(&mut value);
			let block = self.compile_block(&els.0)?;
			value.push_str(&block.value);
			value.push_str("}");

			if block.ty != orig_block.ty {
				errors.push(
					Diagnostic::new(Level::Error, "mismatched expression types")
						.add_label(Label::primary(
							format!("this block has a result of type `{}`...", block.ty),
							els.0.loc.clone(),
						))
						.add_label(Label::secondary(
							format!("...but a value of type `{}` was expected", orig_block.ty),
							first.1.loc.clone(),
						)),
				)
			}

			orig_block.ty
		} else {
			RuntimeType::None
		};

		for _ in 0..else_if_count {
			value.push_str(" }");
		}

		if errors.len() == 0 {
			Ok(Code { value, ty })
		} else {
			Err(errors)
		}
	}

	fn compile_return(&mut self, expr: Option<&Expression<'a>>) -> Result<Code<'a>, Vec<Diagnostic>> {
		Ok(if let Some(expr) = expr {
			let mut code = self.compile_expr(expr)?;
			code.value.push_str(" quit");
			code
		} else {
			Code {
				value: "quit".to_string(),
				ty: RuntimeType::None,
			}
		})
	}

	fn compile_format(&mut self, loc: Location<'a>, args: &[Expression<'a>]) -> Result<Code<'a>, Vec<Diagnostic>> {
		if let Some(arg) = args.get(0) {
			let mut fmt_string = self
				.evaluator
				.evaluate_as_string(arg, "format string must be of type `str`")?;
			let format_replacement = fmt_string.matches("{}");
			let arity = format_replacement.count();
			if arity == args.len() - 1 {
				let mut errors = Vec::new();
				// I hate strings, please don't sue me.
				let mut values = Vec::new();
				for expr in args[1..].iter() {
					let value = self.compile_expr(expr)?;
					let replace = match value.ty {
						RuntimeType::Str => "%s",
						RuntimeType::Num => "%f",
						RuntimeType::Bool => "%s",
						_ => {
							errors.push(
								Diagnostic::new(Level::Error, "can only format primitive types").add_label(
									Label::primary(
										format!("this expression has a result of type `{}`", value.ty),
										expr.1.clone(),
									),
								),
							);
							continue;
						},
					};
					fmt_string = fmt_string.replacen("{}", &replace, 1);
					values.push(value);
				}

				let mut value = String::new();

				for code in values.into_iter().rev() {
					let data = match code.ty {
						RuntimeType::Str => code.value,
						RuntimeType::Num => code.value,
						RuntimeType::Bool => format!("{} if{{ 'true' }} els{{ 'false' }}", code.value),
						_ => "".to_string(),
					};
					value.push_str(&data);
					value.push(' ');
				}
				value.push('\'');
				value.push_str(&fmt_string);
				value.push('\'');
				value.push_str(" (F:Format)");

				if errors.len() == 0 {
					Ok(Code {
						value,
						ty: RuntimeType::Str,
					})
				} else {
					Err(errors)
				}
			} else {
				Err(vec![Diagnostic::new(
					Level::Error,
					"incorrect number of format arguments",
				)
				.add_label(Label::primary(
					format!("expected {} arguments, found {}", arity, args.len() - 1),
					loc,
				))])
			}
		} else {
			Err(vec![Diagnostic::new(Level::Error, "missing format string")
				.add_label(Label::primary("in this invocation of `format`", loc))])
		}
	}

	fn indent_string(&self, s: &mut String) {
		s.extend(IndentIterator {
			indentation: self.indent as usize,
		});
	}
}

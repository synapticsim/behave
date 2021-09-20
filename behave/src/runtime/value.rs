use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::ast::{
	Animation,
	Block,
	Component,
	Enum,
	EnumAccess,
	Function,
	FunctionType,
	Ident,
	InbuiltFunction,
	ResolvedType,
	Struct,
	TypeType,
};
use crate::diagnostic::{Diagnostic, Label, Level};
use crate::items::{ItemMap, StructId};

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeType<'a> {
	Num,
	Str,
	Bool,
	Code,
	Struct(&'a Struct<'a>),
	Enum(&'a Enum<'a>),
	Array(Box<RuntimeType<'a>>),
	Function(RuntimeFunctionType<'a>),
	None,
	Component,
	Animation,
}

impl<'a> RuntimeType<'a> {
	pub fn from(item_map: &'a ItemMap<'a>, ty: &TypeType<'a>) -> Self {
		match ty {
			TypeType::Num => Self::Num,
			TypeType::Str => Self::Str,
			TypeType::Bool => Self::Bool,
			TypeType::Code => Self::Code,
			TypeType::User(ref u) => match u.resolved.unwrap() {
				ResolvedType::Struct(id) => Self::Struct(item_map.get_struct(id)),
				ResolvedType::Enum(id) => Self::Enum(item_map.get_enum(id)),
			},
			TypeType::Array(ref ty) => Self::Array(Box::new(Self::from(item_map, &ty.0))),
			TypeType::Function(ref f) => Self::Function(RuntimeFunctionType::from(item_map, f)),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeFunctionType<'a> {
	pub args: Vec<RuntimeType<'a>>,
	pub ret: Option<Box<RuntimeType<'a>>>,
}

impl<'a> RuntimeFunctionType<'a> {
	pub fn from(item_map: &'a ItemMap<'a>, ty: &FunctionType<'a>) -> Self {
		RuntimeFunctionType {
			args: ty.args.iter().map(|arg| RuntimeType::from(item_map, &arg.0)).collect(),
			ret: ty.ret.as_ref().map(|ret| Box::new(RuntimeType::from(item_map, &ret.0))),
		}
	}
}

impl Display for RuntimeType<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let val = match self {
			Self::Num => "num".to_string(),
			Self::Str => "str".to_string(),
			Self::Bool => "bool".to_string(),
			Self::Code => "code".to_string(),
			Self::Struct(s) => s.name.0.clone(),
			Self::Enum(e) => e.name.0.clone(),
			Self::Array(ty) => format!("[{}]", *ty),
			Self::Function(f) => f.to_string(),
			Self::None => "none".to_string(),
			Self::Component => "component".to_string(),
			Self::Animation => "animation".to_string(),
		};
		write!(f, "{}", val)
	}
}

impl Display for RuntimeFunctionType<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "fn (")?;
		let mut iter = self.args.iter();
		if let Some(arg) = iter.next() {
			write!(f, "{}", arg)?;
		}
		while let Some(arg) = iter.next() {
			write!(f, ", {}", arg)?;
		}
		write!(f, ")")?;
		if let Some(ret) = &self.ret {
			write!(f, " -> {}", ret)?;
		}

		Ok(())
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
	String(String),
	Number(f64),
	Boolean(bool),
	Function(FunctionValue<'a>),
	Object(Object<'a>),
	Enum(EnumAccess),
	Array(RuntimeType<'a>, Vec<Value<'a>>),
	Code(Block<'a>),
	None,
	Component(RuntimeComponent),
	Animation(RuntimeAnimation),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionValue<'a> {
	User(Function<'a>),
	Inbuilt(InbuiltFunction),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Object<'a> {
	pub id: StructId,
	pub fields: HashMap<String, Value<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeComponent {}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeAnimation {}

impl<'a> Value<'a> {
	pub fn get_type(&self, item_map: &'a ItemMap<'a>) -> RuntimeType<'a> {
		match self {
			Self::String(_) => RuntimeType::Str,
			Self::Number(_) => RuntimeType::Num,
			Self::Boolean(_) => RuntimeType::Bool,
			Self::Function(f) => match f {
				FunctionValue::User(f) => RuntimeType::Function(RuntimeFunctionType {
					args: f
						.args
						.iter()
						.map(|arg| RuntimeType::from(item_map, &arg.ty.0))
						.collect(),
					ret: f.ret.as_ref().map(|ret| Box::new(RuntimeType::from(item_map, &ret.0))),
				}),
				_ => unreachable!("ICE: tried to get type of inbuilt function"),
			},
			Self::Object(obj) => RuntimeType::Struct(item_map.get_struct(obj.id)),
			Self::Enum(access) => RuntimeType::Enum(item_map.get_enum(access.id)),
			Self::Array(ty, _) => RuntimeType::Array(Box::new(ty.clone())),
			Self::Code(_) => RuntimeType::Code,
			Self::None => RuntimeType::None,
			Self::Component(_) => RuntimeType::Component,
			Self::Animation(_) => RuntimeType::Animation,
		}
	}
}

#[derive(Debug)]
pub struct VisibleScope<'a> {
	vars: HashMap<String, Value<'a>>,
}

#[derive(Debug)]
pub struct CallFrame<'a> {
	scopes: Vec<VisibleScope<'a>>,
}

#[derive(Debug)]
pub struct CallStack<'a> {
	frames: Vec<CallFrame<'a>>,
}

impl<'a> CallStack<'a> {
	pub fn new() -> Self {
		Self {
			frames: vec![CallFrame { scopes: Vec::new() }],
		}
	}

	pub fn new_var(&mut self, name: &Ident, value: Value<'a>) {
		self.frames
			.last_mut()
			.unwrap()
			.scopes
			.last_mut()
			.unwrap()
			.vars
			.insert(name.0.clone(), value);
	}

	pub fn set_var(&mut self, name: &Ident) -> Result<&mut Value<'a>, Diagnostic> {
		if let Some(val) = self
			.frames
			.last_mut()
			.unwrap()
			.scopes
			.last_mut()
			.unwrap()
			.vars
			.get_mut(&name.0)
		{
			Ok(val)
		} else {
			Err(Diagnostic::new(Level::Error, "local variable does not exist")
				.add_label(Label::primary("accessed here", name.1.clone())))
		}
	}

	pub fn var(&self, name: &Ident) -> Result<Value<'a>, Diagnostic> {
		let frame = self.frames.last().unwrap();
		for scope in frame.scopes.iter().rev() {
			if let Some(value) = scope.vars.get(&name.0) {
				return Ok(value.clone());
			}
		}

		Err(Diagnostic::new(Level::Error, "local variable does not exist")
			.add_label(Label::primary("accessed here", name.1.clone())))
	}

	pub fn scope(&mut self) {
		self.frames
			.last_mut()
			.unwrap()
			.scopes
			.push(VisibleScope { vars: HashMap::new() })
	}

	pub fn end_scope(&mut self) { self.frames.last_mut().unwrap().scopes.pop(); }

	pub fn call(&mut self, args: impl Iterator<Item = (String, Value<'a>)>) {
		self.frames.push(CallFrame {
			scopes: vec![VisibleScope {
				vars: {
					let mut map = HashMap::new();
					for arg in args {
						map.insert(arg.0, arg.1);
					}
					map
				},
			}],
		})
	}

	pub fn end_call(&mut self) { self.frames.pop(); }
}

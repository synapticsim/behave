use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

use crate::ast::{
	Cursor,
	Direction,
	Enum,
	EnumAccess,
	EnumType,
	Function,
	FunctionType,
	Hitbox,
	Ident,
	InbuiltEnum,
	InbuiltFunction,
	InbuiltStruct,
	InteractionMode,
	Location,
	MouseEvent,
	ResolvedType,
	Struct,
	StructType,
	TypeType,
};
use crate::diagnostic::{Diagnostic, Label, Level};
use crate::items::{EnumId, ItemMap, StructId};

#[derive(Clone, Debug)]
pub enum RuntimeType<'a> {
	Num,
	Str,
	Bool,
	Struct(RuntimeStructType<'a>),
	Enum(RuntimeEnumType<'a>),
	Array(Box<RuntimeType<'a>>),
	Map(Box<RuntimeType<'a>>, Box<RuntimeType<'a>>),
	Sum(Vec<RuntimeType<'a>>),
	Function(RuntimeFunctionType<'a>),
	None,
	Code,
	TemplateValue,
}

#[derive(Clone, Debug)]
pub enum RuntimeEnumType<'a> {
	User(EnumId, &'a Enum<'a>),
	Inbuilt(InbuiltEnum),
	Unknown,
}

#[derive(Clone, Debug)]
pub enum RuntimeStructType<'a> {
	User(StructId, &'a Struct<'a>),
	Inbuilt(InbuiltStruct),
}

impl<'a> PartialEq for RuntimeEnumType<'a> {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(RuntimeEnumType::User(l, _), RuntimeEnumType::User(r, _)) => l == r,
			(RuntimeEnumType::Inbuilt(l), RuntimeEnumType::Inbuilt(r)) => l == r,
			(RuntimeEnumType::Unknown, _) => true,
			(_, RuntimeEnumType::Unknown) => true,
			_ => false,
		}
	}
}

impl<'a> PartialEq for RuntimeStructType<'a> {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(RuntimeStructType::User(l, _), RuntimeStructType::User(r, _)) => l == r,
			(RuntimeStructType::Inbuilt(l), RuntimeStructType::Inbuilt(r)) => l == r,
			_ => false,
		}
	}
}

impl<'a> RuntimeType<'a> {
	pub fn from(item_map: &'a ItemMap<'a>, ty: &TypeType<'a>) -> Self {
		match ty {
			TypeType::Num => Self::Num,
			TypeType::Str => Self::Str,
			TypeType::Bool => Self::Bool,
			TypeType::Other(ref u) => match u.resolved.unwrap() {
				ResolvedType::Struct(id) => match id {
					StructType::User(id) => Self::Struct(RuntimeStructType::User(id, item_map.get_struct(id))),
					StructType::Inbuilt(id) => Self::Struct(RuntimeStructType::Inbuilt(id)),
				},
				ResolvedType::Enum(id) => match id {
					EnumType::User(id) => Self::Enum(RuntimeEnumType::User(id, item_map.get_enum(id))),
					EnumType::Inbuilt(id) => Self::Enum(RuntimeEnumType::Inbuilt(id)),
				},
			},
			TypeType::Array(ref ty) => Self::Array(Box::new(Self::from(item_map, &ty.0))),
			TypeType::Map(ref from, ref to) => Self::Map(
				Box::new(Self::from(item_map, &from.0)),
				Box::new(Self::from(item_map, &to.0)),
			),
			TypeType::Sum(ref ty) => Self::Sum(ty.iter().map(|ty| RuntimeType::from(item_map, &ty.0)).collect()),
			TypeType::Function(ref f) => Self::Function(RuntimeFunctionType::from(item_map, f)),
			TypeType::Code => Self::Code,
		}
	}
}

impl<'a> PartialEq for RuntimeType<'a> {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(RuntimeType::Num, RuntimeType::Num)
			| (RuntimeType::Str, RuntimeType::Str)
			| (RuntimeType::Bool, RuntimeType::Bool)
			| (RuntimeType::None, RuntimeType::None)
			| (RuntimeType::Code, RuntimeType::Code)
			| (RuntimeType::TemplateValue, RuntimeType::TemplateValue) => true,
			(RuntimeType::Struct(l), RuntimeType::Struct(r)) => l == r,
			(RuntimeType::Enum(l), RuntimeType::Enum(r)) => l == r,
			(RuntimeType::Array(l), RuntimeType::Array(r)) => l == r,
			(RuntimeType::Map(lk, lv), RuntimeType::Map(rk, rv)) => lk == rk && lv == rv,
			(RuntimeType::Sum(l), RuntimeType::Sum(r)) => l == r,
			(RuntimeType::Sum(l), r) => l.contains(r),
			(l, RuntimeType::Sum(r)) => r.contains(l),
			(RuntimeType::Function(l), RuntimeType::Function(r)) => l == r,
			_ => false,
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
			Self::Struct(s) => match s {
				RuntimeStructType::User(_, e) => e.name.0.clone(),
				RuntimeStructType::Inbuilt(e) => e.to_string(),
			},
			Self::Enum(e) => match e {
				RuntimeEnumType::User(_, e) => e.name.0.clone(),
				RuntimeEnumType::Inbuilt(e) => e.to_string(),
				RuntimeEnumType::Unknown => "unknown".to_string(),
			},
			Self::Array(ty) => format!("[{}]", *ty),
			Self::Map(from, to) => format!("[{} : {}]", *from, *to),
			Self::Sum(ty) => {
				let mut iter = ty.iter();
				let mut str = iter.next().unwrap().to_string();
				for ty in iter {
					str.push_str(" | ");
					str.push_str(&ty.to_string());
				}
				str
			},
			Self::Function(f) => f.to_string(),
			Self::None => "none".to_string(),
			Self::TemplateValue => "template value".to_string(),
			Self::Code => "rpn code".to_string(),
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
		for arg in iter {
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
	Map(RuntimeType<'a>, RuntimeType<'a>, Vec<(Value<'a>, Value<'a>)>),
	Code(Code<'a>),
	None,
	Template(TemplateValue<'a>),
}

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
				_ => unreachable!("tried to get type of inbuilt function"),
			},
			Self::Object(obj) => RuntimeType::Struct(obj.id.clone()),
			Self::Enum(access) => match access.id {
				EnumType::Inbuilt(e) => RuntimeType::Enum(RuntimeEnumType::Inbuilt(e)),
				EnumType::User(e) => RuntimeType::Enum(RuntimeEnumType::User(e, item_map.get_enum(e))),
			},
			Self::Array(ty, _) => RuntimeType::Array(Box::new(ty.clone())),
			Self::Map(key, value, _) => RuntimeType::Map(Box::new(key.clone()), Box::new(value.clone())),
			Self::None => RuntimeType::None,
			Self::Template(_) => RuntimeType::TemplateValue,
			Self::Code(..) => RuntimeType::Code,
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Code<'a> {
	pub ty: RuntimeType<'a>,
	pub value: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TemplateValue<'a> {
	Component(RuntimeComponent<'a>),
	Animation(RuntimeAnimation<'a>),
	Visibility(String),
	Emissive(String),
	Interaction(RuntimeInteraction<'a>),
	Events((String, Location<'a>), Vec<RuntimeEvent>),
	Block(Vec<TemplateValue<'a>>),
	Update(RuntimeUpdate),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionValue<'a> {
	User(Function<'a>),
	Inbuilt(InbuiltFunction),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Object<'a> {
	pub id: RuntimeStructType<'a>,
	pub fields: HashMap<String, Value<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeComponent<'a> {
	pub name: String,
	pub node: Option<(String, Location<'a>)>,
	pub items: Vec<TemplateValue<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeAnimation<'a> {
	pub name: (String, Location<'a>),
	pub length: f64,
	pub lag: f64,
	pub value: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeUpdate {
	pub frequency: Option<f64>,
	pub mode: InteractionMode,
	pub code: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MultipleCursors {
	pub cursors: HashSet<(Hitbox, Cursor)>,
	pub center_radius: f64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Cursors {
	Multiple(MultipleCursors),
	Single(Cursor),
}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeInteraction<'a> {
	pub legacy_cursors: Cursors,
	pub lock_cursors: Cursors,
	pub legacy_events: Vec<MouseEvent>,
	pub lock_events: Vec<MouseEvent>,
	pub legacy_callback: String,
	pub lock_callback: String,
	pub lock_tooltip_title: String,
	pub lock_tooltips: Vec<String>,
	pub can_lock: bool,
	pub node_to_highlight: Option<(String, Location<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeEvent {
	pub time: f64,
	pub direction: Direction,
	pub sounds: Vec<String>,
	pub effects: Vec<String>,
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

	pub fn var(&mut self, name: &Ident) -> Result<&mut Value<'a>, Diagnostic> {
		for scope in self.frames.last_mut().unwrap().scopes.iter_mut().rev() {
			if let Some(val) = scope.vars.get_mut(&name.0) {
				return Ok(val);
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

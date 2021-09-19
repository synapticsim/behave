use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Range;

use crate::diagnostic::{Diagnostic, Level};
use crate::items::{EnumId, StructId, TemplateId};

#[derive(Clone, Debug)]
pub enum ASTTree<'a> {
	Branch(HashMap<String, ASTTree<'a>>),
	Leaf(AST<'a>),
}

impl<'a> ASTTree<'a> {
	pub fn new() -> Self { Self::Branch(HashMap::new()) }

	pub fn add_ast(&mut self, path: &[String], ast: AST<'a>) -> bool {
		match self {
			Self::Branch(ref mut map) => {
				if path.len() == 1 {
					map.insert(path[0].clone(), ASTTree::Leaf(ast));
					true
				} else {
					let tree = map.entry(path[0].clone()).or_insert(ASTTree::new());
					tree.add_ast(&path[1..], ast)
				}
			},
			_ => false,
		}
	}

	pub fn get_ast(&self, path: &[Ident]) -> Result<&ASTTree, Diagnostic> {
		match self {
			Self::Branch(map) => {
				if path.len() == 0 {
					Ok(self)
				} else if let Some(ast) = map.get(&path[0].0) {
					ast.get_ast(&path[1..])
				} else {
					Err(Diagnostic::new(Level::Error, "path does not exist"))
				}
			},
			Self::Leaf(_) => {
				if path.len() == 0 {
					Ok(self)
				} else {
					Err(Diagnostic::new(Level::Error, "path refers to file as folder"))
				}
			},
		}
	}
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Location<'a> {
	pub range: Range<usize>,
	pub file: &'a [String],
}

#[derive(Clone, Debug, PartialEq)]
pub enum ASTType<'a> {
	Main(LODs<'a>, Behavior<'a>),
	Secondary(Vec<Item<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AST<'a> {
	pub imports: Vec<Import<'a>>,
	pub ast_data: ASTType<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LOD<'a> {
	pub min_size: Expression<'a>,
	pub file: Expression<'a>,
	pub loc: Location<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LODs<'a>(pub Vec<LOD<'a>>, pub Range<usize>);

#[derive(Clone, Debug, PartialEq)]
pub struct Behavior<'a>(pub Vec<Statement<'a>>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub enum ItemType<'a> {
	Function(Ident<'a>, Function<'a>),
	Template(TemplateId),
	Struct(StructId),
	Enum(EnumId),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Variable<'a> {
	pub name: Ident<'a>,
	pub value: Option<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant<'a> {
	pub name: Ident<'a>,
	pub value: usize,
	pub loc: Location<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Enum<'a> {
	pub name: Ident<'a>,
	pub variants: Vec<EnumVariant<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct<'a> {
	pub name: Ident<'a>,
	pub fields: Vec<VarEntry<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item<'a>(pub ItemType<'a>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType<'a> {
	pub args: Vec<Type<'a>>,
	pub ret: Option<Box<Type<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Template<'a> {
	pub name: Ident<'a>,
	pub args: Vec<VarEntry<'a>>,
	pub block: Vec<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeType<'a> {
	Num,
	Str,
	Bool,
	Code,
	User(UserType<'a>),
	Array(Box<Type<'a>>),
	Function(FunctionType<'a>),
	None,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UserType<'a> {
	pub path: Path<'a>,
	pub resolved: Option<ResolvedType>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ResolvedType {
	Struct(StructId),
	Enum(EnumId),
}

fn path_to_str<'a>(mut path: impl Iterator<Item = &'a String>) -> String {
	let mut s = String::new();
	s += &path.next().unwrap();
	while let Some(p) = path.next() {
		s.push('.');
		s += &p;
	}
	s
}

impl Display for TypeType<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		use TypeType::*;
		let val = match self {
			Num => "num".to_string(),
			Str => "str".to_string(),
			Bool => "bool".to_string(),
			Code => "code".to_string(),
			User(p) => path_to_str(p.path.0.iter().map(|p| &p.0)),
			Array(ty) => format!("[{}]", ty.0),
			Function(f) => f.to_string(),
			None => "none".to_string(),
		};
		write!(f, "{}", val)
	}
}

impl Display for FunctionType<'_> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "fn (")?;
		let mut iter = self.args.iter();
		if let Some(arg) = iter.next() {
			write!(f, "{}", arg.0)?;
		}
		while let Some(arg) = iter.next() {
			write!(f, ", {}", arg.0)?;
		}
		write!(f, ")")?;
		if let Some(ret) = &self.ret {
			write!(f, " -> {}", ret.0)?;
		}

		Ok(())
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type<'a>(pub TypeType<'a>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub struct VarEntry<'a> {
	pub name: Ident<'a>,
	pub ty: Type<'a>,
	pub default: Option<Box<Expression<'a>>>,
	pub loc: Location<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionType<'a> {
	None,
	String(String),
	Number(f64),
	Boolean(bool),
	Block(Block<'a>),
	Function(Function<'a>),
	Code(Block<'a>),
	Array(Vec<Expression<'a>>),
	Access(Access<'a>),
	StructCreate(StructCreate<'a>),
	RPNAccess(Box<Expression<'a>>),
	Index(Index<'a>),
	Assignment(Assignment<'a>),
	Unary(UnaryOperator, Box<Expression<'a>>),
	Binary(Box<Expression<'a>>, BinaryOperator, Box<Expression<'a>>),
	Call(Call<'a>),
	IfChain(IfChain<'a>),
	Switch(Switch<'a>),
	While(While<'a>),
	For(For<'a>),
	Return(Option<Box<Expression<'a>>>),
	Break(Option<Box<Expression<'a>>>),
	Use(Use<'a>),
	Component(Component<'a>),
	Animation(Animation<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expression<'a>(pub ExpressionType<'a>, pub Location<'a>);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
	Negate,
	Not,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperator {
	Add,
	Subtract,
	Multiply,
	Divide,
	And,
	Or,
	Equal,
	NotEqual,
	Greater,
	Lesser,
	GreaterThanOrEqual,
	LesserThanOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructCreate<'a> {
	pub ty: UserType<'a>,
	pub values: Vec<(Ident<'a>, Expression<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Index<'a> {
	pub array: Box<Expression<'a>>,
	pub index: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Access<'a> {
	pub path: Path<'a>,
	pub resolved: Option<ResolvedAccess<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedAccess<'a> {
	Local(usize),
	Global(GlobalAccess<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum GlobalAccess<'a> {
	EnumVariant(EnumVariant<'a>),
	Function(Function<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentTarget<'a> {
	Var(Access<'a>),
	RPNVar(Box<Expression<'a>>),
	Index(Access<'a>, Box<Expression<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment<'a> {
	pub target: AssignmentTarget<'a>,
	pub value: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Switch<'a> {
	pub on: Box<Expression<'a>>,
	pub cases: Vec<Case<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Case<'a> {
	pub value: Box<Expression<'a>>,
	pub code: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a> {
	pub statements: Vec<Statement<'a>>,
	pub expression: Option<Box<Expression<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call<'a> {
	pub callee: Box<Expression<'a>>,
	pub args: Vec<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfChain<'a> {
	pub ifs: Vec<(Box<Expression<'a>>, Block<'a>, Location<'a>)>,
	pub else_part: Option<(Block<'a>, Location<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While<'a> {
	pub condition: Box<Expression<'a>>,
	pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct For<'a> {
	pub var: Ident<'a>,
	pub container: Box<Expression<'a>>,
	pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'a> {
	pub args: Vec<VarEntry<'a>>,
	pub ret: Option<Type<'a>>,
	pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Use<'a> {
	pub template: UseTarget<'a>,
	pub args: Vec<(Ident<'a>, Expression<'a>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UseTarget<'a> {
	pub path: Path<'a>,
	pub resolved: Option<TemplateId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Component<'a> {
	pub name: Box<Expression<'a>>,
	pub node: Option<Box<Expression<'a>>>,
	pub block: Vec<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Animation<'a> {
	pub name: Box<Expression<'a>>,
	pub length: Box<Expression<'a>>,
	pub lag: Box<Expression<'a>>,
	pub code: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementType<'a> {
	Expression(ExpressionType<'a>),
	Declaration(Variable<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement<'a>(pub StatementType<'a>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub enum ImportType<'a> {
	Normal(Path<'a>),
	Extern(Expression<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import<'a>(pub ImportType<'a>, pub Location<'a>);

#[derive(Clone, Debug, PartialEq)]
pub struct Path<'a>(pub Vec<Ident<'a>>, pub Location<'a>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ident<'a>(pub String, pub Location<'a>);

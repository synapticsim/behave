use std::collections::HashMap;
use std::ops::Range;

use crate::diagnostic::{Diagnostic, Level};

#[derive(Debug)]
pub enum ASTTree {
	Branch(HashMap<String, ASTTree>),
	Leaf(AST),
}

impl ASTTree {
	pub fn new() -> Self { Self::Branch(HashMap::new()) }

	pub fn add_ast(&mut self, path: &[String], ast: AST) -> bool {
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

#[derive(Clone, Debug, PartialEq)]
pub enum ASTType {
	Main(LODs, Behavior),
	Secondary(Vec<Item>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AST {
	pub imports: Vec<Import>,
	pub ast_data: ASTType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LOD {
	pub min_size: Expression,
	pub file: Expression,
	pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LODs(pub Vec<LOD>, pub Range<usize>);

#[derive(Clone, Debug, PartialEq)]
pub struct Behavior(pub Vec<Statement>, pub Range<usize>);

#[derive(Clone, Debug, PartialEq)]
pub enum ItemType {
	Function(Ident, Function),
	Variable(Variable),
	Template(Template),
	Struct(Struct),
	Enum(Enum),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
	pub name: Ident,
	pub value: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
	pub name: Ident,
	pub value: Option<Expression>,
	pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Enum {
	pub name: Ident,
	pub variants: Vec<EnumVariant>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
	pub name: Ident,
	pub fields: Vec<VarEntry>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item(pub ItemType, pub Range<usize>);

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
	pub args: Vec<Type>,
	pub ret: Option<Box<Type>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Template {
	pub name: Ident,
	pub args: Vec<VarEntry>,
	pub block: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeType {
	Num,
	Str,
	Bool,
	Code,
	User(Ident),
	Array(Box<Type>),
	Function(FunctionType),
	Optional(Box<Type>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type(pub TypeType, pub Range<usize>);

#[derive(Clone, Debug, PartialEq)]
pub struct VarEntry {
	pub name: Ident,
	pub ty: Type,
	pub default: Option<Box<Expression>>,
	pub range: Range<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionType {
	None,
	String(String),
	Number(f64),
	Boolean(bool),
	Block(Block),
	Function(Function),
	Code(Block),
	Array(Vec<Expression>),
	Access(Path),
	RPNAccess(Box<Expression>),
	Index(Index),
	Assignment(Assignment),
	Unary(UnaryOperator, Box<Expression>),
	Binary(Box<Expression>, BinaryOperator, Box<Expression>),
	Call(Call),
	IfChain(IfChain),
	Switch(Switch),
	While(While),
	For(For),
	Return(Option<Box<Expression>>),
	Break(Option<Box<Expression>>),
	Use(Use),
	Component(Component),
	Animation(Animation),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expression(pub ExpressionType, pub Range<usize>);

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
pub struct Index {
	pub array: Box<Expression>,
	pub index: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignmentTarget {
	Var(Path),
	RPNVar(Box<Expression>),
	Index(Path, Box<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
	pub target: AssignmentTarget,
	pub value: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Switch {
	pub on: Box<Expression>,
	pub cases: Vec<Case>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Case {
	pub value: Box<Expression>,
	pub code: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
	pub statements: Vec<Statement>,
	pub expression: Option<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
	pub callee: Box<Expression>,
	pub args: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfChain {
	pub ifs: Vec<(Box<Expression>, Block, Range<usize>)>,
	pub else_part: Option<(Block, Range<usize>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
	pub condition: Box<Expression>,
	pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct For {
	pub var: Ident,
	pub container: Box<Expression>,
	pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
	pub params: Vec<VarEntry>,
	pub ret: Option<Type>,
	pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Use {
	pub template: Path,
	pub args: Vec<(Ident, Expression)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Component {
	pub name: Box<Expression>,
	pub node: Option<Box<Expression>>,
	pub block: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Animation {
	pub name: Box<Expression>,
	pub length: Box<Expression>,
	pub lag: Box<Expression>,
	pub code: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementType {
	Expression(ExpressionType),
	Declaration(Variable),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement(pub StatementType, pub Range<usize>);

#[derive(Clone, Debug, PartialEq)]
pub enum ImportType {
	Normal(Path),
	Extern(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import(pub ImportType, pub Range<usize>);

#[derive(Clone, Debug, PartialEq)]
pub struct Path(pub Vec<Ident>, pub Range<usize>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ident(pub String, pub Range<usize>);

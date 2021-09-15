use std::ops::Range;

#[derive(Debug)]
pub enum ASTType {
	Main(LODs, Behaviors),
	Secondary(Vec<Item>),
}

#[derive(Debug)]
pub struct AST {
	pub imports: Vec<Import>,
	pub ast_data: ASTType,
}

#[derive(Debug)]
pub struct LOD {
	pub min_size: Expression,
	pub file: Expression,
	pub range: Range<usize>,
}

#[derive(Debug)]
pub struct LODs(pub Vec<LOD>, pub Range<usize>);

#[derive(Debug)]
pub struct Behaviors(pub Vec<Expression>, pub Range<usize>);

#[derive(Debug)]
pub enum ItemType {
	Function(Ident, Function),
	Variable(Variable),
	Template(Template),
	Struct(Struct),
	Enum(Enum),
}

#[derive(Debug)]
pub struct Variable {
	pub name: Ident,
	pub ty: Option<Type>,
	pub value: Option<Expression>,
}

#[derive(Debug)]
pub struct EnumVariant {
	pub name: Ident,
	pub value: Option<Expression>,
	pub range: Range<usize>,
}

#[derive(Debug)]
pub struct Enum {
	pub name: Ident,
	pub variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub struct Struct {
	pub name: Ident,
	pub fields: Vec<VarEntry>,
}

#[derive(Debug)]
pub struct Item(pub ItemType, pub Range<usize>);

#[derive(Debug)]
pub struct FunctionType {
	pub args: Vec<Type>,
	pub ret: Option<Box<Type>>,
}

#[derive(Debug)]
pub struct Template {
	pub name: Ident,
	pub args: VarEntry,
	pub block: Vec<Expression>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Type(pub TypeType, pub Range<usize>);

#[derive(Debug)]
pub struct VarEntry {
	pub name: Ident,
	pub ty: Type,
	pub range: Range<usize>,
}

#[derive(Debug)]
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
	Switch(Vec<Case>),
	While(While),
	For(For),
	Return(Option<Box<Expression>>),
	Break(Option<Box<Expression>>),
}

#[derive(Debug)]
pub struct Expression(pub ExpressionType, pub Range<usize>);

#[derive(Debug)]
pub enum UnaryOperator {
	Negate,
	Not,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Index {
	pub array: Box<Expression>,
	pub index: Box<Expression>,
}

#[derive(Debug)]
pub struct Assignment {
	pub variable: Box<Expression>,
	pub value: Box<Expression>,
}

#[derive(Debug)]
pub struct Case {
	pub value: Box<Expression>,
	pub code: Box<Expression>,
}

#[derive(Debug)]
pub struct Block {
	pub statements: Vec<Statement>,
	pub expression: Option<Box<Expression>>,
}

#[derive(Debug)]
pub struct Call {
	pub callee: Box<Expression>,
	pub args: Vec<Expression>,
}

#[derive(Debug)]
pub struct IfChain {
	pub ifs: Vec<(Box<Expression>, Block, Range<usize>)>,
	pub else_part: Option<(Block, Range<usize>)>,
}

#[derive(Debug)]
pub struct While {
	pub condition: Box<Expression>,
	pub block: Block,
}

#[derive(Debug)]
pub struct For {
	pub var: Ident,
	pub container: Box<Expression>,
	pub block: Block,
}

#[derive(Debug)]
pub struct Function {
	pub params: Vec<VarEntry>,
	pub ret: Option<Type>,
	pub block: Block,
}

#[derive(Debug)]
pub enum StatementType {
	Expression(ExpressionType),
	Declaration(Variable),
}

#[derive(Debug)]
pub struct Statement(pub StatementType, pub Range<usize>);

#[derive(Debug)]
pub enum ImportType {
	Normal(Path),
	Extern(Expression),
}

#[derive(Debug)]
pub struct Import(pub ImportType, pub Range<usize>);

#[derive(Debug)]
pub struct Path(pub Vec<Ident>, pub Range<usize>);

#[derive(Debug)]
pub struct Ident(pub String, pub Range<usize>);

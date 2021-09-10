use std::ops::Range;

pub enum ASTType {
	Main(LODs, Behaviors),
	Secondary(Vec<Item>),
}

pub struct AST {
	pub imports: Vec<Import>,
	pub ast_data: ASTType,
}

pub struct LOD {
	pub min_size: NumberLiteral,
	pub file: StringLiteral,
}

pub struct LODs(pub Vec<(LOD, Range<usize>)>, pub Range<usize>);

pub struct Behaviors(pub Range<usize>);

pub enum ItemType {
	Function,
	Variable,
	Template,
	Struct,
	Enum,
}

pub struct Item(pub ItemType, pub Range<usize>);

pub enum ImportType {
	Normal(Path),
	Extern(StringLiteral),
}

pub struct Import(pub ImportType, pub Range<usize>);

pub struct Path(pub Vec<Ident>, pub Range<usize>);

pub struct Ident(pub String, pub Range<usize>);

pub struct StringLiteral(pub String, pub Range<usize>);

pub struct NumberLiteral(pub f64, pub Range<usize>);

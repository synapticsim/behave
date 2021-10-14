use std::{
	fmt::{Display, Formatter},
	ops::Range,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
	pub start: u32,
	pub end: u32,
}

impl Default for Span {
	fn default() -> Self {
		Self {
			start: u32::MAX,
			end: u32::MIN,
		}
	}
}

impl From<Range<u32>> for Span {
	fn from(r: Range<u32>) -> Self {
		Self {
			start: r.start,
			end: r.end,
		}
	}
}

impl Into<Range<u32>> for Span {
	fn into(self) -> Range<u32> { self.start..self.end }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LiteralKind {
	Int,
	Float,
	String { terminated: bool },
	Bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
	Plus,
	Minus,
	Star,
	Slash,
	Percent,
	Lt,
	Gt,
	Eq,
	Not,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Delimiter {
	Paren,
	Brace,
	Bracket,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
	Struct,
	Impl,
	Enum,
	Function,
	Let,
	Lods,
	Behavior,
	Template,
	Import,
	Use,
	Mod,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
	Keyword(Keyword),
	Whitespace,
	Comment,
	Literal(LiteralKind),
	Ident,
	Question,
	At,
	Colon,
	Separator,
	Dot,
	DotDot,
	Comma,
	Semi,
	Pipe,
	Arrow,
	Op(Op),
	OpEq(Op),
	OpenDelim(Delimiter),
	CloseDelim(Delimiter),
	None,
	And,
	Or,
	As,
	Is,
	If,
	In,
	Else,
	Switch,
	While,
	For,
	Return,
	Break,
	Continue,
	Ref,
	Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token<'a> {
	pub kind: TokenKind,
	pub data: &'a str,
	pub span: Span,
}

impl AsRef<str> for TokenKind {
	fn as_ref(&self) -> &str {
		use Delimiter::*;
		use TokenKind::*;

		use self::{Keyword::*, Op::*};

		match self {
			Whitespace => unreachable!(),
			Comment => unreachable!(),
			Ident => "ident",
			Literal(kind) => match kind {
				LiteralKind::String { .. } => "string literal",
				LiteralKind::Int => "integer literal",
				LiteralKind::Float => "float literal",
				LiteralKind::Bool => "boolean literal",
			},
			Question => "?",
			At => "at",
			Colon => "colon",
			Separator => "::",
			Dot => "dot",
			DotDot => "..",
			Comma => "comma",
			Semi => "semicolon",
			Pipe => "|",
			Arrow => "->",
			Op(op) => match op {
				Plus => "+",
				Minus => "-",
				Star => "*",
				Slash => "/",
				Percent => "%",
				Lt => "<",
				Gt => ">",
				Eq => "=",
				Not => "!",
			},
			OpEq(op) => match op {
				Plus => "+=",
				Minus => "-=",
				Star => "*=",
				Slash => "/=",
				Percent => "%=",
				Lt => "<=",
				Gt => ">=",
				Eq => "==",
				Not => "!=",
			},
			OpenDelim(delim) => match delim {
				Brace => "{",
				Bracket => "[",
				Paren => "(",
			},
			CloseDelim(delim) => match delim {
				Brace => "}",
				Bracket => "]",
				Paren => ")",
			},
			None => "none",
			And => "and",
			Or => "or",
			If => "if",
			In => "in",
			As => "as",
			Is => "is",
			Else => "else",
			While => "while",
			For => "for",
			Keyword(keyword) => match keyword {
				Struct => "struct",
				Impl => "impl",
				Enum => "enum",
				Function => "fn",
				Let => "let",
				Lods => "lods",
				Behavior => "behavior",
				Template => "template",
				Import => "import",
				Use => "use",
				Mod => "mod",
			},
			Return => "return",
			Break => "break",
			Continue => "continue",
			Switch => "switch",
			Ref => "ref",
			Unknown => "unknown",
		}
	}
}

impl Display for TokenKind {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.as_ref()) }
}

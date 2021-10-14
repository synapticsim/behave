use std::{
	collections::HashMap,
	iter::{FusedIterator, Peekable},
	ops::Range,
	str::CharIndices,
};

use lazy_static::lazy_static;

use super::token::{Token, TokenKind};
use crate::syntax::token::{Delimiter, Keyword, LiteralKind, Op, Span};

#[derive(Clone)]
pub struct Lexer<'a> {
	source: Peekable<CharIndices<'a>>,
	source_raw: &'a str,
}

impl<'a> Lexer<'a> {
	pub fn new(source: &'a str) -> Self {
		Self {
			source: source.char_indices().peekable(),
			source_raw: source,
		}
	}
}

impl<'a> Lexer<'a> {
	fn tok(&self, kind: TokenKind, span: Range<usize>) -> Token<'a> {
		Token {
			kind,
			data: &self.source_raw[span.clone()],
			span: Span {
				start: span.start as u32,
				end: span.end as u32,
			},
		}
	}
}

impl<'a> Iterator for Lexer<'a> {
	type Item = Token<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		let c = self.source.next()?;

		Some(match c.1 {
			'?' => self.tok(TokenKind::Question, c.0..c.0 + 1),
			'@' => self.tok(TokenKind::At, c.0..c.0 + 1),
			'(' => self.tok(TokenKind::OpenDelim(Delimiter::Paren), c.0..c.0 + 1),
			')' => self.tok(TokenKind::CloseDelim(Delimiter::Paren), c.0..c.0 + 1),
			'{' => self.tok(TokenKind::OpenDelim(Delimiter::Brace), c.0..c.0 + 1),
			'}' => self.tok(TokenKind::CloseDelim(Delimiter::Brace), c.0..c.0 + 1),
			'[' => self.tok(TokenKind::OpenDelim(Delimiter::Bracket), c.0..c.0 + 1),
			']' => self.tok(TokenKind::CloseDelim(Delimiter::Bracket), c.0..c.0 + 1),
			',' => self.tok(TokenKind::Comma, c.0..c.0 + 1),
			';' => self.tok(TokenKind::Semi, c.0..c.0 + 1),
			'|' => self.tok(TokenKind::Pipe, c.0..c.0 + 1),
			'.' => {
				if self.source.next_if(|c| c.1 == '.').is_some() {
					self.tok(TokenKind::DotDot, c.0..c.0 + 2)
				} else {
					self.tok(TokenKind::Dot, c.0..c.0 + 1)
				}
			},
			':' => {
				if self.source.next_if(|c| c.1 == ':').is_some() {
					self.tok(TokenKind::Separator, c.0..c.0 + 2)
				} else {
					self.tok(TokenKind::Colon, c.0..c.0 + 1)
				}
			},
			'+' | '*' | '%' | '=' | '!' | '>' | '<' => {
				let op = match c.1 {
					'+' => Op::Plus,
					'-' => Op::Minus,
					'*' => Op::Star,
					'%' => Op::Percent,
					'=' => Op::Eq,
					'!' => Op::Not,
					'>' => Op::Gt,
					'<' => Op::Lt,
					_ => unreachable!(),
				};
				if self.source.next_if(|c| c.1 == '=').is_some() {
					self.tok(TokenKind::OpEq(op), c.0..c.0 + 2)
				} else {
					self.tok(TokenKind::Op(op), c.0..c.0 + 1)
				}
			},
			'-' => {
				if self.source.next_if(|c| c.1 == '=').is_some() {
					self.tok(TokenKind::OpEq(Op::Minus), c.0..c.0 + 2)
				} else if self.source.next_if(|c| c.1 == '>').is_some() {
					self.tok(TokenKind::Arrow, c.0..c.0 + 2)
				} else {
					self.tok(TokenKind::Op(Op::Minus), c.0..c.0 + 1)
				}
			},
			'/' => {
				if let Some(c) = self.source.peek() {
					let c = *c;
					match c.1 {
						'/' => {
							while {
								// Cursed do-while loop.
								self.source.next().map(|c| c.1 != '\n').unwrap_or(false)
							} {}
							self.tok(TokenKind::Comment, c.0..c.0)
						},
						'=' => {
							self.source.next();
							self.tok(TokenKind::OpEq(Op::Slash), c.0..c.0 + 2)
						},
						_ => self.tok(TokenKind::Op(Op::Slash), c.0..c.0 + 1),
					}
				} else {
					self.tok(TokenKind::Op(Op::Slash), c.0..c.0 + 1)
				}
			},
			'"' => {
				let start_byte_index = c.0;
				let mut end_byte_index = c.0 + 1;
				loop {
					if let Some(c) = self.source.next() {
						end_byte_index = c.0 + 1;
						if c.1 == '\\' {
							self.source.next();
						} else if c.1 == '\n' {
							break self.tok(
								TokenKind::Literal(LiteralKind::String { terminated: false }),
								start_byte_index..end_byte_index,
							);
						} else if c.1 == '"' {
							break self.tok(
								TokenKind::Literal(LiteralKind::String { terminated: true }),
								start_byte_index..end_byte_index,
							);
						}
					} else {
						break self.tok(
							TokenKind::Literal(LiteralKind::String { terminated: false }),
							start_byte_index..end_byte_index,
						);
					}
				}
			},
			'0'..='9' => {
				let start_byte_index = c.0;
				let mut decimal = false;
				let mut end_byte_index = c.0;
				while let Some(c) = self.source.peek() {
					end_byte_index = c.0;
					if c.1 == '.' {
						if decimal {
							break;
						}
						decimal = true;
					} else if c.1 > '9' || c.1 < '0' {
						break;
					}
					self.source.next();
				}
				if decimal {
					self.tok(TokenKind::Literal(LiteralKind::Float), start_byte_index..end_byte_index)
				} else {
					self.tok(TokenKind::Literal(LiteralKind::Int), start_byte_index..end_byte_index)
				}
			},
			ch if ch.is_whitespace() => self.tok(TokenKind::Whitespace, c.0..c.0 + 1),
			'_' | 'A'..='Z' | 'a'..='z' => {
				let start_byte_index = c.0;
				let mut end_byte_index = c.0 + 1;
				while let Some(c) = self
					.source
					.next_if(|c| matches!(c.1, '_' | 'A'..='Z' | 'a'..='z' | '0'..='9'))
				{
					end_byte_index = c.0 + 1;
				}

				let ident = &self.source_raw[start_byte_index..end_byte_index];
				match KEYWORDS.get(ident) {
					Some(token) => self.tok(*token, start_byte_index..end_byte_index),
					None => match ident {
						"true" | "false" => {
							self.tok(TokenKind::Literal(LiteralKind::Bool), start_byte_index..end_byte_index)
						},
						_ => self.tok(TokenKind::Ident, start_byte_index..end_byte_index),
					},
				}
			},
			_ => self.tok(TokenKind::Unknown, c.0..c.0 + 1),
		})
	}
}

impl FusedIterator for &mut Lexer<'_> {}

fn insert_keyword(m: &mut HashMap<&'static str, TokenKind>, k: &'static str, keyword: Keyword) {
	m.insert(k, TokenKind::Keyword(keyword));
}

lazy_static! {
	static ref KEYWORDS: HashMap<&'static str, TokenKind> = {
		let mut m = HashMap::new();
		m.insert("none", TokenKind::None);
		m.insert("and", TokenKind::And);
		m.insert("or", TokenKind::Or);
		m.insert("if", TokenKind::If);
		m.insert("in", TokenKind::In);
		m.insert("else", TokenKind::Else);
		m.insert("while", TokenKind::While);
		m.insert("for", TokenKind::For);
		m.insert("as", TokenKind::As);
		m.insert("is", TokenKind::Is);
		m.insert("return", TokenKind::Return);
		m.insert("break", TokenKind::Break);
		m.insert("continue", TokenKind::Continue);
		m.insert("switch", TokenKind::Switch);
		m.insert("ref", TokenKind::Ref);

		insert_keyword(&mut m, "struct", Keyword::Struct);
		insert_keyword(&mut m, "enum", Keyword::Enum);
		insert_keyword(&mut m, "fn", Keyword::Function);
		insert_keyword(&mut m, "lods", Keyword::Lods);
		insert_keyword(&mut m, "behavior", Keyword::Behavior);
		insert_keyword(&mut m, "template", Keyword::Template);
		insert_keyword(&mut m, "use", Keyword::Use);
		insert_keyword(&mut m, "import", Keyword::Import);
		insert_keyword(&mut m, "mod", Keyword::Mod);
		insert_keyword(&mut m, "let", Keyword::Let);
		insert_keyword(&mut m, "impl", Keyword::Impl);
		m
	};
}

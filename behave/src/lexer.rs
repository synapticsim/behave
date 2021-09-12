use std::{
	collections::HashMap,
	iter::{FusedIterator, Peekable},
	str::CharIndices,
};

use lazy_static::lazy_static;

use crate::{
	diagnostic::{Diagnostic, Label, Level},
	token::{Token, TokenType},
};

pub struct Lexer<'a> {
	file: &'a str,
	source: Peekable<CharIndices<'a>>,
	source_raw: &'a str,
}

impl<'a> Lexer<'a> {
	pub fn new(file: &'a str, source: &'a str) -> Self {
		Self {
			file,
			source: source.char_indices().peekable(),
			source_raw: source,
		}
	}
}

impl Lexer<'_> {}

impl Iterator for Lexer<'_> {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		let c = self.source.next()?;

		Some(match c.1 {
			'@' => Token(TokenType::At, c.0..c.0 + 1),
			'?' => Token(TokenType::QuestionMark, c.0..c.0 + 1),
			'(' => Token(TokenType::LeftParen, c.0..c.0 + 1),
			')' => Token(TokenType::RightParen, c.0..c.0 + 1),
			'{' => Token(TokenType::LeftBrace, c.0..c.0 + 1),
			'}' => Token(TokenType::RightBrace, c.0..c.0 + 1),
			'[' => Token(TokenType::LeftBracket, c.0..c.0 + 1),
			']' => Token(TokenType::RightBracket, c.0..c.0 + 1),
			',' => Token(TokenType::Comma, c.0..c.0 + 1),
			'.' => Token(TokenType::Period, c.0..c.0 + 1),
			':' => Token(TokenType::Colon, c.0..c.0 + 1),
			';' => Token(TokenType::Semicolon, c.0..c.0 + 1),
			'+' => {
				if self.source.peek().map(|c| c.1 == '=').unwrap_or(false) {
					self.source.next();
					Token(TokenType::PlusEqual, c.0..c.0 + 2)
				} else {
					Token(TokenType::Plus, c.0..c.0 + 1)
				}
			},
			'-' => {
				if self.source.peek().map(|c| c.1 == '=').unwrap_or(false) {
					self.source.next();
					Token(TokenType::MinusEqual, c.0..c.0 + 2)
				} else if self.source.peek().map(|c| c.1 == '>').unwrap_or(false) {
					self.source.next();
					Token(TokenType::Arrow, c.0..c.0 + 2)
				} else {
					Token(TokenType::Minus, c.0..c.0 + 1)
				}
			},
			'*' => {
				if self.source.peek().map(|c| c.1 == '=').unwrap_or(false) {
					self.source.next();
					Token(TokenType::StarEqual, c.0..c.0 + 2)
				} else {
					Token(TokenType::Star, c.0..c.0 + 1)
				}
			},
			'%' => {
				if self.source.peek().map(|c| c.1 == '=').unwrap_or(false) {
					self.source.next();
					Token(TokenType::PercentEqual, c.0..c.0 + 2)
				} else {
					Token(TokenType::Percent, c.0..c.0 + 1)
				}
			},
			'=' => {
				if self.source.peek().map(|c| c.1 == '=').unwrap_or(false) {
					self.source.next();
					Token(TokenType::DoubleEqual, c.0..c.0 + 2)
				} else {
					Token(TokenType::Equal, c.0..c.0 + 1)
				}
			},
			'!' => {
				if self.source.peek().map(|c| c.1 == '=').unwrap_or(false) {
					self.source.next();
					Token(TokenType::BangEqual, c.0..c.0 + 2)
				} else {
					Token(TokenType::Bang, c.0..c.0 + 1)
				}
			},
			'>' => {
				if self.source.peek().map(|c| c.1 == '=').unwrap_or(false) {
					self.source.next();
					Token(TokenType::RightChevronEqual, c.0..c.0 + 2)
				} else {
					Token(TokenType::RightChevron, c.0..c.0 + 1)
				}
			},
			'<' => {
				if self.source.peek().map(|c| c.1 == '=').unwrap_or(false) {
					self.source.next();
					Token(TokenType::LeftChevronEqual, c.0..c.0 + 2)
				} else {
					Token(TokenType::LeftChevron, c.0..c.0 + 1)
				}
			},
			'/' => {
				if let Some(c) = self.source.peek() {
					match c.1 {
						'/' => {
							let c = *c;
							while {
								// Cursed do-while loop.
								let c = self.source.next();
								c.map(|c| c.1 != '\n').unwrap_or(false)
							} {}
							Token(TokenType::Comment, c.0..c.0)
						},
						'=' => {
							let c = *c;
							self.source.next();
							Token(TokenType::SlashEqual, c.0..c.0 + 2)
						},
						_ => Token(TokenType::Slash, c.0..c.0 + 1),
					}
				} else {
					Token(TokenType::Slash, c.0..c.0 + 1)
				}
			},
			'"' => {
				let start_byte_index = c.0;
				while let Some(c) = self.source.next() {
					if c.1 == '\\' {
						self.source.next();
					} else if c.1 == '\n' {
						return Some(Token(
							TokenType::Diagnostic(
								Diagnostic::new(Level::Error, "string does not terminate")
									.add_label(Label::primary(
										self.file,
										"the string starts here",
										start_byte_index..start_byte_index + 1,
									))
									.add_label(Label::secondary(
										self.file,
										"but there is a newline here",
										c.0..c.0 + 1,
									)),
							),
							c.0..c.0,
						));
					} else if c.1 == '"' {
						return Some(Token(
							TokenType::String(self.source_raw[start_byte_index + 1..c.0].to_string()),
							start_byte_index - 1..c.0 + 1,
						));
					}
				}

				Token(
					TokenType::Diagnostic(Diagnostic::new(Level::Error, "string does not terminate").add_label(
						Label::primary(
							self.file,
							"the string starts here",
							start_byte_index..start_byte_index + 1,
						),
					)),
					c.0..c.0,
				)
			},
			'0'..='9' => {
				let start_byte_index = c.0;
				let mut decimal = false;
				let mut end_byte_index = 0;
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
				match self.source_raw[start_byte_index..end_byte_index].parse() {
					Ok(val) => Token(TokenType::Number(val), start_byte_index..end_byte_index),
					Err(err) => Token(
						TokenType::Diagnostic(
							Diagnostic::new(Level::Error, format!("failed to parse number: {}", err))
								.add_label(Label::primary(self.file, "", start_byte_index..end_byte_index + 1)),
						),
						c.0..c.0,
					),
				}
			},
			ch if ch.is_whitespace() => Token(TokenType::Whitespace, c.0..c.0 + 1),
			'_' | 'A'..='Z' | 'a'..='z' => {
				let start_byte_index = c.0;
				let mut end_byte_index = 0;
				while let Some(c) = self.source.peek() {
					end_byte_index = c.0;
					if !matches!(c.1, '_' | 'A'..='Z' | 'a'..='z' | '0'..='9') {
						break;
					}
					self.source.next();
				}

				let ident = &self.source_raw[start_byte_index..end_byte_index];
				match KEYWORDS.get(ident) {
					Some(token) => Token(token.clone(), start_byte_index..end_byte_index),
					None => match ident {
						"true" => Token(TokenType::Boolean(true), start_byte_index..end_byte_index),
						"false" => Token(TokenType::Boolean(false), start_byte_index..end_byte_index),
						_ => Token(TokenType::Ident(ident.to_string()), start_byte_index..end_byte_index),
					},
				}
			},
			_ => Token(
				TokenType::Diagnostic(
					Diagnostic::new(Level::Error, "unexpected character").add_label(Label::primary(
						self.file,
						"",
						c.0..c.0 + 1,
					)),
				),
				c.0..c.0,
			),
		})
	}
}

impl FusedIterator for &mut Lexer<'_> {}

lazy_static! {
	static ref KEYWORDS: HashMap<&'static str, TokenType> = {
		let mut m = HashMap::new();
		m.insert("and", TokenType::And);
		m.insert("or", TokenType::Or);
		m.insert("not", TokenType::Not);
		m.insert("if", TokenType::If);
		m.insert("else", TokenType::Else);
		m.insert("while", TokenType::While);
		m.insert("for", TokenType::For);
		m.insert("in", TokenType::In);
		m.insert("component", TokenType::Component);
		m.insert("on", TokenType::On);
		m.insert("extern", TokenType::Extern);
		m.insert("import", TokenType::Import);
		m.insert("lods", TokenType::Lods);
		m.insert("behavior", TokenType::Behavior);
		m.insert("template", TokenType::Template);
		m.insert("str", TokenType::Str);
		m.insert("code", TokenType::Code);
		m.insert("num", TokenType::Num);
		m.insert("bool", TokenType::Bool);
		m.insert("struct", TokenType::Struct);
		m.insert("enum", TokenType::Enum);
		m.insert("fn", TokenType::Function);
		m.insert("animation", TokenType::Animation);
		m.insert("use", TokenType::Use);
		m.insert("return", TokenType::Return);
		m.insert("break", TokenType::Break);
		m.insert("switch", TokenType::Switch);
		m.insert("let", TokenType::Let);
		m.insert("alias", TokenType::Alias);
		m.insert("none", TokenType::None);
		m
	};
}

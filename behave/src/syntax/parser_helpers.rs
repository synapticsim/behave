use crate::{
	diagnostic::{Diagnostic, Label, Level},
	syntax::{
		ast::{Ident, Lit, LitKind, Loc, Path},
		parser::Parser,
		token::{Delimiter, LiteralKind, Span, Token, TokenKind},
	},
	SourceFile,
};

macro_rules! peek {
	($self:expr, $($pattern:pat_param)|+ $(if $guard:expr)? $(,)?) => {
		$self.next_if(|tok| match tok.kind {
			$( $pattern )|+ $( if $guard )? => true,
			_ => false
		})
	};
}

macro_rules! next {
	($self:expr) => {
		match $self.next() {
			Some(tok) => Some(tok),
			None => {
				$self.diagnostics.add(Diagnostic::new(
					Level::Error,
					format!("unexpected <eof> in file '{}'", $self.file),
				));
				None
			},
		}
	};
}

macro_rules! expect {
	($self:expr, $($pattern:pat_param)|+ $(if $guard:expr)?, err = $error:expr $(,)?) => {{
		match $self.peek() {
			Some(tok) => match tok.kind {
				$( $pattern )|+ $( if $guard )? => {
					$self.next();
					Some(tok)
				},
				kind => {
					$self.diagnostics.add(
						Diagnostic::new(Level::Error, $error)
						.add_label(Label::primary(format!("found `{}`", kind), $self.loc(tok.span.clone()).into())),
					);
					Some(tok)
				}
			},
			None => {
				$self.diagnostics.add(Diagnostic::new(
					Level::Error,
					format!("unexpected <eof> in file '{}'", $self.file),
				));
				None
			},
		}
	}};
}

impl<'a, 'b, F> Parser<'a, 'b, F>
where
	F: FnMut(&[&str]) -> Option<&'a SourceFile>,
{
	pub fn delim_list<P, O>(
		&mut self, separator: TokenKind, delim: Delimiter, mut val_parser: P,
	) -> Option<(Vec<O>, Loc<'a>)>
	where
		P: FnMut(&mut Self) -> Option<(O, Loc<'a>)>,
	{
		let mut items = Vec::new();
		let span = expect!(self, TokenKind::OpenDelim(d) if d == delim, err = format!("expected `{}`", TokenKind::OpenDelim(delim)))?.span;
		let mut loc = self.loc(span);

		let insert = |p: &mut Self, f: &mut P, items: &mut Vec<O>, loc: &mut Loc<'a>| {
			f(p).map(|val| {
				items.push(val.0);
				*loc += val.1;
			})
		};

		loop {
			if let Some(tok) = peek!(self, TokenKind::CloseDelim(d) if d == delim) {
				loc += tok.span;
				break;
			} else if peek!(self, x if x == separator).is_some() {
				if let Some(tok) = peek!(self, TokenKind::CloseDelim(d) if d == delim) {
					loc += tok.span;
					break;
				} else {
					insert(self, &mut val_parser, &mut items, &mut loc);
				}
			} else {
				insert(self, &mut val_parser, &mut items, &mut loc);
			}
		}

		Some((items, loc))
	}

	pub fn list<P, O>(&mut self, separator: TokenKind, mut val_parser: P) -> Option<(Vec<O>, Loc<'a>)>
	where
		P: FnMut(&mut Self) -> Option<(O, Loc<'a>)>,
	{
		let mut items = Vec::new();
		let mut span = Loc {
			span: Default::default(),
			file: self.file,
		};

		while {
			let val = val_parser(self)?;
			items.push(val.0);
			span += val.1;

			peek!(self, x if x == separator).is_some()
		} {}

		Some((items, span))
	}

	pub fn ident(&self, tok: Token<'a>) -> Ident<'a> {
		assert_eq!(tok.kind, TokenKind::Ident);
		Ident {
			node: tok.data,
			loc: self.loc(tok.span),
		}
	}

	pub fn literal(&mut self) -> Lit<'a> {
		let tok = if let Some(tok) = next!(self) {
			tok
		} else {
			return Lit {
				node: LitKind::Err(""),
				loc: self.loc(Default::default()),
			};
		};
		let loc = self.loc(tok.span);
		match tok.kind {
			TokenKind::Literal(kind) => match kind {
				LiteralKind::String { terminated } => Lit {
					node: LitKind::Str(if terminated {
						&tok.data[1..tok.data.len() - 1]
					} else {
						&tok.data[1..]
					}),
					loc,
				},
				LiteralKind::Int => match tok.data.parse::<i64>() {
					Ok(data) => Lit {
						node: LitKind::Int(data),
						loc,
					},
					Err(_) => {
						self.diagnostics.add(
							Diagnostic::new(Level::Error, "integer is too large")
								.add_label(Label::primary("", loc.into())),
						);
						Lit {
							node: LitKind::Err(tok.data),
							loc,
						}
					},
				},
				LiteralKind::Float => match tok.data.parse::<f64>() {
					Ok(data) => Lit {
						node: LitKind::Float(data),
						loc,
					},
					Err(_) => unreachable!("failed to parse float literal: how is this possible?"),
				},
				LiteralKind::Bool => Lit {
					node: LitKind::Bool(tok.data == "true"),
					loc,
				},
			},
			kind => {
				self.diagnostics.add(
					Diagnostic::new(Level::Error, "expected literal")
						.add_label(Label::primary(format!("found `{}`", kind), loc.into())),
				);
				Lit {
					node: LitKind::Err(tok.data),
					loc,
				}
			},
		}
	}

	pub fn path(&mut self) -> Option<Path<'a>> {
		self.list(TokenKind::Separator, |p| {
			let ident = expect!(p, TokenKind::Ident, err = "expected `ident` in path")?;
			let loc = p.loc(ident.span);
			Some((Ident { node: ident.data, loc }, loc))
		})
		.map(|path| Path {
			node: path.0,
			loc: path.1,
		})
	}

	pub fn next_if(&mut self, func: impl FnOnce(&Token<'a>) -> bool) -> Option<Token<'a>> {
		match self.next() {
			Some(tok) if func(&tok) => Some(tok),
			other => {
				self.peeked = Some(other);
				None
			},
		}
	}

	pub fn peek(&mut self) -> Option<Token<'a>> {
		self.peeked
			.get_or_insert_with(|| {
				while let Some(tok) = self.lexer.next() {
					match tok.kind {
						TokenKind::Whitespace | TokenKind::Comment => {},
						TokenKind::Unknown => self.diagnostics.add(
							Diagnostic::new(Level::Error, "unknown token").add_label(Label::primary(
								"",
								Loc {
									span: tok.span,
									file: self.file,
								}
								.into(),
							)),
						),
						_ => return Some(tok),
					}
				}

				None
			})
			.clone()
	}

	pub fn next(&mut self) -> Option<Token<'a>> {
		match self.peeked.take() {
			Some(tok) => tok,
			None => {
				while let Some(tok) = self.lexer.next() {
					match tok.kind {
						TokenKind::Whitespace | TokenKind::Comment => {},
						TokenKind::Unknown => self.diagnostics.add(
							Diagnostic::new(Level::Error, "unknown token")
								.add_label(Label::primary("", self.loc(tok.span).into())),
						),
						_ => return Some(tok),
					}
				}

				None
			},
		}
	}

	pub fn loc(&self, span: Span) -> Loc<'a> { Loc { span, file: self.file } }
}

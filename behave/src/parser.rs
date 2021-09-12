use std::{iter::Peekable, ops::Range};

use crate::{
	ast::{
		ASTType,
		Behaviors,
		BinaryOperator,
		Enum,
		EnumVariant,
		Expression,
		ExpressionType,
		FunctionType,
		Ident,
		Import,
		ImportType,
		Item,
		ItemType,
		LODs,
		Path,
		Struct,
		Type,
		TypeType,
		UnaryOperator,
		VarEntry,
		AST,
		LOD,
	},
	diagnostic::{Diagnostic, Label, Level},
	lexer::Lexer,
	token::{Token, TokenType},
};

pub enum ParserMode {
	MainFile,
	ImportedFile,
}

pub struct Parser<'a> {
	mode: ParserMode,
	file: &'a str,
	lexer: Peekable<Lexer<'a>>,
	diagnostics: &'a mut Vec<Diagnostic>,
}

fn no_range() -> Range<usize> { usize::MAX..usize::MIN }

macro_rules! merge_range {
	($r1:expr, $r2:expr) => {
        {
            let r1 = $r1;
            let r2 = $r2;
            r1.start.min(r2.start)..r1.end.max(r2.end)
        }
	};

	($r1:expr, $r2:expr, $($r3:expr),*) => {
        {
            let r1 = $r1;
            let r2 = $r2;
            merge_range!(r1.start.min(r2.start)..r1.end.max(r2.end), $($r3),*)
        }
	};
}

macro_rules! next {
	($self:expr) => {
		match $self.next() {
			Some(tok) => tok,
			None => {
				return Err(Diagnostic::new(Level::Error, "unexpected end of file")
					.add_note("the file ended when it wasn't supposed to"))
			},
		}
	};

	($self:expr,else $if_not:expr) => {
		match $self.next() {
			Some(tok) => tok,
			None => $if_not,
		}
	};
}

macro_rules! peek {
	($self:expr,else $if_not:expr) => {
		match $self.peek() {
			Some(tok) => tok,
			None => $if_not,
		}
	};

	($self:expr, $(|)? $($pattern:pat_param)|+ $(if $guard:expr)?,if $if:block) => {
		{
			match $self.peek() {
					Some(tok) => if match tok.0 {
                	$( $pattern )|+ $( if $guard )? => true,
                	_ => false
            	} {
					$self.next();
					$if
				},
				None => {},
			}
		}
	};

	($self:expr, $(|)? $($pattern:pat_param)|+ $(if $guard:expr)?,else $if_not:expr) => {
		{
			match $self.peek() {
				Some(tok) => if !match tok.0 {
                	$( $pattern )|+ $( if $guard )? => true,
                	_ => false
            	} {
					$if_not
				} else {
					$self.next();
				},
				None => $if_not,
			}
		}
	};

	($self:expr, $(|)? $($pattern:pat_param)|+ $(if $guard:expr)?,if $if:block else $else:block) => {
		{
			match $self.peek() {
				Some(tok) => if match tok.0 {
                	$( $pattern )|+ $( if $guard )? => true,
                	_ => false
            	} {
					$self.next();
					$if
				} else {
					$else
				},
				None => $else,
			}
		}
	};
}

macro_rules! resync {
	($self:expr, $expression:expr, until $(|)? $($pattern:pat_param)|+ $(if $guard:expr)?,else $on_resync:expr $(,)?) => {
        match $expression {
            Ok(val) => val,
            Err(diag) => {
                $self.diagnostics.push(diag);

                while !match next!($self, else $on_resync).0 {
                    $( $pattern )|+ $( if $guard )? => true,
                    _ => false
                } {}

                $on_resync
            }
        }
    };

    ($self:expr, $expression:expr, until $(|)? $($pattern:pat_param)|+ $(if $guard:expr)?, else $on_resync:expr, $note:expr $(,)?) => {
        match $expression {
            Ok(val) => val,
            Err(diag) => {
                $self.diagnostics.push(diag.add_note($note));

                while !match next!($self, else $on_resync).0 {
                    $( $pattern )|+ $( if $guard )? => true,
                    _ => false
                } {}

                $on_resync
            }
        }
    };
}

macro_rules! expect {
    ($self:expr, $(|)? $($pattern:pat_param)|+ $(if $guard:expr)?, $message:expr) => {
        {
            let tok = next!($self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file").add_note("the file ended when it was not supposed to")));
            match tok.0 {
                $( $pattern )|+ $( if $guard )? => tok.1,
                _ => return Err(Diagnostic::new(Level::Error, $message).add_label(Label::unexpected($self.file, &tok))),
            }
        }
    };
}
macro_rules! expect_outer {
    ($self:expr, $(|)? $($pattern:pat_param)|+ $(if $guard:expr)?, $message:expr, else $if_no:expr) => {
        {
            let tok = next!($self, else {
                $self.diagnostics.push(Diagnostic::new(Level::Error, "unexpected end of file").add_note("the file ended when it was not supposed to"));
                $if_no
            });
            match tok.0 {
                $( $pattern )|+ $( if $guard )? => tok.1,
                _ => {
                $self.diagnostics.push(Diagnostic::new(Level::Error, $message).add_label(Label::unexpected($self.file, &tok)));
                $if_no
            },
        }
    }
    };
}

macro_rules! parse_literal {
	($name:ident, $expect:ident, $message:expr) => {
		fn $name(&mut self) -> Result<Expression, Diagnostic> {
			let tok = next!(self);
			if let TokenType::$expect(val) = tok.0 {
				Ok(Expression(ExpressionType::$expect(val), tok.1))
			} else {
				Err(Diagnostic::new(Level::Error, $message).add_label(Label::unexpected(self.file, &tok)))
			}
		}
	};
}

impl<'a> Parser<'a> {
	parse_literal!(parse_string, String, "expected string literal");

	parse_literal!(parse_number, Number, "expected number literal");

	parse_literal!(parse_bool, Boolean, "expected boolean literal");

	pub fn new(mode: ParserMode, file: &'a str, lexer: Lexer<'a>, diagnostics: &'a mut Vec<Diagnostic>) -> Self {
		Self {
			mode,
			file,
			lexer: lexer.peekable(),
			diagnostics,
		}
	}
}

impl Parser<'_> {
	pub fn parse(mut self) -> Option<AST> {
		let mut error = false;
		let mut ast = AST {
			imports: Vec::new(),
			ast_data: match self.mode {
				ParserMode::MainFile => ASTType::Main(LODs(Vec::new(), no_range()), Behaviors(no_range())),
				ParserMode::ImportedFile => ASTType::Secondary(Vec::new()),
			},
		};

		'w: while let Some(token) = self.next() {
			match token.0 {
				TokenType::Import => {
					let p = resync!(self, self.parse_path(), until TokenType::Semicolon, else {
						error = true;
						continue 'w;
					});
					expect_outer!(self, TokenType::Semicolon, "expected semicolon", else {
						error = true;
						continue 'w;
					});
					let r = p.1.clone();
					ast.imports
						.push(Import(ImportType::Normal(p), merge_range!(token.1, r)));
				},
				TokenType::Extern => {
					let p = resync!(
						self,
						self.parse_string(),
						until TokenType::Semicolon,
						else continue 'w,
						"expected a path to an external XML file",
					);
					expect_outer!(self, TokenType::Semicolon, "expected semicolon", else {
						error = true;
						continue 'w;
					});
					let r = p.1.clone();
					ast.imports
						.push(Import(ImportType::Extern(p), merge_range!(token.1, r)));
				},
				TokenType::Lods => match ast.ast_data {
					ASTType::Main(ref mut lods, _) => {
						*lods = resync!(self, self.parse_lods(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						lods.1 = merge_range!(token.1, &lods.1);
					},
					ASTType::Secondary(..) => {
						error = true;
						let lods = resync!(self, self.parse_lods(), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "LOD description is only allowed in the main file")
								.add_label(Label::primary(
									self.file,
									"move this to the main `.beh` file",
									merge_range!(token.1, lods.1),
								)),
						);
					},
				},
				TokenType::Behavior => match ast.ast_data {
					ASTType::Main(_, ref mut behaviors) => {
						*behaviors = resync!(self, self.parse_behaviors(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						behaviors.0 = merge_range!(token.1, &behaviors.0);
					},
					ASTType::Secondary(..) => {
						error = true;
						let behaviors =
							resync!(self, self.parse_behaviors(), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Behavior description is only allowed in the main file")
								.add_label(Label::primary(
									self.file,
									"move this to the main `.beh` file",
									merge_range!(token.1, behaviors.0),
								)),
						);
					},
				},
				TokenType::Enum => match ast.ast_data {
					ASTType::Main(..) => {
						error = true;
						let item = resync!(self, self.parse_enum(), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Enum declaration is only allowed in imported files")
								.add_label(Label::primary(
									self.file,
									"move this to an imported `.beh` file",
									merge_range!(token.1, item.1),
								)),
						);
					},
					ASTType::Secondary(ref mut items) => {
						let mut item = resync!(self, self.parse_enum(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						item.1 = merge_range!(token.1, item.1);
						items.push(item);
					},
				},
				TokenType::Struct => match ast.ast_data {
					ASTType::Main(..) => {
						error = true;
						let item = resync!(self, self.parse_struct(), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Struct declaration is only allowed in imported files")
								.add_label(Label::primary(
									self.file,
									"move this to an imported `.beh` file",
									merge_range!(token.1, item.1),
								)),
						);
					},
					ASTType::Secondary(ref mut items) => {
						let mut item = resync!(self, self.parse_struct(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						item.1 = merge_range!(token.1, item.1);
						items.push(item);
					},
				},
				TokenType::Let => match ast.ast_data {
					ASTType::Main(..) => {
						error = true;
						let item = resync!(self, self.parse_variable(ExpressionParseMode::Normal), until TokenType::Semicolon, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Variable declaration is only allowed in imported files")
								.add_label(Label::primary(
									self.file,
									"move this to an imported `.beh` file",
									merge_range!(token.1, item.1),
								)),
						)
					},
					ASTType::Secondary(ref mut items) => {
						let mut item = resync!(self, self.parse_variable(ExpressionParseMode::Normal), until TokenType::Semicolon, else {
							error = true;
							continue 'w;
						});
						item.1 = merge_range!(token.1, item.1);
						items.push(item);
					},
				},
				_ => {
					self.diagnostics.push(
						Diagnostic::new(
							Level::Error,
							match self.mode {
								ParserMode::MainFile => "expected `import`, `lods` or `behavior`",
								ParserMode::ImportedFile => "expected `import` or `item`",
							},
						)
						.add_label(Label::unexpected(self.file, &token)),
					);
					return None;
				},
			}
		}

		match &ast.ast_data {
			ASTType::Main(lods, _) => {
				if lods.0.is_empty() {
					self.diagnostics.push(
						Diagnostic::new(Level::Error, "LODs are empty")
							.add_note("add a LOD definition list (`lods` {})"),
					);
					return None;
				}
			},
			_ => {},
		}

		if error {
			None
		} else {
			Some(ast)
		}
	}

	fn parse_lods(&mut self) -> Result<LODs, Diagnostic> {
		let mut lods = LODs(Vec::new(), no_range());
		lods.1 = expect!(self, TokenType::LeftBrace, "expected `{`");

		loop {
			let min_size = self.parse_expression(ExpressionParseMode::Normal)?;
			expect!(self, TokenType::Arrow, "expected `->`");
			let file = self.parse_expression(ExpressionParseMode::Normal)?;
			let (r1, r2) = (min_size.1.clone(), file.1.clone());
			lods.0.push(LOD {
				min_size,
				file,
				range: merge_range!(r1, r2),
			});

			peek!(self, TokenType::Comma, else break);
		}

		lods.1 = merge_range!(lods.1, expect!(self, TokenType::RightBrace, "expected `}`"));
		Ok(lods)
	}

	fn parse_behaviors(&mut self) -> Result<Behaviors, Diagnostic> {
		let mut behaviors = Behaviors(no_range());
		behaviors.0 = merge_range!(
			expect!(self, TokenType::LeftBrace, "expected `{`"),
			expect!(self, TokenType::RightBrace, "expected `}`")
		);
		Ok(behaviors)
	}

	fn parse_enum(&mut self) -> Result<Item, Diagnostic> {
		let mut item = Enum {
			name: self.parse_ident()?,
			variants: Vec::new(),
		};
		let mut range = merge_range!(item.name.1.clone(), expect!(self, TokenType::LeftBrace, "expected `{`"));
		loop {
			let ident = self.parse_ident()?;
			let mut range = ident.1.clone();
			let value = peek!(self, TokenType::Equal, if {
				let num = self.parse_number()?;
				range = merge_range!(range, num.1.clone());
				Some(num)
			} else {
				None
			});

			item.variants.push(EnumVariant {
				name: ident,
				value,
				range,
			});

			peek!(self, TokenType::Comma, else break);
		}
		range = merge_range!(range, expect!(self, TokenType::RightBrace, "expected `}`"));

		Ok(Item(ItemType::Enum(item), range))
	}

	fn parse_struct(&mut self) -> Result<Item, Diagnostic> {
		let name = self.parse_ident()?;
		let mut fields = Vec::new();
		let mut range = name.1.clone();
		expect!(self, TokenType::LeftBrace, "expected `{`");
		if let Some(tok) = self.peek() {
			if let TokenType::RightBrace = tok.0 {
				range = tok.1;
			} else {
				loop {
					let field = self.parse_var_entry()?;
					range = merge_range!(range, &field.range);
					fields.push(field);
					peek!(self, TokenType::Comma, else break);
				}
			}
		} else {
			return Err(Diagnostic::new(
				Level::Error,
				"unexpected end of file: expected type or `)`",
			));
		}

		Ok(Item(
			ItemType::Struct(Struct { name, fields }),
			merge_range!(range, expect!(self, TokenType::RightBrace, "expected `}`")),
		))
	}

	fn parse_variable(&mut self, mode: ExpressionParseMode) -> Result<Item, Diagnostic> {
		let name = self.parse_ident()?;
		let range = name.1.clone();
		let ty = peek!(self, TokenType::Colon, if {
			Some(self.parse_type()?)
		} else {
			None
		});
		let init = peek!(self, TokenType::Equal, if {
			Some(self.parse_expression(mode)?)
		} else {
			None
		});
		Ok(Item(
			ItemType::Variable(name, ty, init),
			merge_range!(range, expect!(self, TokenType::Semicolon, "expected `semicolon`")),
		))
	}

	fn parse_var_entry(&mut self) -> Result<VarEntry, Diagnostic> {
		let name = self.parse_ident()?;
		expect!(self, TokenType::Colon, "expected `colon`");
		let ty = self.parse_type()?;

		Ok(VarEntry {
			range: merge_range!(&name.1, &ty.1),
			name,
			ty,
		})
	}

	fn parse_type(&mut self) -> Result<Type, Diagnostic> {
		let tok = next!(self);
		let ty = match tok.0 {
			TokenType::Num => Type(TypeType::Num, tok.1),
			TokenType::Str => Type(TypeType::Str, tok.1),
			TokenType::Bool => Type(TypeType::Bool, tok.1),
			TokenType::Code => Type(TypeType::Code, tok.1),
			TokenType::Ident(s) => Type(TypeType::User(Ident(s, tok.1.clone())), tok.1),
			TokenType::LeftBracket => Type(
				TypeType::Array(Box::new(self.parse_type()?)),
				merge_range!(tok.1, expect!(self, TokenType::RightBracket, "expected `]`")),
			),
			TokenType::Function => {
				expect!(self, TokenType::LeftParen, "expected `(`");
				let args = self.parse_tuple_type()?;
				let mut range = merge_range!(tok.1, args.1);
				let mut ty = FunctionType {
					args: args.0,
					ret: None,
				};
				peek!(self, TokenType::Arrow, if {
					let ret = self.parse_type()?;
					range = merge_range!(range, &ret.1);
					ty.ret = Some(Box::new(ret))
				});
				Type(TypeType::Function(ty), range)
			},
			TokenType::LeftParen => {
				let ty = self.parse_tuple_type()?;
				Type(TypeType::Tuple(ty.0), merge_range!(tok.1, ty.1))
			},
			_ => {
				return Err(Diagnostic::new(Level::Error, "expected type").add_label(Label::unexpected(self.file, &tok)))
			},
		};
		Ok(match self.peek() {
			Some(tok) => {
				if match tok.0 {
					TokenType::QuestionMark => true,
					_ => false,
				} {
					self.next();
					let r = ty.1.clone();
					Type(TypeType::Optional(Box::new(ty)), merge_range!(r, tok.1))
				} else {
					ty
				}
			},
			None => ty,
		})
	}

	fn parse_tuple_type(&mut self) -> Result<(Vec<Type>, Range<usize>), Diagnostic> {
		let mut tuple = Vec::new();
		let mut range = no_range();
		if let Some(tok) = self.peek() {
			if let TokenType::RightParen = tok.0 {
				range = tok.1;
			} else {
				loop {
					let ty = self.parse_type()?;
					range = merge_range!(range, &ty.1);
					tuple.push(ty);
					peek!(self, TokenType::Comma, else break);
				}
			}
		} else {
			return Err(Diagnostic::new(
				Level::Error,
				"unexpected end of file: expected type or `)`",
			));
		}
		Ok((
			tuple,
			merge_range!(range, expect!(self, TokenType::RightParen, "expected `)`")),
		))
	}

	fn parse_expression(&mut self, mode: ExpressionParseMode) -> Result<Expression, Diagnostic> {
		self.parse_with_precedence(precedence::ASSIGNMENT, mode)
	}

	fn parse_tuple(&mut self, mode: ExpressionParseMode) -> Result<Expression, Diagnostic> {
		let mut range = self.next().unwrap().1;
		let mut tuple = Vec::new();
		if let Some(tok) = self.peek() {
			if let TokenType::RightParen = tok.0 {
				range = merge_range!(range, tok.1);
			} else {
				loop {
					let expr = self.parse_expression(mode)?;
					range = merge_range!(range, &expr.1);
					tuple.push(expr);
					peek!(self, TokenType::Comma, else break);
				}
			}
		} else {
			return Err(Diagnostic::new(
				Level::Error,
				"unexpected end of file: expected expression or `)`",
			));
		}
		Ok(Expression(
			ExpressionType::Tuple(tuple),
			merge_range!(range, expect!(self, TokenType::RightParen, "expected `)`")),
		))
	}

	fn parse_array(&mut self, mode: ExpressionParseMode) -> Result<Expression, Diagnostic> {
		let mut range = self.next().unwrap().1;
		let mut array = Vec::new();
		if let Some(tok) = self.peek() {
			if let TokenType::RightParen = tok.0 {
				range = merge_range!(range, tok.1);
			} else {
				loop {
					let expr = self.parse_expression(mode)?;
					range = merge_range!(range, &expr.1);
					array.push(expr);
					peek!(self, TokenType::Comma, else break);
				}
			}
		} else {
			return Err(Diagnostic::new(
				Level::Error,
				"unexpected end of file: expected expression or `]`",
			));
		}
		Ok(Expression(
			ExpressionType::Array(array),
			merge_range!(range, expect!(self, TokenType::RightBracket, "expected `]`")),
		))
	}

	fn parse_unary(&mut self, op: UnaryOperator, mode: ExpressionParseMode) -> Result<Expression, Diagnostic> {
		self.next();
		let expr = self.parse_with_precedence(precedence::UNARY, mode)?;
		let r = expr.1.clone();
		Ok(Expression(ExpressionType::Unary(op, Box::new(expr)), r))
	}

	fn parse_binary(
		&mut self, op: BinaryOperator, precedence: usize, left: Expression, mode: ExpressionParseMode,
	) -> Result<Expression, Diagnostic> {
		let right = self.parse_with_precedence(precedence + 1, mode)?;
		let range = merge_range!(&left.1, &right.1);
		Ok(Expression(
			ExpressionType::Binary(Box::new(left), op, Box::new(right)),
			range,
		))
	}

	fn parse_with_precedence(
		&mut self, precedence: usize, mode: ExpressionParseMode,
	) -> Result<Expression, Diagnostic> {
		let tok = peek!(self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file")));
		let prefix = if let Some(rule) = get_parse_rule(&tok.0) {
			if let Some(prefix) = rule.prefix {
				prefix
			} else {
				return Err(
					Diagnostic::new(Level::Error, "expected expression").add_label(Label::unexpected(self.file, &tok))
				);
			}
		} else {
			return Err(
				Diagnostic::new(Level::Error, "expected expression").add_label(Label::unexpected(self.file, &tok))
			);
		};

		let mut expr = prefix(self, mode)?;
		expr.1 = merge_range!(tok.1, expr.1);
		loop {
			let tok = peek!(self, else return Ok(expr));
			let infix = if let Some(rule) = get_parse_rule(&tok.0) {
				if let Some(infix) = rule.infix {
					infix
				} else {
					return Ok(expr);
				}
			} else {
				return Ok(expr);
			};

			if precedence > infix.1 {
				break;
			}

			self.next();
			expr = infix.0(self, expr, mode)?;
		}

		Ok(expr)
	}

	fn parse_path(&mut self) -> Result<Path, Diagnostic> {
		let mut p = Path(vec![], usize::MAX..usize::MIN);
		loop {
			let ident = self.parse_ident()?;
			p.1 = merge_range!(p.1, &ident.1);
			p.0.push(ident);

			peek!(self, TokenType::Period, else break);
		}

		Ok(p)
	}

	fn parse_ident(&mut self) -> Result<Ident, Diagnostic> {
		let tok = next!(self);
		if let TokenType::Ident(val) = tok.0 {
			Ok(Ident(val, tok.1))
		} else {
			Err(Diagnostic::new(Level::Error, "expected identifier").add_label(Label::unexpected(self.file, &tok)))
		}
	}

	fn next(&mut self) -> Option<Token> {
		while let Some(tok) = self.lexer.next() {
			match tok.0 {
				TokenType::Whitespace | TokenType::Comment => {},
				TokenType::Diagnostic(diag) => self.diagnostics.push(diag),
				_ => return Some(tok),
			}
		}

		None
	}

	fn peek(&mut self) -> Option<Token> {
		while let Some(tok) = self.lexer.peek() {
			match &tok.0 {
				TokenType::Whitespace | TokenType::Comment => {
					self.lexer.next();
				},
				TokenType::Diagnostic(diag) => {
					self.diagnostics.push(diag.clone());
					self.lexer.next();
				},
				_ => return Some(tok.clone()),
			}
		}

		None
	}
}

#[derive(Clone, Copy)]
enum ExpressionParseMode {
	Normal,
	RPNCode,
	Template,
}

fn get_parse_rule(token: &TokenType) -> Option<&'static ParseRule> {
	match token {
		TokenType::Ident(_) => Some(&ParseRule {
			prefix: Some(&|p, _| {
				let path = p.parse_path()?;
				let r = path.1.clone();
				Ok(Expression(ExpressionType::Access(path), r))
			}),
			infix: None,
		}),
		TokenType::None => Some(&ParseRule {
			prefix: Some(&|p, _| {
				p.next();
				Ok(Expression(ExpressionType::None, no_range()))
			}),
			infix: None,
		}),
		TokenType::Number(_) => Some(&ParseRule {
			prefix: Some(&|p, _| p.parse_number()),
			infix: None,
		}),
		TokenType::String(_) => Some(&ParseRule {
			prefix: Some(&|p, _| p.parse_string()),
			infix: None,
		}),
		TokenType::Boolean(_) => Some(&ParseRule {
			prefix: Some(&|p, _| p.parse_bool()),
			infix: None,
		}),
		TokenType::LeftParen => Some(&ParseRule {
			prefix: Some(&|p, mode| p.parse_tuple(mode)),
			infix: None,
		}),
		TokenType::LeftBracket => Some(&ParseRule {
			prefix: Some(&|p, mode| p.parse_array(mode)),
			infix: Some((
				&|p, left, mode| {
					let index = p.parse_expression(mode)?;
					Ok(Expression(
						ExpressionType::Index(Box::new(left), Box::new(index)),
						expect!(p, TokenType::RightBracket, "expected `]`"),
					))
				},
				precedence::CALL,
			)),
		}),
		TokenType::Bang => Some(&ParseRule {
			prefix: Some(&|p, mode| p.parse_unary(UnaryOperator::Not, mode)),
			infix: None,
		}),
		TokenType::Plus => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::Add, precedence::TERM, left, mode),
				precedence::TERM,
			)),
		}),
		TokenType::Minus => Some(&ParseRule {
			prefix: Some(&|p, mode| p.parse_unary(UnaryOperator::Negate, mode)),
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::Subtract, precedence::TERM, left, mode),
				precedence::TERM,
			)),
		}),
		TokenType::Star => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::Multiply, precedence::FACTOR, left, mode),
				precedence::FACTOR,
			)),
		}),
		TokenType::Slash => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::Divide, precedence::FACTOR, left, mode),
				precedence::FACTOR,
			)),
		}),
		TokenType::DoubleEqual => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::Equal, precedence::EQUALITY, left, mode),
				precedence::EQUALITY,
			)),
		}),
		TokenType::BangEqual => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::NotEqual, precedence::EQUALITY, left, mode),
				precedence::EQUALITY,
			)),
		}),
		TokenType::RightChevron => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::Greater, precedence::COMPARISION, left, mode),
				precedence::COMPARISION,
			)),
		}),
		TokenType::LeftChevron => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::Lesser, precedence::COMPARISION, left, mode),
				precedence::COMPARISION,
			)),
		}),
		TokenType::RightChevronEqual => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| {
					p.parse_binary(BinaryOperator::GreaterThanOrEqual, precedence::COMPARISION, left, mode)
				},
				precedence::COMPARISION,
			)),
		}),
		TokenType::LeftChevronEqual => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::LesserThanOrEqual, precedence::COMPARISION, left, mode),
				precedence::COMPARISION,
			)),
		}),
		TokenType::Or => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::Or, precedence::OR, left, mode),
				precedence::OR,
			)),
		}),
		TokenType::And => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::And, precedence::AND, left, mode),
				precedence::AND,
			)),
		}),
		TokenType::Equal => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| {
					if let ExpressionType::Access(path) = left.0 {
						let expr = p.parse_expression(mode)?;
						let range = expr.1.clone();
						Ok(Expression(ExpressionType::Assignment(path, Box::new(expr)), range))
					} else {
						Err(
							Diagnostic::new(Level::Error, "expected assignment target").add_label(Label::primary(
								p.file,
								"cannot assign to `expr`",
								left.1,
							)),
						)
					}
				},
				precedence::CALL,
			)),
		}),
		_ => None,
	}
}

struct ParseRule {
	prefix: Option<&'static dyn Fn(&mut Parser, ExpressionParseMode) -> Result<Expression, Diagnostic>>,
	infix: Option<(
		&'static dyn Fn(&mut Parser, Expression, ExpressionParseMode) -> Result<Expression, Diagnostic>,
		usize,
	)>,
}

mod precedence {
	pub const ASSIGNMENT: usize = 1;
	pub const OR: usize = 2;
	pub const AND: usize = 3;
	pub const EQUALITY: usize = 4;
	pub const COMPARISION: usize = 5;
	pub const TERM: usize = 6;
	pub const FACTOR: usize = 7;
	pub const UNARY: usize = 8;
	pub const CALL: usize = 9;
	pub const PRIMARY: usize = 10;
}

use std::{iter::Peekable, ops::Range};

use crate::ast::{Animation, AssignmentTarget, Component};
use crate::{
	ast::{
		ASTType,
		Assignment,
		Behavior,
		BinaryOperator,
		Block,
		Call,
		Case,
		Enum,
		EnumVariant,
		Expression,
		ExpressionType,
		Function,
		FunctionType,
		Ident,
		IfChain,
		Import,
		ImportType,
		Index,
		Item,
		ItemType,
		LODs,
		Path,
		Statement,
		StatementType,
		Struct,
		Switch,
		Template,
		Type,
		TypeType,
		UnaryOperator,
		Use,
		VarEntry,
		Variable,
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
	file: &'a [String],
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

	pub fn new(mode: ParserMode, file: &'a [String], lexer: Lexer<'a>, diagnostics: &'a mut Vec<Diagnostic>) -> Self {
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
				ParserMode::MainFile => ASTType::Main(LODs(Vec::new(), no_range()), Behavior(Vec::new(), no_range())),
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
						*behaviors = resync!(self, self.parse_behavior(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						behaviors.1 = merge_range!(token.1, &behaviors.1);
					},
					ASTType::Secondary(..) => {
						error = true;
						let behaviors =
							resync!(self, self.parse_behavior(), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Behavior description is only allowed in the main file")
								.add_label(Label::primary(
									self.file,
									"move this to the main `.beh` file",
									merge_range!(token.1, behaviors.1),
								)),
						);
					},
				},
				TokenType::Template => match ast.ast_data {
					ASTType::Main(..) => {
						error = true;
						let template =
							resync!(self, self.parse_template(), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Template definition is only allowed in imported files")
								.add_label(Label::primary(
									self.file,
									"move this to the imported `.beh` file",
									merge_range!(token.1, template.1),
								)),
						);
					},
					ASTType::Secondary(ref mut items) => {
						let mut template = resync!(self, self.parse_template(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						template.1 = merge_range!(token.1, &template.1);
						items.push(Item(ItemType::Template(template.0), template.1));
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
						let var = resync!(self, self.parse_variable(ExpressionParseMode::Normal), until TokenType::Semicolon, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Variable declaration is only allowed in imported files")
								.add_label(Label::primary(
									self.file,
									"move this to an imported `.beh` file",
									merge_range!(token.1, var.1),
								)),
						)
					},
					ASTType::Secondary(ref mut items) => {
						let var = resync!(self, self.parse_variable(ExpressionParseMode::Normal), until TokenType::Semicolon, else {
							error = true;
							continue 'w;
						});
						items.push(Item(ItemType::Variable(var.0), merge_range!(token.1, var.1)));
					},
				},
				TokenType::Function => match ast.ast_data {
					ASTType::Main(..) => {
						error = true;
						let _ = resync!(self, self.parse_ident(), until TokenType::RightBrace, else continue 'w);
						let func = resync!(self, self.parse_function(ExpressionParseMode::Normal), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Function declaration is only allowed in imported files")
								.add_label(Label::primary(
									self.file,
									"move this to an imported `.beh` file",
									merge_range!(token.1, func.1),
								)),
						)
					},
					ASTType::Secondary(ref mut items) => {
						let ident = resync!(self, self.parse_ident(), until TokenType::RightBrace, else {
							error = true;
							continue 'w
						});
						let func = resync!(self, self.parse_function(ExpressionParseMode::Normal), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						items.push(Item(ItemType::Function(ident, func.0), merge_range!(token.1, func.1)));
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

	fn parse_behavior(&mut self) -> Result<Behavior, Diagnostic> {
		let statements = self.parse_template_block()?;
		Ok(Behavior(statements.0, statements.1))
	}

	fn parse_template(&mut self) -> Result<(Template, Range<usize>), Diagnostic> {
		let mut template = Template {
			name: self.parse_ident()?,
			args: Vec::new(),
			block: Vec::new(),
		};
		let mut range = expect!(self, TokenType::LeftParen, "expected `(`");
		let tok = peek!(self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file: expected `)`")));
		if let TokenType::RightParen = tok.0 {
			range = tok.1;
		} else {
			loop {
				let arg = self.parse_var_entry()?;
				range = merge_range!(range, &arg.range);
				template.args.push(arg);
				peek!(self, TokenType::Comma, else break);
			}
		}
		range = merge_range!(range, expect!(self, TokenType::RightParen, "expected `)`"));

		let statements = self.parse_template_block()?;
		template.block = statements.0;
		Ok((template, merge_range!(range, statements.1)))
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
			return Err(Diagnostic::new(Level::Error, "unexpected end of file: expected `}`"));
		}

		Ok(Item(
			ItemType::Struct(Struct { name, fields }),
			merge_range!(range, expect!(self, TokenType::RightBrace, "expected `}`")),
		))
	}

	fn parse_variable(&mut self, mode: ExpressionParseMode) -> Result<(Variable, Range<usize>), Diagnostic> {
		let name = self.parse_ident()?;
		let range = name.1.clone();
		let mut last_range = range.clone();
		let value = peek!(self, TokenType::Equal, if {
			let expr = self.parse_expression(mode)?;
			last_range = expr.1.clone();
			Some(expr)
		} else {
			None
		});
		Ok((
			Variable { name, value },
			merge_range!(&range, {
				let tok = next!(self);
				match tok.0 {
					TokenType::Semicolon => {},
					_ => {
						return Err(
							Diagnostic::new(Level::Error, "expected `semicolon`").add_label(Label::primary(
								self.file,
								"add a semicolon after this",
								last_range,
							)),
						)
					},
				};
				tok.1
			}),
		))
	}

	fn parse_var_entry(&mut self) -> Result<VarEntry, Diagnostic> {
		let name = self.parse_ident()?;
		expect!(self, TokenType::Colon, "expected `colon`");
		let ty = self.parse_type()?;
		let default = peek!(self, TokenType::Equal, if {
			Some(Box::new(self.parse_expression(ExpressionParseMode::Normal)?))
		} else {
			None
		});

		Ok(VarEntry {
			range: merge_range!(&name.1, &ty.1),
			name,
			ty,
			default,
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
				let mut args = Vec::new();
				let mut range = tok.1;
				if let Some(tok) = self.peek() {
					if let TokenType::RightParen = tok.0 {
						range = tok.1;
					} else {
						loop {
							let ty = self.parse_type()?;
							range = merge_range!(range, &ty.1);
							args.push(ty);
							peek!(self, TokenType::Comma, else break);
						}
					}
				} else {
					return Err(Diagnostic::new(
						Level::Error,
						"unexpected end of file: expected type or `)`",
					));
				}
				range = merge_range!(range, expect!(self, TokenType::RightParen, "expected `)`"));
				let mut ty = FunctionType { args, ret: None };
				peek!(self, TokenType::Arrow, if {
					let ret = self.parse_type()?;
					range = merge_range!(range, &ret.1);
					ty.ret = Some(Box::new(ret))
				});
				Type(TypeType::Function(ty), range)
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

	fn parse_template_block(&mut self) -> Result<(Vec<Statement>, Range<usize>), Diagnostic> {
		let mut statements = Vec::new();
		let mut range = expect!(self, TokenType::LeftBrace, "expected `{`");
		let mut tok = peek!(self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file")));
		if let TokenType::RightBrace = tok.0 {
			self.next();
			range = tok.1;
			Ok((statements, range))
		} else {
			loop {
				match tok.0 {
					TokenType::Let => {
						self.next();
						let item = self.parse_variable(ExpressionParseMode::Normal)?;
						range = merge_range!(range, &item.1);
						statements.push(Statement(StatementType::Declaration(item.0), item.1));
					},
					_ => {
						let expr = self.parse_expression(ExpressionParseMode::Template)?;
						range = merge_range!(range, &expr.1);
						statements.push(Statement(StatementType::Expression(expr.0), expr.1))
					},
				}

				let next = if let Some(tok) = self.peek() {
					tok
				} else {
					return Err(Diagnostic::new(Level::Error, "unexpected end of file"));
				};
				match &next.0 {
					TokenType::RightBrace => {
						range = merge_range!(range, next.1);
						self.next();
						break;
					},
					_ => tok = next,
				}
			}

			Ok((statements, range))
		}
	}

	fn parse_template_values(&mut self) -> Result<(Vec<(Ident, Expression)>, Range<usize>), Diagnostic> {
		let range = expect!(self, TokenType::LeftBrace, "expected `{`");
		let mut args = Vec::new();
		loop {
			let arg = self.parse_ident()?;
			expect!(self, TokenType::Colon, "expected `colon`");
			let value = self.parse_expression(ExpressionParseMode::Normal)?;
			args.push((arg, value));

			peek!(self, TokenType::Comma, else break);
		}
		Ok((
			args,
			merge_range!(range, expect!(self, TokenType::RightBrace, "expected `}`")),
		))
	}

	fn parse_expression(&mut self, mode: ExpressionParseMode) -> Result<Expression, Diagnostic> {
		self.parse_with_precedence(precedence::ASSIGNMENT, mode)
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
		let tok = next!(self);
		let expr = self.parse_with_precedence(precedence::UNARY, mode)?;
		let range = merge_range!(tok.1, expr.1.clone());
		if mode != ExpressionParseMode::Template {
			Ok(Expression(ExpressionType::Unary(op, Box::new(expr)), range))
		} else {
			Err(Diagnostic::new(
				Level::Error,
				"a unary expression is not allowed in template expressions",
			)
			.add_label(Label::primary(self.file, "unexpected expression", range)))
		}
	}

	fn parse_binary(
		&mut self, op: BinaryOperator, precedence: usize, left: Expression, mode: ExpressionParseMode,
	) -> Result<Expression, Diagnostic> {
		let right = self.parse_with_precedence(precedence + 1, mode)?;
		let range = merge_range!(&left.1, &right.1);
		if mode != ExpressionParseMode::Template {
			Ok(Expression(
				ExpressionType::Binary(Box::new(left), op, Box::new(right)),
				range,
			))
		} else {
			Err(Diagnostic::new(
				Level::Error,
				"a binary expression is not allowed in template expressions",
			)
			.add_label(Label::primary(self.file, "unexpected expression", range)))
		}
	}

	fn parse_rpn(&mut self, mode: ExpressionParseMode) -> Result<Expression, Diagnostic> {
		let tok = next!(self);
		match mode {
			ExpressionParseMode::Normal => {
				let next = next!(self);
				if let TokenType::LeftBrace = next.0 {
					let block = self.parse_block(ExpressionParseMode::RPNCode)?;
					Ok(Expression(ExpressionType::Code(block.0), merge_range!(tok.1, block.1)))
				} else {
					Err(Diagnostic::new(Level::Error, "expected RPN code block")
						.add_label(Label::unexpected(self.file, &next))
						.add_note("RPN variables can only be read when inside an RPN code block"))
				}
			},
			ExpressionParseMode::RPNCode => {
				let variable = match self.parse_expression(mode) {
					Ok(expr) => expr,
					Err(diag) => return Err(diag.add_note("expected MSFS variable")),
				};
				let range = variable.1.clone();
				Ok(Expression(
					ExpressionType::RPNAccess(Box::new(variable)),
					merge_range!(tok.1, range),
				))
			},
			ExpressionParseMode::Template => Err(Diagnostic::new(
				Level::Error,
				"expected either component or template expression",
			)
			.add_label(Label::primary(self.file, "unexpected RPN expression", tok.1))),
		}
	}

	fn parse_block(&mut self, mode: ExpressionParseMode) -> Result<(Block, Range<usize>), Diagnostic> {
		let mut block = Block {
			statements: Vec::new(),
			expression: None,
		};
		let mut range = no_range();
		let mut tok = peek!(self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file")));
		if let TokenType::RightBrace = tok.0 {
			self.next();
			Ok((block, tok.1))
		} else {
			loop {
				match tok.0 {
					TokenType::Let => {
						self.next();
						let item = self.parse_variable(mode)?;
						range = merge_range!(range, &item.1);
						block
							.statements
							.push(Statement(StatementType::Declaration(item.0), item.1));
					},
					_ => {
						let expr = self.parse_expression(mode)?;
						range = merge_range!(range, &expr.1);
						peek!(self, TokenType::Semicolon, if {
							block.statements.push(Statement(StatementType::Expression(expr.0), expr.1))
						} else {
							if let Some(expr) = block.expression {
								return Err(
									Diagnostic::new(Level::Error, "missing semicolon after statement")
									.add_label(Label::primary(self.file, "add a semicolon after this expression", expr.1))
								);
							} else {
								block.expression = Some(Box::new(expr));
							}
						})
					},
				}

				let next = if let Some(tok) = self.peek() {
					tok
				} else {
					return Err(Diagnostic::new(Level::Error, "unexpected end of file"));
				};
				match &next.0 {
					TokenType::RightBrace => {
						range = merge_range!(range, next.1);
						self.next();
						break;
					},
					_ => tok = next,
				}
			}

			Ok((block, range))
		}
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

	fn parse_function(&mut self, mode: ExpressionParseMode) -> Result<(Function, Range<usize>), Diagnostic> {
		let mut params = Vec::new();
		let mut range = expect!(self, TokenType::LeftParen, "expected `(`");
		let tok = peek!(self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file: expected `)`")));
		if let TokenType::RightParen = tok.0 {
			range = tok.1;
		} else {
			loop {
				let arg = self.parse_var_entry()?;
				range = merge_range!(range, &arg.range);
				params.push(arg);
				peek!(self, TokenType::Comma, else break);
			}
		}
		range = merge_range!(range, expect!(self, TokenType::RightParen, "expected `)`"));
		let ret = peek!(self, TokenType::Arrow, if {
			Some(self.parse_type()?)
		} else {
			None
		});
		range = merge_range!(range, expect!(self, TokenType::LeftBrace, "expected `{`"));

		let block = self.parse_block(mode)?;
		range = merge_range!(range, block.1);
		Ok((
			Function {
				params,
				ret,
				block: block.0,
			},
			range,
		))
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

#[derive(Clone, Copy, PartialEq)]
enum ExpressionParseMode {
	Normal,
	RPNCode,
	Template,
}

fn get_parse_rule(token: &TokenType) -> Option<&'static ParseRule> {
	match token {
		TokenType::At => Some(&ParseRule {
			prefix: Some(&|p, mode| p.parse_rpn(mode)),
			infix: None,
		}),
		TokenType::Ident(_) => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let path = p.parse_path()?;
				if mode != ExpressionParseMode::Template {
					let r = path.1.clone();
					Ok(Expression(ExpressionType::Access(path), r))
				} else {
					Err(
						Diagnostic::new(Level::Error, "variable access is not allowed in template expressions")
							.add_label(Label::primary(p.file, "the variable was accessed here", path.1)),
					)
				}
			}),
			infix: None,
		}),
		TokenType::None => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let tok = next!(p);
				if mode != ExpressionParseMode::Template {
					Ok(Expression(ExpressionType::None, tok.1))
				} else {
					Err(
						Diagnostic::new(Level::Error, "a none expression is not allowed in template expressions")
							.add_label(Label::primary(p.file, "unexpected expression", tok.1)),
					)
				}
			}),
			infix: None,
		}),
		TokenType::Number(_) => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let num = p.parse_number()?;
				if mode != ExpressionParseMode::Template {
					Ok(num)
				} else {
					Err(Diagnostic::new(
						Level::Error,
						"a number expression is not allowed in template expressions",
					)
					.add_label(Label::primary(p.file, "unexpected expression", num.1)))
				}
			}),
			infix: None,
		}),
		TokenType::String(_) => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let string = p.parse_string()?;
				if mode != ExpressionParseMode::Template {
					Ok(string)
				} else {
					Err(Diagnostic::new(
						Level::Error,
						"a string expression is not allowed in template expressions",
					)
					.add_label(Label::primary(p.file, "unexpected expression", string.1)))
				}
			}),
			infix: None,
		}),
		TokenType::Boolean(_) => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let boolean = p.parse_bool()?;
				if mode != ExpressionParseMode::Template {
					Ok(boolean)
				} else {
					Err(Diagnostic::new(
						Level::Error,
						"a boolean expression is not allowed in template expressions",
					)
					.add_label(Label::primary(p.file, "unexpected expression", boolean.1)))
				}
			}),
			infix: None,
		}),
		TokenType::LeftParen => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				p.next();
				let expr = p.parse_expression(mode)?;
				if mode != ExpressionParseMode::Template {
					let range = expr.1.clone();
					Ok(Expression(
						expr.0,
						merge_range!(range, expect!(p, TokenType::RightParen, "expected `)`")),
					))
				} else {
					Err(Diagnostic::new(
						Level::Error,
						"a grouping expression is not allowed in template expressions",
					)
					.add_label(Label::primary(p.file, "unexpected expression", expr.1)))
				}
			}),
			infix: Some((
				&|p, left, mode| {
					let mut args = Vec::new();
					let mut range = no_range();
					if let Some(tok) = p.peek() {
						if let TokenType::RightParen = tok.0 {
							range = tok.1;
						} else {
							loop {
								let expr = p.parse_expression(mode)?;
								range = merge_range!(range, &expr.1);
								args.push(expr);
								peek!(p, TokenType::Comma, else break);
							}
						}
						let l_range = left.1.clone();
						Ok(Expression(
							ExpressionType::Call(Call {
								callee: Box::new(left),
								args,
							}),
							merge_range!(l_range, range, expect!(p, TokenType::RightParen, "expected `)`")),
						))
					} else {
						Err(Diagnostic::new(
							Level::Error,
							"unexpected end of file: expected type or `)`",
						))
					}
				},
				precedence::CALL,
			)),
		}),
		TokenType::LeftBracket => Some(&ParseRule {
			prefix: Some(&|p, mode| p.parse_array(mode)),
			infix: Some((
				&|p, left, mode| {
					let index = p.parse_expression(mode)?;
					if mode != ExpressionParseMode::Template {
						Ok(Expression(
							ExpressionType::Index(Index {
								array: Box::new(left),
								index: Box::new(index),
							}),
							expect!(p, TokenType::RightBracket, "expected `]`"),
						))
					} else {
						Err(Diagnostic::new(
							Level::Error,
							"an array expression is not allowed in template expressions",
						)
						.add_label(Label::primary(p.file, "unexpected expression", index.1)))
					}
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
				&|p, left, mode| p.parse_binary(BinaryOperator::Greater, precedence::COMPARISON, left, mode),
				precedence::COMPARISON,
			)),
		}),
		TokenType::LeftChevron => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::Lesser, precedence::COMPARISON, left, mode),
				precedence::COMPARISON,
			)),
		}),
		TokenType::RightChevronEqual => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::GreaterThanOrEqual, precedence::COMPARISON, left, mode),
				precedence::COMPARISON,
			)),
		}),
		TokenType::LeftChevronEqual => Some(&ParseRule {
			prefix: None,
			infix: Some((
				&|p, left, mode| p.parse_binary(BinaryOperator::LesserThanOrEqual, precedence::COMPARISON, left, mode),
				precedence::COMPARISON,
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
					let expr = p.parse_expression(mode)?;
					let range = merge_range!(&left.1, &expr.1);
					let target =
						match left.0 {
							ExpressionType::Access(path) => AssignmentTarget::Var(path),
							ExpressionType::RPNAccess(expr) => AssignmentTarget::RPNVar(expr),
							ExpressionType::Index(index) => {
								if let ExpressionType::Access(path) = index.array.0 {
									AssignmentTarget::Index(path, index.index)
								} else {
									return Err(Diagnostic::new(Level::Error, "invalid assignment").add_label(
										Label::primary(p.file, "can only assign directly to arrays", left.1),
									));
								}
							},
							_ => {
								return Err(Diagnostic::new(Level::Error, "invalid assignment")
									.add_label(Label::primary(p.file, "can only assign to variables", left.1)))
							},
						};
					Ok(Expression(
						ExpressionType::Assignment(Assignment {
							target,
							value: Box::new(expr),
						}),
						range,
					))
				},
				precedence::CALL,
			)),
		}),
		TokenType::LeftBrace => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let tok = next!(p);
				let block = p.parse_block(mode)?;
				Ok(Expression(ExpressionType::Block(block.0), merge_range!(tok.1, block.1)))
			}),
			infix: None,
		}),
		TokenType::If => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let if_tok = next!(p);
				let mut range = if_tok.1;
				let mut chain = IfChain {
					ifs: vec![{
						let expr = Box::new(p.parse_expression(mode)?);
						expect!(p, TokenType::LeftBrace, "expected `block` after `if` condition");
						let block = p.parse_block(mode)?;
						range = merge_range!(range, block.1);
						(expr, block.0, range.clone())
					}],
					else_part: None,
				};

				while let Some(tok) = p.peek() {
					if let TokenType::Else = tok.0 {
						p.next();
						let next = next!(p);
						match next.0 {
							TokenType::LeftBrace => {
								chain.else_part = Some({
									let block = p.parse_block(mode)?;
									range = merge_range!(range, block.1.clone());
									(block.0, merge_range!(tok.1, block.1))
								})
							},
							TokenType::If => {
								let else_if = {
									let expr = Box::new(p.parse_expression(mode)?);
									expect!(p, TokenType::LeftBrace, "expected `block` after `if` condition");
									let block = p.parse_block(mode)?;
									range = merge_range!(range, block.1.clone());
									(expr, block.0, merge_range!(tok.1, block.1))
								};
								if let Some(else_part) = &chain.else_part {
									return Err(Diagnostic::new(Level::Error, "cannot have `else if` after `else`")
										.add_label(Label::secondary(p.file, "move this...", else_part.1.clone()))
										.add_label(Label::secondary(p.file, "after this", else_if.2)));
								} else {
									chain.ifs.push(else_if)
								}
							},
							_ => {
								return Err(Diagnostic::new(
									Level::Error,
									"expected `else if` or else block after `else`",
								)
								.add_label(Label::unexpected(p.file, &next)))
							},
						}
					} else {
						break;
					}
				}

				Ok(Expression(ExpressionType::IfChain(chain), range))
			}),
			infix: None,
		}),
		TokenType::Function => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let range = next!(p).1;
				let func = p.parse_function(mode)?;
				Ok(Expression(
					ExpressionType::Function(func.0),
					merge_range!(range, func.1),
				))
			}),
			infix: None,
		}),
		TokenType::Return => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let tok = next!(p);
				let range;
				let value = p.parse_expression(mode);
				let value = if let Ok(expr) = value {
					range = merge_range!(tok.1, &expr.1);
					Some(Box::new(expr))
				} else {
					range = tok.1;
					None
				};

				Ok(Expression(ExpressionType::Return(value), range))
			}),
			infix: None,
		}),
		TokenType::Break => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let tok = next!(p);
				let range;
				let value = p.parse_expression(mode);
				let value = if let Ok(expr) = value {
					range = merge_range!(tok.1, &expr.1);
					Some(Box::new(expr))
				} else {
					range = tok.1;
					None
				};

				Ok(Expression(ExpressionType::Break(value), range))
			}),
			infix: None,
		}),
		TokenType::Switch => Some(&ParseRule {
			prefix: Some(&|p, mode| {
				let tok = next!(p);
				let on = p.parse_expression(mode)?;
				expect!(p, TokenType::LeftBrace, "expected `{`");
				let mut cases = Vec::new();
				let next = peek!(p, else return Err(Diagnostic::new(Level::Error, "unexpected end of file")));
				if let TokenType::RightBrace = next.0 {
					p.next();
					Ok(Expression(
						ExpressionType::Switch(Switch {
							on: Box::new(on),
							cases,
						}),
						merge_range!(tok.1, next.1),
					))
				} else {
					loop {
						let value = p.parse_expression(mode)?;
						expect!(p, TokenType::Arrow, "expected `->`");
						let code = p.parse_expression(mode)?;

						cases.push(Case {
							value: Box::new(value),
							code: Box::new(code),
						});

						peek!(p, TokenType::Comma, else break);
					}

					Ok(Expression(
						ExpressionType::Switch(Switch {
							on: Box::new(on),
							cases,
						}),
						merge_range!(tok.1, expect!(p, TokenType::RightBrace, "expected `}`")),
					))
				}
			}),
			infix: None,
		}),
		TokenType::Use => Some(&ParseRule {
			prefix: Some(&|p, _| {
				let range = next!(p).1;
				let path = p.parse_path()?;
				let range = merge_range!(range, path.1.clone());
				let mut usee = Use {
					template: path,
					args: Vec::new(),
				};
				peek!(p, TokenType::Semicolon, if {
					return Ok(Expression(ExpressionType::Use(usee), range))
				});

				let args = p.parse_template_values()?;
				usee.args = args.0;

				Ok(Expression(ExpressionType::Use(usee), merge_range!(range, args.1)))
			}),
			infix: None,
		}),
		TokenType::Component => Some(&ParseRule {
			prefix: Some(&|p, _| {
				let range = next!(p).1;
				let name = p.parse_expression(ExpressionParseMode::Normal)?;
				let node = peek!(p, TokenType::On, if {
					Some(Box::new(p.parse_expression(ExpressionParseMode::Normal)?))
				} else {
					None
				});
				let statements = p.parse_template_block()?;
				Ok(Expression(
					ExpressionType::Component(Component {
						name: Box::new(name),
						node,
						block: statements.0,
					}),
					merge_range!(range, statements.1),
				))
			}),
			infix: None,
		}),
		TokenType::Animation => Some(&ParseRule {
			prefix: Some(&|p, _| {
				let range = next!(p).1;
				let name = p.parse_expression(ExpressionParseMode::Normal)?;
				let args = p.parse_template_values()?;
				let range = merge_range!(range, args.1);
				let mut args_iter = args.0.into_iter();
				let length = if let Some(length) = args_iter.find(|val| val.0 .0 == "length") {
					length
				} else {
					return Err(Diagnostic::new(Level::Error, "expected animation length")
						.add_label(Label::primary(p.file, "here", range)));
				};
				let lag = if let Some(lag) = args_iter.find(|val| val.0 .0 == "lag") {
					lag
				} else {
					return Err(Diagnostic::new(Level::Error, "expected animation lag")
						.add_label(Label::primary(p.file, "here", range)));
				};
				let code = if let Some(code) = args_iter.find(|val| val.0 .0 == "value") {
					code
				} else {
					return Err(Diagnostic::new(Level::Error, "expected animation code")
						.add_label(Label::primary(p.file, "here", range)));
				};

				Ok(Expression(
					ExpressionType::Animation(Animation {
						name: Box::new(name),
						length: Box::new(length.1),
						lag: Box::new(lag.1),
						code: Box::new(code.1),
					}),
					range,
				))
			}),
			infix: None,
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
	pub const COMPARISON: usize = 5;
	pub const TERM: usize = 6;
	pub const FACTOR: usize = 7;
	pub const UNARY: usize = 8;
	pub const CALL: usize = 9;
}

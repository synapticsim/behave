use std::collections::HashMap;
use std::{iter::Peekable, ops::Range};

use crate::ast::{
	Access,
	Animation,
	AssignmentTarget,
	BehaviorExpression,
	Component,
	Interaction,
	Location,
	OtherType,
	StructCreate,
	Update,
	UseTarget,
};
use crate::items::ItemMap;
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

pub struct Parser<'a, 'b>
where
	'a: 'b,
{
	mode: ParserMode,
	struct_literal_allowed: bool,
	file: &'a [String],
	lexer: Peekable<Lexer<'b>>,
	item_map: &'b mut ItemMap<'a>,
	diagnostics: &'b mut Vec<Diagnostic>,
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
		fn $name(&mut self) -> Result<Expression<'a>, Diagnostic> {
			let tok = next!(self);
			if let TokenType::$expect(val) = tok.0 {
				Ok(Expression(ExpressionType::$expect(val), self.loc(tok.1)))
			} else {
				Err(Diagnostic::new(Level::Error, $message).add_label(Label::unexpected(self.file, &tok)))
			}
		}
	};
}

impl<'a, 'b> Parser<'a, 'b> {
	parse_literal!(parse_string, String, "expected string literal");

	parse_literal!(parse_number, Number, "expected number literal");

	parse_literal!(parse_bool, Boolean, "expected boolean literal");

	pub fn new(
		mode: ParserMode, file: &'a [String], lexer: Lexer<'b>, type_map: &'b mut ItemMap<'a>,
		diagnostics: &'b mut Vec<Diagnostic>,
	) -> Self {
		Self {
			mode,
			struct_literal_allowed: true,
			file,
			lexer: lexer.peekable(),
			item_map: type_map,
			diagnostics,
		}
	}

	pub fn parse(mut self) -> Option<AST<'a>> {
		let mut error = false;
		let mut ast = AST {
			imports: Vec::new(),
			ast_data: match self.mode {
				ParserMode::MainFile => ASTType::Main(
					LODs(Vec::new(), self.loc(no_range())),
					Behavior(Vec::new(), self.loc(no_range())),
				),
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
					let r = p.1.range.clone();
					ast.imports
						.push(Import(ImportType::Normal(p), self.loc(merge_range!(token.1, r))));
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
					let r = p.1.range.clone();
					ast.imports
						.push(Import(ImportType::Extern(p), self.loc(merge_range!(token.1, r))));
				},
				TokenType::Lods => match ast.ast_data {
					ASTType::Main(ref mut lods, _) => {
						*lods = resync!(self, self.parse_lods(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						lods.1 = self.loc(merge_range!(token.1, &lods.1.range));
					},
					ASTType::Secondary(..) => {
						error = true;
						let lods = resync!(self, self.parse_lods(), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "LOD description is only allowed in the main file")
								.add_label(Label::primary(
									"move this to the main `.beh` file",
									self.loc(merge_range!(token.1, lods.1.range)),
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
						behaviors.1 = self.loc(merge_range!(token.1, &behaviors.1.range));
					},
					ASTType::Secondary(..) => {
						error = true;
						let behaviors =
							resync!(self, self.parse_behavior(), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Behavior description is only allowed in the main file")
								.add_label(Label::primary(
									"move this to the main `.beh` file",
									self.loc(merge_range!(token.1, behaviors.1.range)),
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
									"move this to the imported `.beh` file",
									self.loc(merge_range!(token.1, template.1.range)),
								)),
						);
					},
					ASTType::Secondary(ref mut items) => {
						let mut template = resync!(self, self.parse_template(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						template.1 = self.loc(merge_range!(token.1, &template.1.range));
						items.push(Item(
							ItemType::Template(self.item_map.add_template(template.0)),
							template.1,
						));
					},
				},
				TokenType::Enum => match ast.ast_data {
					ASTType::Main(..) => {
						error = true;
						let item = resync!(self, self.parse_enum(), until TokenType::RightBrace, else continue 'w);
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Enum declaration is only allowed in imported files")
								.add_label(Label::primary(
									"move this to an imported `.beh` file",
									self.loc(merge_range!(token.1, item.1.range)),
								)),
						);
					},
					ASTType::Secondary(ref mut items) => {
						let mut item = resync!(self, self.parse_enum(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						item.1 = self.loc(merge_range!(token.1, item.1.range));
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
									"move this to an imported `.beh` file",
									self.loc(merge_range!(token.1, item.1.range)),
								)),
						);
					},
					ASTType::Secondary(ref mut items) => {
						let mut item = resync!(self, self.parse_struct(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
						item.1 = self.loc(merge_range!(token.1, item.1.range));
						items.push(item);
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
									"move this to an imported `.beh` file",
									self.loc(merge_range!(token.1, func.1.range)),
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
						items.push(Item(
							ItemType::Function(ident, self.item_map.add_function(func.0)),
							self.loc(merge_range!(token.1, func.1.range)),
						));
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

	fn parse_lods(&mut self) -> Result<LODs<'a>, Diagnostic> {
		let mut lods = LODs(Vec::new(), self.loc(no_range()));
		lods.1 = {
			let temp = expect!(self, TokenType::LeftBrace, "expected `{`");
			self.loc(temp)
		};

		loop {
			let min_size = self.parse_expression(ExpressionParseMode::Normal)?;
			expect!(self, TokenType::Arrow, "expected `->`");
			let file = self.parse_expression(ExpressionParseMode::Normal)?;
			let (r1, r2) = (min_size.1.clone(), file.1.clone());
			lods.0.push(LOD {
				min_size,
				file,
				loc: self.loc(merge_range!(r1.range, r2.range)),
			});

			peek!(self, TokenType::Comma, else break);
		}

		lods.1 = {
			let temp = merge_range!(lods.1.range, expect!(self, TokenType::RightBrace, "expected `}`"));
			self.loc(temp)
		};
		Ok(lods)
	}

	fn parse_behavior(&mut self) -> Result<Behavior<'a>, Diagnostic> {
		let statements = self.parse_template_block()?;
		Ok(Behavior(statements.0, statements.1))
	}

	fn parse_template(&mut self) -> Result<(Template<'a>, Location<'a>), Diagnostic> {
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
				range = merge_range!(range, &arg.loc.range);
				template.args.push(arg);
				peek!(self, TokenType::Comma, else break);
			}
		}
		range = merge_range!(range, expect!(self, TokenType::RightParen, "expected `)`"));

		let statements = self.parse_template_block()?;
		template.block = statements.0;
		Ok((template, self.loc(merge_range!(range, statements.1.range))))
	}

	fn parse_enum(&mut self) -> Result<Item<'a>, Diagnostic> {
		let mut item = Enum {
			name: self.parse_ident()?,
			variants: Vec::new(),
		};
		let mut range = merge_range!(
			item.name.1.range.clone(),
			expect!(self, TokenType::LeftBrace, "expected `{`")
		);
		let mut last_val = 0;
		loop {
			let ident = self.parse_ident()?;
			let mut range = ident.1.range.clone();
			let value = peek!(self, TokenType::Equal, if {
				let num = self.parse_number()?;
				let n = if let ExpressionType::Number(n) = num.0 {
					let v = n.floor() as usize;
					last_val = v;
					v
				} else {
					unreachable!()
				};
				range = merge_range!(range, num.1.range.clone());
				n
			} else {
				last_val += 1;
				last_val
			});

			item.variants.push(EnumVariant {
				name: ident,
				value,
				loc: self.loc(range),
			});

			peek!(self, TokenType::Comma, else break);
		}

		let mut values = HashMap::new();
		let mut errors = Vec::new();
		for variant in item.variants.iter().enumerate() {
			if let Some(idx) = values.get(&variant.1.value) {
				let old: &EnumVariant = &item.variants[*idx];
				let new = &item.variants[variant.0];

				errors.push(
					Diagnostic::new(Level::Error, "two enum variants cannot have the same value")
						.add_label(Label::primary(
							format!("this has value '{}'", new.value),
							new.loc.clone(),
						))
						.add_label(Label::secondary("previously defined here", old.loc.clone())),
				);
			} else {
				values.insert(variant.1.value, variant.0);
			}
		}

		if errors.len() == 0 {
			range = merge_range!(range, expect!(self, TokenType::RightBrace, "expected `}`"));
			Ok(Item(ItemType::Enum(self.item_map.add_enum(item)), self.loc(range)))
		} else {
			let mut iter = errors.into_iter();
			let ret = Err(iter.next().unwrap());
			self.diagnostics.extend(iter);
			ret
		}
	}

	fn parse_struct(&mut self) -> Result<Item<'a>, Diagnostic> {
		let name = self.parse_ident()?;
		let mut fields = Vec::new();
		let mut range = name.1.range.clone();
		expect!(self, TokenType::LeftBrace, "expected `{`");
		if let Some(tok) = self.peek() {
			if let TokenType::RightBrace = tok.0 {
				range = tok.1;
			} else {
				loop {
					let field = self.parse_var_entry()?;
					range = merge_range!(range, &field.loc.range);
					fields.push(field);
					peek!(self, TokenType::Comma, else break);
				}
			}
		} else {
			return Err(Diagnostic::new(Level::Error, "unexpected end of file: expected `}`"));
		}

		Ok(Item(
			ItemType::Struct(self.item_map.add_struct(Struct { name, fields })),
			{
				let temp = expect!(self, TokenType::RightBrace, "expected `}`");
				self.loc(merge_range!(range, temp))
			},
		))
	}

	fn parse_variable(&mut self, mode: ExpressionParseMode) -> Result<(Variable<'a>, Location<'a>), Diagnostic> {
		let name = self.parse_ident()?;
		let range = name.1.range.clone();
		let mut last_range = range.clone();
		let value = peek!(self, TokenType::Equal, if {
			let expr = self.parse_expression(mode)?;
			last_range = expr.1.range.clone();
			Some(expr)
		} else {
			None
		});
		Ok((Variable { name, value }, {
			let tok = next!(self);
			self.loc(merge_range!(&range, {
				match tok.0 {
					TokenType::Semicolon => {},
					_ => {
						return Err(Diagnostic::new(Level::Error, "expected `semicolon`")
							.add_label(Label::primary("add a semicolon after this", self.loc(last_range))))
					},
				};
				tok.1
			}))
		}))
	}

	fn parse_var_entry(&mut self) -> Result<VarEntry<'a>, Diagnostic> {
		let name = self.parse_ident()?;
		expect!(self, TokenType::Colon, "expected `colon`");
		let ty = self.parse_type()?;
		let default = peek!(self, TokenType::Equal, if {
			Some(Box::new(self.parse_expression(ExpressionParseMode::Normal)?))
		} else {
			None
		});

		Ok(VarEntry {
			loc: self.loc(merge_range!(&name.1.range, &ty.1.range)),
			name,
			ty,
			default,
		})
	}

	fn parse_type(&mut self) -> Result<Type<'a>, Diagnostic> {
		let mut sum_ty = Vec::new();
		loop {
			let tok = next!(self);
			sum_ty.push(match tok.0 {
				TokenType::Num => Type(TypeType::Num, self.loc(tok.1)),
				TokenType::Str => Type(TypeType::Str, self.loc(tok.1)),
				TokenType::Bool => Type(TypeType::Bool, self.loc(tok.1)),
				TokenType::Code => Type(TypeType::Code, self.loc(tok.1)),
				TokenType::Ident(s) => {
					let mut path = vec![Ident(s, self.loc(tok.1.clone()))];
					let mut range = tok.1;
					loop {
						peek!(self, TokenType::Period, else break);
						let ident = self.parse_ident()?;
						range = merge_range!(range, &ident.1.range);
						path.push(ident);
					}

					Type(
						TypeType::Other(OtherType {
							path: Path(path, self.loc(range.clone())),
							resolved: None,
						}),
						self.loc(range),
					)
				},
				TokenType::LeftBracket => {
					let key = self.parse_type()?;
					let ty = peek!(self, TokenType::Colon, if {
						let value = self.parse_type()?;
						TypeType::Map(Box::new(key), Box::new(value))
					} else {
						TypeType::Array(Box::new(key))
					});
					Type(ty, {
						let temp = expect!(self, TokenType::RightBracket, "expected `]`");
						self.loc(merge_range!(tok.1, temp))
					})
				},
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
								range = merge_range!(range, &ty.1.range);
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
						range = merge_range!(range, &ret.1.range);
						ty.ret = Some(Box::new(ret))
					});
					Type(TypeType::Function(ty), self.loc(range))
				},
				_ => {
					return Err(
						Diagnostic::new(Level::Error, "expected type").add_label(Label::unexpected(self.file, &tok))
					)
				},
			});

			peek!(self, TokenType::Pipe, else break);
		}

		if sum_ty.len() == 1 {
			Ok(sum_ty.into_iter().next().unwrap())
		} else {
			let range = merge_range!(sum_ty[0].1.range.clone(), sum_ty.last().unwrap().1.range.clone());
			Ok(Type(TypeType::Sum(sum_ty), self.loc(range)))
		}
	}

	fn parse_template_block(&mut self) -> Result<(Vec<Statement<'a>>, Location<'a>), Diagnostic> {
		let mut statements = Vec::new();
		let mut range = expect!(self, TokenType::LeftBrace, "expected `{`");
		let mut tok = peek!(self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file")));
		if let TokenType::RightBrace = tok.0 {
			self.next();
			range = tok.1;
			Ok((statements, self.loc(range)))
		} else {
			loop {
				match tok.0 {
					TokenType::Let => {
						self.next();
						let item = self.parse_variable(ExpressionParseMode::Normal)?;
						range = merge_range!(range, &item.1.range);
						statements.push(Statement(StatementType::Declaration(item.0), item.1));
					},
					_ => {
						let expr = self.parse_expression(ExpressionParseMode::Template)?;
						range = merge_range!(range, &expr.1.range);
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

			Ok((statements, self.loc(range)))
		}
	}

	fn parse_values(
		&mut self, mode: ExpressionParseMode,
	) -> Result<(Vec<(Ident<'a>, Expression<'a>)>, Location<'a>), Diagnostic> {
		let range = expect!(self, TokenType::LeftBrace, "expected `{`");
		let mut args = Vec::new();
		if let Some(tok) = self.peek() {
			if let TokenType::RightBrace = tok.0 {
			} else {
				loop {
					let arg = self.parse_ident()?;
					expect!(self, TokenType::Colon, "expected `colon`");
					let value = self.parse_expression(mode)?;
					args.push((arg, value));

					peek!(self, TokenType::Comma, else break);
				}
			}
		} else {
			return Err(Diagnostic::new(
				Level::Error,
				"unexpected end of file: expected value setter or `}`",
			));
		}
		Ok((args, {
			let temp = expect!(self, TokenType::RightBrace, "expected `}`");
			self.loc(merge_range!(range, temp))
		}))
	}

	fn parse_expression(&mut self, mode: ExpressionParseMode) -> Result<Expression<'a>, Diagnostic> {
		self.parse_with_precedence(precedence::ASSIGNMENT, mode)
	}

	fn parse_array_or_map(&mut self, mode: ExpressionParseMode) -> Result<Expression<'a>, Diagnostic> {
		let mut range = self.next().unwrap().1;

		let key = self.parse_expression(mode)?;
		let expr = peek!(self, TokenType::Colon, if {
			let mut map = Vec::new();
			let value = self.parse_expression(mode)?;
			map.push((key, value));
			if let Some(tok) = self.peek() {
				if let TokenType::Comma = tok.0 {
					self.next();
					loop {
						let key = self.parse_expression(mode)?;
						expect!(self, TokenType::Colon, "expected `colon`");
						let value = self.parse_expression(mode)?;
						range = merge_range!(range, &value.1.range);
						map.push((key, value));
						peek!(self, TokenType::Comma, else break);
					}
				}

				ExpressionType::Map(map)
			} else {
				return Err(Diagnostic::new(
					Level::Error,
					"unexpected end of file: expected expression",
				));
			}
		} else {
			let mut array = vec![key];
			if let Some(tok) = self.peek() {
				if let TokenType::Comma = tok.0 {
					self.next();
					loop {
						let expr = self.parse_expression(mode)?;
						range = merge_range!(range, &expr.1.range);
						array.push(expr);
						peek!(self, TokenType::Comma, else break);
					}
				}

				ExpressionType::Array(array)
			} else {
				return Err(Diagnostic::new(
					Level::Error,
					"unexpected end of file: expected expression",
				));
			}
		});

		let temp = expect!(self, TokenType::RightBracket, "expected `]`");
		let loc = self.loc(merge_range!(range, temp));
		if mode == ExpressionParseMode::Normal {
			Ok(Expression(expr, loc))
		} else {
			Err(Diagnostic::new(
				Level::Error,
				"an array or map expression is not allowed in this context",
			)
			.add_label(Label::primary("unexpected expression", loc.clone())))
		}
	}

	fn parse_unary(&mut self, op: UnaryOperator, mode: ExpressionParseMode) -> Result<Expression<'a>, Diagnostic> {
		let tok = next!(self);
		let expr = self.parse_with_precedence(precedence::UNARY, mode)?;
		let range = merge_range!(tok.1, &expr.1.range);
		if mode != ExpressionParseMode::Template {
			Ok(Expression(ExpressionType::Unary(op, Box::new(expr)), self.loc(range)))
		} else {
			Err(Diagnostic::new(
				Level::Error,
				"a unary expression is not allowed in template expressions",
			)
			.add_label(Label::primary("unexpected expression", self.loc(range))))
		}
	}

	fn parse_binary(
		&mut self, op: BinaryOperator, precedence: usize, left: Expression<'a>, mode: ExpressionParseMode,
	) -> Result<Expression<'a>, Diagnostic> {
		let right = self.parse_with_precedence(precedence + 1, mode)?;
		let range = merge_range!(&left.1.range, &right.1.range);
		if mode != ExpressionParseMode::Template {
			Ok(Expression(
				ExpressionType::Binary(Box::new(left), op, Box::new(right)),
				self.loc(range),
			))
		} else {
			Err(Diagnostic::new(
				Level::Error,
				"a binary expression is not allowed in template expressions",
			)
			.add_label(Label::primary("unexpected expression", self.loc(range))))
		}
	}

	fn parse_rpn(&mut self, mode: ExpressionParseMode) -> Result<Expression<'a>, Diagnostic> {
		let tok = next!(self);
		match mode {
			ExpressionParseMode::Normal => {
				let next = next!(self);
				if let TokenType::LeftBrace = next.0 {
					let block = self.parse_block(ExpressionParseMode::RPNCode)?;
					Ok(Expression(
						ExpressionType::Code(block.0),
						self.loc(merge_range!(tok.1, block.1.range)),
					))
				} else {
					Err(Diagnostic::new(Level::Error, "expected RPN code block")
						.add_label(Label::unexpected(self.file, &next))
						.add_note("RPN variables can only be read when inside an RPN code block"))
				}
			},
			ExpressionParseMode::RPNCode => {
				let variable = match self.parse_with_precedence(precedence::CALL, mode) {
					Ok(expr) => expr,
					Err(diag) => return Err(diag.add_note("expected MSFS RPN variable")),
				};
				let range = variable.1.range.clone();
				Ok(Expression(
					ExpressionType::RPNAccess(Box::new(variable)),
					self.loc(merge_range!(tok.1, range)),
				))
			},
			ExpressionParseMode::Template => Err(Diagnostic::new(
				Level::Error,
				"expected either component or template expression",
			)
			.add_label(Label::primary("unexpected RPN expression", self.loc(tok.1)))),
		}
	}

	fn parse_block(&mut self, mode: ExpressionParseMode) -> Result<(Block<'a>, Location<'a>), Diagnostic> {
		let mut block = Block {
			statements: Vec::new(),
			expression: None,
			loc: self.loc(no_range()),
		};
		let mut range = no_range();
		let mut tok = peek!(self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file")));
		if let TokenType::RightBrace = tok.0 {
			self.next();
			Ok((block, self.loc(tok.1)))
		} else {
			loop {
				match tok.0 {
					TokenType::Let => {
						self.next();
						let item = self.parse_variable(mode)?;
						range = merge_range!(range, &item.1.range);
						block
							.statements
							.push(Statement(StatementType::Declaration(item.0), item.1));
					},
					_ => {
						let expr = self.parse_expression(mode)?;
						range = merge_range!(range, &expr.1.range);
						peek!(self, TokenType::Semicolon, if {
							block.statements.push(Statement(StatementType::Expression(expr.0), expr.1))
						} else {
							if let Some(expr) = block.expression {
								return Err(
									Diagnostic::new(Level::Error, "missing semicolon after statement")
									.add_label(Label::primary("add a semicolon after this expression", expr.1))
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

			block.loc = self.loc(range.clone());
			Ok((block, self.loc(range)))
		}
	}

	fn parse_with_precedence(
		&mut self, precedence: usize, mode: ExpressionParseMode,
	) -> Result<Expression<'a>, Diagnostic> {
		let tok = peek!(self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file")));
		let prefix = if let Some(rule) = self.get_parse_rule(&tok.0) {
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
		loop {
			let tok = peek!(self, else return Ok(expr));
			let infix = if let Some(rule) = self.get_parse_rule(&tok.0) {
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
			expr.1 = self.loc(merge_range!(tok.1, expr.1.range));
		}
		expr.1 = self.loc(merge_range!(tok.1, expr.1.range));

		Ok(expr)
	}

	fn parse_function(&mut self, mode: ExpressionParseMode) -> Result<(Function<'a>, Location<'a>), Diagnostic> {
		let mut params = Vec::new();
		let mut range = expect!(self, TokenType::LeftParen, "expected `(`");
		let tok = peek!(self, else return Err(Diagnostic::new(Level::Error, "unexpected end of file: expected `)`")));
		if let TokenType::RightParen = tok.0 {
			range = tok.1;
		} else {
			loop {
				let arg = self.parse_var_entry()?;
				range = merge_range!(range, &arg.loc.range);
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

		let mut block = self.parse_block(mode)?;
		block.1 = self.loc(merge_range!(&range, block.1.range.clone()));
		range = merge_range!(range, block.1.range);
		Ok((
			Function {
				args: params,
				ret,
				block: block.0,
			},
			self.loc(range),
		))
	}

	fn parse_component(&mut self) -> Result<(Component<'a>, Range<usize>), Diagnostic> {
		let name = self.parse_expression(ExpressionParseMode::Normal)?;
		let node = peek!(self, TokenType::Ident(i) if i == "on", if {
			self.struct_literal_allowed = false;
			Some(Box::new(self.parse_expression(ExpressionParseMode::Normal)?))
		} else {
			None
		});
		self.struct_literal_allowed = true;
		let statements = self.parse_template_block()?;
		Ok((
			Component {
				name: Box::new(name),
				node,
				block: statements.0,
			},
			statements.1.range,
		))
	}

	fn parse_animation(&mut self) -> Result<(Animation<'a>, Range<usize>), Diagnostic> {
		self.struct_literal_allowed = false;
		let name = Box::new(self.parse_expression(ExpressionParseMode::Normal)?);
		self.struct_literal_allowed = true;
		let args = self.parse_values(ExpressionParseMode::Normal)?;
		let range = args.1.range;
		let args_iter = args.0.iter();
		let length = Box::new(
			if let Some(length) = args_iter.clone().find(|val| val.0 .0 == "length") {
				length.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected animation length")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let lag = Box::new(if let Some(lag) = args_iter.clone().find(|val| val.0 .0 == "lag") {
			lag.1.clone()
		} else {
			return Err(Diagnostic::new(Level::Error, "expected animation lag")
				.add_label(Label::primary("here", self.loc(range))));
		});
		let value = Box::new(if let Some(code) = args_iter.clone().find(|val| val.0 .0 == "value") {
			code.1.clone()
		} else {
			return Err(Diagnostic::new(Level::Error, "expected animation value")
				.add_label(Label::primary("here", self.loc(range))));
		});

		Ok((
			Animation {
				name,
				length,
				lag,
				value,
			},
			range,
		))
	}

	fn parse_interaction(&mut self) -> Result<(Interaction<'a>, Range<usize>), Diagnostic> {
		let args = self.parse_values(ExpressionParseMode::Normal)?;
		let range = args.1.range;
		let args_iter = args.0.iter();
		let legacy_cursors = Box::new(
			if let Some(cursors) = args_iter.clone().find(|val| val.0 .0 == "legacy_cursors") {
				cursors.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected legacy cursors")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let lock_cursors = Box::new(
			if let Some(cursors) = args_iter.clone().find(|val| val.0 .0 == "lock_cursors") {
				cursors.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected lock cursors")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let legacy_events = Box::new(
			if let Some(events) = args_iter.clone().find(|val| val.0 .0 == "legacy_events") {
				events.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected legacy events")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let lock_events = Box::new(
			if let Some(events) = args_iter.clone().find(|val| val.0 .0 == "lock_events") {
				events.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected lock events")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let legacy_callback = Box::new(
			if let Some(events) = args_iter.clone().find(|val| val.0 .0 == "legacy_callback") {
				events.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected legacy callback")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let lock_callback = Box::new(
			if let Some(callback) = args_iter.clone().find(|val| val.0 .0 == "lock_callback") {
				callback.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected lock callback")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let lock_tooltip_title = Box::new(
			if let Some(callback) = args_iter.clone().find(|val| val.0 .0 == "lock_tooltip_title") {
				callback.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected lock tooltip title")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let lock_tooltips = Box::new(
			if let Some(callback) = args_iter.clone().find(|val| val.0 .0 == "lock_tooltips") {
				callback.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected lock tooltips")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let can_lock = Box::new(
			if let Some(callback) = args_iter.clone().find(|val| val.0 .0 == "can_lock") {
				callback.1.clone()
			} else {
				return Err(Diagnostic::new(Level::Error, "expected can_lock")
					.add_label(Label::primary("here", self.loc(range))));
			},
		);
		let node_to_highlight = if let Some(node) = args_iter.clone().find(|val| val.0 .0 == "node_to_highlight") {
			Some(Box::new(node.1.clone()))
		} else {
			None
		};

		Ok((
			Interaction {
				legacy_cursors,
				lock_cursors,
				legacy_events,
				lock_events,
				legacy_callback,
				lock_callback,
				lock_tooltip_title,
				lock_tooltips,
				can_lock,
				node_to_highlight,
			},
			range,
		))
	}

	fn parse_events(&mut self) -> Result<(Box<Expression<'a>>, Box<Expression<'a>>, Range<usize>), Diagnostic> {
		self.struct_literal_allowed = false;
		let on = self.parse_with_precedence(precedence::COMPARISON, ExpressionParseMode::Normal)?;
		self.struct_literal_allowed = true;
		expect!(self, TokenType::Equal, "expected `=`");
		let events = self.parse_expression(ExpressionParseMode::Normal)?;
		let range = merge_range!(&on.1.range, &events.1.range);
		Ok((Box::new(on), Box::new(events), range))
	}

	fn parse_update(&mut self) -> Result<(Update<'a>, Range<usize>), Diagnostic> {
		let args = self.parse_values(ExpressionParseMode::Normal)?;
		let range = args.1.range;
		let args_iter = args.0.iter();
		let frequency = if let Some(frequency) = args_iter.clone().find(|val| val.0 .0 == "frequency") {
			Some(Box::new(frequency.1.clone()))
		} else {
			None
		};
		let mode = Box::new(if let Some(mode) = args_iter.clone().find(|val| val.0 .0 == "mode") {
			mode.1.clone()
		} else {
			return Err(Diagnostic::new(Level::Error, "expected update interaction mode")
				.add_label(Label::primary("here", self.loc(range))));
		});
		let code = Box::new(if let Some(code) = args_iter.clone().find(|val| val.0 .0 == "do") {
			code.1.clone()
		} else {
			return Err(Diagnostic::new(Level::Error, "expected update code")
				.add_label(Label::primary("here", self.loc(range))));
		});

		Ok((Update { frequency, mode, code }, range))
	}

	fn parse_path(&mut self) -> Result<Path<'a>, Diagnostic> {
		let mut idents = vec![];
		let mut range = no_range();
		loop {
			let ident = self.parse_ident()?;
			range = merge_range!(range, &ident.1.range);
			idents.push(ident);

			peek!(self, TokenType::Period, else break);
		}

		Ok(Path(idents, self.loc(range)))
	}

	fn parse_ident(&mut self) -> Result<Ident<'a>, Diagnostic> {
		let tok = next!(self);
		if let TokenType::Ident(val) = tok.0 {
			Ok(Ident(val, self.loc(tok.1)))
		} else {
			Err(Diagnostic::new(Level::Error, "expected identifier").add_label(Label::unexpected(self.file, &tok)))
		}
	}

	fn loc(&self, range: Range<usize>) -> Location<'a> { Location { file: self.file, range } }

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

	fn get_parse_rule<'c>(&self, token: &TokenType) -> Option<&ParseRule<'a, 'b>> {
		match token {
			TokenType::At => Some(&ParseRule {
				prefix: Some(&|p, mode| p.parse_rpn(mode)),
				infix: None,
			}),
			TokenType::Ident(_) => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					if mode == ExpressionParseMode::Template {
						let next = next!(p);
						match if let TokenType::Ident(ref s) = next.0 {
							s.as_str()
						} else {
							unreachable!()
						} {
							"component" => {
								let component = p.parse_component()?;
								Ok(Expression(
									ExpressionType::Behavior(BehaviorExpression::Component(component.0)),
									p.loc(merge_range!(next.1, component.1)),
								))
							},
							"animation" => {
								let animation = p.parse_animation()?;
								Ok(Expression(
									ExpressionType::Behavior(BehaviorExpression::Animation(animation.0)),
									p.loc(merge_range!(next.1, animation.1)),
								))
							},
							"visibility" => {
								let expr = Box::new(p.parse_expression(ExpressionParseMode::Normal)?);
								let range = merge_range!(next.1, expr.1.range.clone());
								Ok(Expression(
									ExpressionType::Behavior(BehaviorExpression::Visible(expr)),
									p.loc(range),
								))
							},
							"emissive" => {
								let expr = Box::new(p.parse_expression(ExpressionParseMode::Normal)?);
								let range = merge_range!(next.1, expr.1.range.clone());
								Ok(Expression(
									ExpressionType::Behavior(BehaviorExpression::Emissive(expr)),
									p.loc(range),
								))
							},
							"interaction" => {
								let interaction = p.parse_interaction()?;
								Ok(Expression(
									ExpressionType::Behavior(BehaviorExpression::Interaction(interaction.0)),
									p.loc(merge_range!(next.1, interaction.1)),
								))
							},
							"events" => {
								let events = p.parse_events()?;
								Ok(Expression(
									ExpressionType::Behavior(BehaviorExpression::Events(events.0, events.1)),
									p.loc(merge_range!(next.1, events.2)),
								))
							},
							"update" => {
								let update = p.parse_update()?;
								Ok(Expression(
									ExpressionType::Behavior(BehaviorExpression::Update(update.0)),
									p.loc(merge_range!(next.1, update.1)),
								))
							},
							_ => Err(Diagnostic::new(Level::Error, "expected behavior expression")
								.add_label(Label::unexpected(p.file, &next))),
						}
					} else {
						let path = p.parse_path()?;
						let r = path.1.clone();
						if let Some(tok) = p.peek() {
							if p.struct_literal_allowed && matches!(tok.0, TokenType::LeftBrace) {
								let ty = Type(TypeType::Other(OtherType { path, resolved: None }), r.clone());
								if mode != ExpressionParseMode::Template {
									let values = p.parse_values(mode)?;
									Ok(Expression(
										ExpressionType::StructCreate(StructCreate { ty, values: values.0 }),
										p.loc(merge_range!(r.range, values.1.range)),
									))
								} else {
									Err(Diagnostic::new(
										Level::Error,
										"struct creation is not allowed in template expressions",
									)
									.add_label(Label::primary("the struct was created here", r)))
								}
							} else {
								Ok(Expression(ExpressionType::Access(Access { path, resolved: None }), r))
							}
						} else {
							Ok(Expression(ExpressionType::Access(Access { path, resolved: None }), r))
						}
					}
				}),
				infix: None,
			}),
			TokenType::None => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					let tok = next!(p);
					if mode == ExpressionParseMode::Normal {
						Ok(Expression(ExpressionType::None, p.loc(tok.1)))
					} else {
						Err(
							Diagnostic::new(Level::Error, "a none expression is not allowed in this context")
								.add_label(Label::primary("unexpected expression", p.loc(tok.1))),
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
						.add_label(Label::primary("unexpected expression", num.1)))
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
							"a string expression is not allowed in behavior expressions",
						)
						.add_label(Label::primary("unexpected expression", string.1)))
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
							"a boolean expression is not allowed in behavior expressions",
						)
						.add_label(Label::primary("unexpected expression", boolean.1)))
					}
				}),
				infix: None,
			}),
			TokenType::LeftParen => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					p.next();
					let expr = p.parse_expression(mode)?;
					if mode != ExpressionParseMode::Template {
						let range = expr.1.range.clone();
						Ok(Expression(expr.0, {
							let temp = expect!(p, TokenType::RightParen, "expected `)`");
							p.loc(merge_range!(range, temp))
						}))
					} else {
						Err(Diagnostic::new(
							Level::Error,
							"a grouping expression is not allowed in behavior expressions",
						)
						.add_label(Label::primary("unexpected expression", expr.1)))
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
									range = merge_range!(range, &expr.1.range);
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
								{
									let temp = expect!(p, TokenType::RightParen, "expected `)`");
									p.loc(merge_range!(l_range.range, range, temp))
								},
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
				prefix: Some(&|p, mode| p.parse_array_or_map(mode)),
				infix: Some((
					&|p, left, mode| {
						let index = p.parse_expression(mode)?;
						if mode == ExpressionParseMode::Normal {
							Ok(Expression(
								ExpressionType::Index(Index {
									array: Box::new(left),
									index: Box::new(index),
								}),
								{
									let temp = expect!(p, TokenType::RightBracket, "expected `]`");
									p.loc(temp)
								},
							))
						} else {
							Err(
								Diagnostic::new(Level::Error, "an index expression is not allowed in this context")
									.add_label(Label::primary("unexpected expression", index.1)),
							)
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
					&|p, left, mode| {
						p.parse_binary(BinaryOperator::GreaterThanOrEqual, precedence::COMPARISON, left, mode)
					},
					precedence::COMPARISON,
				)),
			}),
			TokenType::LeftChevronEqual => Some(&ParseRule {
				prefix: None,
				infix: Some((
					&|p, left, mode| {
						p.parse_binary(BinaryOperator::LesserThanOrEqual, precedence::COMPARISON, left, mode)
					},
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
						let range = merge_range!(&left.1.range, &expr.1.range);
						let target = match left.0 {
							ExpressionType::Access(access) => AssignmentTarget::Var(access),
							ExpressionType::RPNAccess(expr) => AssignmentTarget::RPNVar(expr),
							ExpressionType::Index(index) => {
								if let ExpressionType::Access(access) = index.array.0 {
									AssignmentTarget::Index(access, index.index)
								} else {
									return Err(Diagnostic::new(Level::Error, "invalid assignment")
										.add_label(Label::primary("can only assign directly to arrays", left.1)));
								}
							},
							_ => {
								return Err(Diagnostic::new(Level::Error, "invalid assignment")
									.add_label(Label::primary("can only assign to variables", left.1)))
							},
						};
						Ok(Expression(
							ExpressionType::Assignment(Assignment {
								target,
								value: Box::new(expr),
							}),
							p.loc(range),
						))
					},
					precedence::ASSIGNMENT,
				)),
			}),
			TokenType::LeftBrace => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					let tok = next!(p);
					let block = p.parse_block(mode)?;
					Ok(Expression(
						ExpressionType::Block(block.0),
						p.loc(merge_range!(tok.1, block.1.range)),
					))
				}),
				infix: None,
			}),
			TokenType::Is => Some(&ParseRule {
				prefix: None,
				infix: Some((
					&|p, left, mode| {
						let ty = p.parse_type()?;
						let range = merge_range!(left.1.range.clone(), ty.1.range.clone());
						if mode == ExpressionParseMode::Normal {
							Ok(Expression(ExpressionType::Is(Box::new(left), ty), p.loc(range)))
						} else {
							Err(
								Diagnostic::new(Level::Error, "an is expression is not allowed in this context")
									.add_label(Label::primary("unexpected expression", p.loc(range))),
							)
						}
					},
					precedence::ASSIGNMENT,
				)),
			}),
			TokenType::As => Some(&ParseRule {
				prefix: None,
				infix: Some((
					&|p, left, mode| {
						let ty = p.parse_type()?;
						let range = merge_range!(left.1.range.clone(), ty.1.range.clone());
						if mode == ExpressionParseMode::RPNCode {
							Ok(Expression(ExpressionType::As(Box::new(left), ty), p.loc(range)))
						} else {
							Err(
								Diagnostic::new(Level::Error, "an as expression is not allowed in this context")
									.add_label(Label::primary("unexpected expression", p.loc(range))),
							)
						}
					},
					precedence::CALL,
				)),
			}),
			TokenType::If => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					let if_tok = next!(p);
					let mut range = if_tok.1;
					let mut chain = IfChain {
						ifs: vec![{
							p.struct_literal_allowed = false;
							let expr = Box::new(p.parse_expression(if mode == ExpressionParseMode::RPNCode {
								ExpressionParseMode::RPNCode
							} else {
								ExpressionParseMode::Normal
							})?);
							expect!(p, TokenType::LeftBrace, "expected `block` after `if` condition");
							p.struct_literal_allowed = true;
							let block = p.parse_block(mode)?;
							range = merge_range!(range, block.1.range);
							(expr, block.0, p.loc(range.clone()))
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
										range = merge_range!(range, &block.1.range);
										(block.0, p.loc(merge_range!(tok.1, block.1.range)))
									})
								},
								TokenType::If => {
									let else_if = {
										let expr = Box::new(p.parse_expression(mode)?);
										expect!(p, TokenType::LeftBrace, "expected `block` after `if` condition");
										let block = p.parse_block(mode)?;
										range = merge_range!(range, &block.1.range);
										(expr, block.0, p.loc(merge_range!(tok.1, block.1.range)))
									};
									if let Some(else_part) = &chain.else_part {
										return Err(Diagnostic::new(
											Level::Error,
											"cannot have `else if` after `else`",
										)
										.add_label(Label::secondary("move this...", else_part.1.clone()))
										.add_label(Label::secondary("after this", else_if.2.clone())));
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

					Ok(Expression(ExpressionType::IfChain(chain), p.loc(range.clone())))
				}),
				infix: None,
			}),
			TokenType::Function => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					let range = next!(p).1;
					let func = p.parse_function(mode)?;
					let loc = p.loc(merge_range!(range, func.1.range));
					if mode == ExpressionParseMode::Normal {
						Ok(Expression(ExpressionType::Function(func.0), loc))
					} else {
						Err(
							Diagnostic::new(Level::Error, "a function expression is not allowed in this context")
								.add_label(Label::primary("unexpected expression", loc)),
						)
					}
				}),
				infix: None,
			}),
			TokenType::Return => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					let tok = next!(p);
					let range;
					let value = p.parse_expression(mode);
					let value = if let Ok(expr) = value {
						range = merge_range!(tok.1, &expr.1.range);
						Some(Box::new(expr))
					} else {
						range = tok.1;
						None
					};

					Ok(Expression(ExpressionType::Return(value), p.loc(range)))
				}),
				infix: None,
			}),
			TokenType::Break => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					let tok = next!(p);
					let range;
					let value = p.parse_expression(mode);
					let value = if let Ok(expr) = value {
						range = merge_range!(tok.1, &expr.1.range);
						Some(Box::new(expr))
					} else {
						range = tok.1;
						None
					};

					Ok(Expression(ExpressionType::Break(value), p.loc(range)))
				}),
				infix: None,
			}),
			TokenType::Switch => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					let tok = next!(p);
					p.struct_literal_allowed = false;
					let on = p.parse_expression(mode)?;
					p.struct_literal_allowed = true;
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
							p.loc(merge_range!(tok.1, next.1)),
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
							{
								let temp = expect!(p, TokenType::RightBrace, "expected `}`");
								p.loc(merge_range!(tok.1, temp))
							},
						))
					}
				}),
				infix: None,
			}),
			TokenType::Use => Some(&ParseRule {
				prefix: Some(&|p, mode| {
					let range = next!(p).1;
					if mode == ExpressionParseMode::Template {
						let path = p.parse_path()?;
						let range = merge_range!(range, path.1.range.clone());
						let mut usee = Use {
							template: UseTarget { path, resolved: None },
							args: Vec::new(),
						};
						peek!(p, TokenType::Semicolon, if {
							return Ok(Expression(ExpressionType::Behavior(BehaviorExpression::Use(usee)), p.loc(range)))
						});

						let args = p.parse_values(ExpressionParseMode::Normal)?;
						usee.args = args.0;

						Ok(Expression(
							ExpressionType::Behavior(BehaviorExpression::Use(usee)),
							p.loc(merge_range!(range, args.1.range)),
						))
					} else {
						Err(Diagnostic::new(
							Level::Error,
							"a use expression is not allowed outside behavior expressions",
						)
						.add_label(Label::primary("unexpected expression", p.loc(range))))
					}
				}),
				infix: None,
			}),
			_ => None,
		}
	}
}

#[derive(Clone, Copy, PartialEq)]
enum ExpressionParseMode {
	Normal,
	RPNCode,
	Template,
}

struct ParseRule<'a, 'b>
where
	'a: 'b,
{
	prefix: Option<&'b dyn Fn(&mut Parser<'a, 'b>, ExpressionParseMode) -> Result<Expression<'a>, Diagnostic>>,
	infix: Option<(
		&'b dyn Fn(&mut Parser<'a, 'b>, Expression<'a>, ExpressionParseMode) -> Result<Expression<'a>, Diagnostic>,
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

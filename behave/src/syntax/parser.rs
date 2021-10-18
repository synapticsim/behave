use crate::{
	diagnostic::{Diagnostic, Diagnostics, Label, Level},
	syntax::{
		ast::{
			Arm,
			BehExprKind,
			BinOp,
			BinOpKind,
			Block,
			Expr,
			ExprField,
			ExprKind,
			FnDecl,
			ImplKind,
			ImportTree,
			ImportTreeKind,
			Item,
			ItemKind,
			Loc,
			Local,
			Mod,
			Param,
			Pat,
			PatKind,
			Path,
			RPNAccess,
			RPNType,
			RetTy,
			Stmt,
			StmtKind,
			StructExpr,
			Ty,
			TyKind,
			UnOp,
			UnOpKind,
			Variant,
			AST,
		},
		lexer::Lexer,
		token::{Delimiter, Keyword, LiteralKind, Op, Span, Token, TokenKind},
	},
	SourceFile,
};

pub struct Parser<'a, 'b, F>
where
	F: FnMut(&[&str]) -> Option<&'a SourceFile>,
{
	pub(super) diagnostics: &'b mut Diagnostics,
	pub(super) lexer: Lexer<'a>,
	pub(super) peeked: Option<Option<Token<'a>>>,
	pub(super) file: &'a str,
	pub(super) path: &'b [&'a str],
	pub(super) loader: &'b mut F,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ExprMode {
	Behavior,
	RPN,
	Normal,
}

impl<'a, 'b, F> Parser<'a, 'b, F>
where
	F: FnMut(&[&str]) -> Option<&'a SourceFile>,
{
	pub fn new(file: &'a SourceFile, path: &'b [&'a str], loader: &'b mut F, diagnostics: &'b mut Diagnostics) -> Self {
		Self {
			diagnostics,
			lexer: Lexer::new(&file.contents),
			peeked: None,
			file: &file.path,
			path,
			loader,
		}
	}

	pub fn main(mut self) -> AST<'a> {
		let mut lods = None;
		let mut behavior = None;
		let mut items = Vec::new();
		let mut error = false;

		while let Some(tok) = self.peek() {
			match tok.kind {
				TokenKind::Keyword(Keyword::Lods) => {
					self.next();
					self.lods()
						.map(|(exprs, loc)| lods = Some((exprs, self.loc(tok.span) + loc)));
				},
				TokenKind::Keyword(Keyword::Behavior) => {
					self.next();
					let block = self.block(ExprMode::Behavior);
					let loc = block.loc;
					behavior = Some((block, loc + tok.span))
				},
				_ => {
					if let Some(item) = self.item(error) {
						error = false;
						items.push(item);
					} else {
						error = true;
						self.next();
					}
				},
			}
		}

		AST {
			lods: match lods {
				Some(lods) => lods,
				None => {
					self.diagnostics.add(
						Diagnostic::new(Level::Error, "`lods` not present in main file").add_label(Label::primary(
							"consider adding a `lods` element",
							self.loc((0..1).into()).into(),
						)),
					);
					(Vec::new(), self.loc(Default::default()))
				},
			},
			behavior,
			items,
		}
	}

	pub fn items(mut self) -> Vec<Item<'a>> {
		let mut items = Vec::new();
		let mut error = false;

		while let Some(_) = self.peek() {
			if let Some(item) = self.item(error) {
				error = false;
				items.push(item);
			} else {
				error = true;
				self.next();
			}
		}

		items
	}

	fn lods(&mut self) -> Option<(Vec<(Expr<'a>, Expr<'a>)>, Loc<'a>)> {
		self.delim_list(TokenKind::Comma, Delimiter::Brace, |p| {
			let size = p.expr_must(ExprMode::Normal, true);
			expect!(p, TokenKind::Arrow, err = "expected `->`");
			let gltf = p.expr_must(ExprMode::Normal, true);
			let span = size.loc + gltf.loc;
			Some(((size, gltf), span))
		})
	}

	fn expr(&mut self, mode: ExprMode, struct_literal: bool) -> Option<Expr<'a>> {
		self.precedence(precedence::ASSIGNMENT, mode, struct_literal)
	}

	fn expr_must(&mut self, mode: ExprMode, struct_literal: bool) -> Expr<'a> {
		self.precedence_must(precedence::ASSIGNMENT, mode, struct_literal)
	}

	fn precedence(&mut self, precedence: u8, mode: ExprMode, struct_literal: bool) -> Option<Expr<'a>> {
		let tok = if let Some(tok) = self.peek() { tok } else { return None };
		let mut expr = if let Some(prefix) = Self::prefix(tok, mode) {
			prefix(self, mode, struct_literal)
		} else {
			return None;
		};

		loop {
			let tok = if let Some(next) = self.peek() {
				next
			} else {
				break;
			};

			if let Some(infix) = Self::infix(tok) {
				if precedence > infix.1 {
					break;
				}

				let loc = expr.loc;
				expr = infix.0(self, expr, mode, struct_literal);
				expr.loc += loc;
			} else {
				break;
			}
		}

		Some(expr)
	}

	fn precedence_must(&mut self, precedence: u8, mode: ExprMode, struct_literal: bool) -> Expr<'a> {
		if let Some(expr) = self.precedence(precedence, mode, struct_literal) {
			expr
		} else {
			if let Some(tok) = self.peek() {
				let loc = self.loc(tok.span);
				self.diagnostics.add(
					Diagnostic::new(Level::Error, "expected expression").add_label(Label::primary("", loc.into())),
				);
				Expr {
					node: ExprKind::Err,
					loc,
				}
			} else {
				self.diagnostics.add(Diagnostic::new(
					Level::Error,
					format!("unexpected end of file `{}`", self.file),
				));
				Expr {
					node: ExprKind::Err,
					loc: self.loc(Default::default()),
				}
			}
		}
	}

	fn prefix(tok: Token, mode: ExprMode) -> Option<fn(&mut Self, mode: ExprMode, struct_literal: bool) -> Expr<'a>> {
		Some(match tok.kind {
			TokenKind::None => |p, _, _| {
				let span = p.next().expect("tried to parse none at <eof>").span;
				Expr {
					node: ExprKind::None,
					loc: p.loc(span),
				}
			},
			TokenKind::Literal(_) => |p, mode, _| {
				let lit = p.literal();
				let expr = Expr {
					loc: lit.loc,
					node: ExprKind::Lit(lit),
				};
				if mode != ExprMode::Behavior {
					expr
				} else {
					p.diagnostics.add(
						Diagnostic::new(Level::Error, "expected behavior expression")
							.add_label(Label::primary("found literal", expr.loc.into())),
					);
					Expr {
						node: ExprKind::Err,
						loc: expr.loc,
					}
				}
			},
			TokenKind::OpenDelim(Delimiter::Brace) => |p, mode, _| {
				let block = p.block(mode);
				Expr {
					loc: block.loc,
					node: ExprKind::Block(block),
				}
			},
			TokenKind::Return => |p, mode, struct_allowed| {
				let tok = p.next().unwrap();
				if mode == ExprMode::Behavior {
					p.diagnostics.add(
						Diagnostic::new(Level::Error, "expected behavior expression")
							.add_label(Label::primary("found return", p.loc(tok.span).into())),
					);
					Expr {
						node: ExprKind::Err,
						loc: p.loc(tok.span),
					}
				} else {
					let mut loc = p.loc(tok.span);
					let value = p.expr(mode, struct_allowed).map(|expr| {
						loc += expr.loc;
						Box::new(expr)
					});

					Expr {
						node: ExprKind::Ret(value),
						loc,
					}
				}
			},
			TokenKind::Break => |p, _, _| {
				let tok = p.next().unwrap();
				Expr {
					node: ExprKind::Break,
					loc: p.loc(tok.span),
				}
			},
			TokenKind::Continue => |p, _, _| {
				let tok = p.next().unwrap();
				Expr {
					node: ExprKind::Continue,
					loc: p.loc(tok.span),
				}
			},
			TokenKind::Op(Op::Not) | TokenKind::Op(Op::Minus) => Self::unary,
			TokenKind::OpenDelim(Delimiter::Paren) => Self::paren,
			TokenKind::OpenDelim(Delimiter::Bracket) => Self::array_or_map,
			TokenKind::If => Self::ifs,
			TokenKind::While => Self::whiles,
			TokenKind::For => Self::fors,
			TokenKind::Switch => Self::switch,
			TokenKind::Ref => |p, mode, struct_literal| {
				let tok = p.next().expect("tried to parse ref at <eof>");
				let expr = p.precedence_must(precedence::UNARY, mode, struct_literal);
				let expr = Expr {
					loc: expr.loc + tok.span,
					node: ExprKind::Ref(Box::new(expr)),
				};

				if mode == ExprMode::Behavior {
					p.diagnostics.add(
						Diagnostic::new(Level::Error, "a ref expression is not allowed in this context")
							.add_label(Label::primary("unexpected expression", expr.loc.into())),
					);
				}

				expr
			},
			TokenKind::Keyword(Keyword::Function) => |p, _, s| p.closure(ExprMode::Normal, s),
			TokenKind::Ident if tok.data == "rpn" => |p, _, s| {
				let span = p.next().expect("tried to parse closure at <eof>").span;
				let mut expr = p.closure(ExprMode::RPN, s);
				expr.loc += span;
				if let ExprKind::Func(decl, block) = expr.node {
					expr.node = ExprKind::RPNFunc(decl, block)
				} else {
					unreachable!()
				}
				expr
			},
			TokenKind::Ident if mode == ExprMode::Behavior && tok.data == "component" => |p, _, _| {
				let loc = p.next().expect("tried to parse component at <eof>").span;
				let mut loc = p.loc(loc);
				let name = p.expr_must(ExprMode::Normal, false);
				loc += name.loc;
				let node = if let Some(tok) = p.peek() {
					match tok.kind {
						TokenKind::Ident if tok.data == "on" => {
							p.next();
							let expr = p.expr_must(ExprMode::Normal, false);
							loc += expr.loc;
							Some(expr)
						},
						_ => None,
					}
				} else {
					None
				};
				let block = p.block(ExprMode::Behavior);
				loc += block.loc;

				Expr {
					node: ExprKind::BehExpr(Box::new(BehExprKind::Component(name, node, block))),
					loc,
				}
			},
			TokenKind::Ident if mode == ExprMode::Behavior && tok.data == "animation" => |p, _, _| {
				let loc = p.next().expect("tried to parse animation at <eof>").span;
				let mut loc = p.loc(loc);
				let name = p.expr_must(ExprMode::Normal, false);
				loc += name.loc;
				Expr {
					node: ExprKind::BehExpr(Box::new(BehExprKind::Animation(
						name,
						if let Some(fields) = p.struct_expr() {
							loc += fields.1;
							fields.0
						} else {
							p.diagnostics.add(
								Diagnostic::new(Level::Error, "expected animation arguments")
									.add_label(Label::primary("", loc.into())),
							);
							Vec::new()
						},
					))),
					loc,
				}
			},
			TokenKind::Ident if mode == ExprMode::Behavior && tok.data == "visibility" => |p, _, _| {
				let loc = p.next().expect("tried to parse visibility at <eof>").span;
				expect!(p, TokenKind::Op(Op::Eq), err = "expected `=`");
				let expr = p.expr_must(ExprMode::Normal, true);
				let loc = expr.loc + loc;
				Expr {
					node: ExprKind::BehExpr(Box::new(BehExprKind::Visibility(expr))),
					loc,
				}
			},
			TokenKind::Ident if mode == ExprMode::Behavior && tok.data == "emissive" => |p, _, _| {
				let loc = p.next().expect("tried to parse emissive at <eof>").span;
				expect!(p, TokenKind::Op(Op::Eq), err = "expected `=`");
				let expr = p.expr_must(ExprMode::Normal, true);
				let loc = expr.loc + loc;
				Expr {
					node: ExprKind::BehExpr(Box::new(BehExprKind::Emissive(expr))),
					loc,
				}
			},
			TokenKind::Ident if mode == ExprMode::Behavior && tok.data == "interaction" => |p, _, _| {
				let loc = p.next().expect("tried to parse interaction at <eof>").span;
				if let Some(fields) = p.struct_expr() {
					Expr {
						node: ExprKind::BehExpr(Box::new(BehExprKind::Interaction(fields.0))),
						loc: fields.1 + loc,
					}
				} else {
					let loc = p.loc(loc);
					p.diagnostics.add(
						Diagnostic::new(Level::Error, "expected interaction arguments")
							.add_label(Label::primary("", loc.into())),
					);
					Expr {
						node: ExprKind::Err,
						loc,
					}
				}
			},
			TokenKind::Ident if mode == ExprMode::Behavior && tok.data == "events" => |p, _, _| {
				let loc = p.next().expect("tried to parse events at <eof>").span;
				let mut loc = p.loc(loc);
				if let Some(tok) = p.peek() {
					match tok.kind {
						TokenKind::Ident if tok.data == "on" => {
							p.next();
						},
						kind => {
							p.diagnostics.add(
								Diagnostic::new(Level::Error, "expected `on`")
									.add_label(Label::primary(format!("found `{}`", kind), p.loc(tok.span).into())),
							);
						},
					}
				}
				let on = p.precedence_must(precedence::OR, ExprMode::Normal, true);
				loc += on.loc;
				expect!(p, TokenKind::Op(Op::Eq), err = "expected `=`");
				let events = p.expr_must(ExprMode::Normal, true);
				loc += events.loc;

				Expr {
					node: ExprKind::BehExpr(Box::new(BehExprKind::Events(on, events))),
					loc,
				}
			},
			TokenKind::Ident if mode == ExprMode::Behavior && tok.data == "update" => |p, _, _| {
				let loc = p.next().expect("tried to parse update at <eof>").span;
				if let Some(fields) = p.struct_expr() {
					Expr {
						node: ExprKind::BehExpr(Box::new(BehExprKind::Update(fields.0))),
						loc: fields.1 + loc,
					}
				} else {
					let loc = p.loc(loc);
					p.diagnostics.add(
						Diagnostic::new(Level::Error, "expected update arguments")
							.add_label(Label::primary("", loc.into())),
					);
					Expr {
						node: ExprKind::Err,
						loc,
					}
				}
			},
			TokenKind::Ident if mode == ExprMode::Behavior && tok.data == "inputevent" => |p, _, _| {
				let loc = p.next().expect("tried to parse inputevent at <eof>").span;
				let mut loc = p.loc(loc);
				let name = p.expr_must(ExprMode::Normal, false);
				loc += name.loc;
				Expr {
					node: ExprKind::BehExpr(Box::new(BehExprKind::InputEvent(
						name,
						if let Some(fields) = p.struct_expr() {
							loc += fields.1;
							fields.0
						} else {
							p.diagnostics.add(
								Diagnostic::new(Level::Error, "expected inputevent arguments")
									.add_label(Label::primary("", loc.into())),
							);
							Vec::new()
						},
					))),
					loc,
				}
			},
			TokenKind::Ident => |p, mode, literal| {
				let path = if let Some(path) = p.path() {
					path
				} else {
					return Expr {
						node: ExprKind::Err,
						loc: p.loc(Default::default()),
					};
				};

				let expr = if literal
					&& p.peek()
						.map(|tok| tok.kind == TokenKind::OpenDelim(Delimiter::Brace))
						.unwrap_or(false)
				{
					if let Some(fields) = p.struct_expr() {
						Expr {
							loc: path.loc + fields.1,
							node: ExprKind::Struct(StructExpr { path, fields: fields.0 }),
						}
					} else {
						Expr {
							loc: path.loc,
							node: ExprKind::Path(path),
						}
					}
				} else {
					Expr {
						loc: path.loc,
						node: ExprKind::Path(path),
					}
				};

				if mode == ExprMode::Behavior {
					p.diagnostics.add(
						Diagnostic::new(Level::Error, "expected behavior expression")
							.add_label(Label::primary("", expr.loc.into())),
					);
				}

				expr
			},
			TokenKind::Keyword(Keyword::Use) => |p, mode, _| {
				let loc = p.next().expect("tried to parse use at <eof>").span;
				let mut loc = p.loc(loc);
				let path = if let Some(path) = p.path() {
					path
				} else {
					p.diagnostics.add(
						Diagnostic::new(Level::Error, "expected template use path")
							.add_label(Label::primary("", loc.into())),
					);
					return Expr {
						node: ExprKind::Err,
						loc,
					};
				};
				loc += path.loc;
				let fields = if let Some(fields) = p.struct_expr() {
					fields
				} else {
					p.diagnostics.add(
						Diagnostic::new(Level::Error, "expected template arguments")
							.add_label(Label::primary("", loc.into())),
					);
					return Expr {
						node: ExprKind::Err,
						loc,
					};
				};
				loc += fields.1;

				if mode == ExprMode::Behavior {
					Expr {
						node: ExprKind::BehExpr(Box::new(BehExprKind::Use(StructExpr { path, fields: fields.0 }))),
						loc,
					}
				} else {
					p.diagnostics.add(
						Diagnostic::new(Level::Error, "use expressions are not allowed in this context")
							.add_label(Label::primary("", loc.into())),
					);
					Expr {
						node: ExprKind::Err,
						loc,
					}
				}
			},
			_ => return None,
		})
	}

	fn infix(
		tok: Token,
	) -> Option<(
		fn(&mut Self, left: Expr<'a>, mode: ExprMode, struct_literal: bool) -> Expr<'a>,
		u8,
	)> {
		Some(match tok.kind {
			TokenKind::Op(op) => match op {
				Op::Plus => (
					|p, left, mode, literal| p.binary(BinOpKind::Add, precedence::TERM, left, mode, literal),
					precedence::TERM,
				),
				Op::Minus => (
					|p, left, mode, literal| p.binary(BinOpKind::Sub, precedence::TERM, left, mode, literal),
					precedence::TERM,
				),
				Op::Star => (
					|p, left, mode, literal| p.binary(BinOpKind::Mul, precedence::FACTOR, left, mode, literal),
					precedence::FACTOR,
				),
				Op::Slash => (
					|p, left, mode, literal| p.binary(BinOpKind::Div, precedence::FACTOR, left, mode, literal),
					precedence::FACTOR,
				),
				Op::Gt => (
					|p, left, mode, literal| p.binary(BinOpKind::Gt, precedence::COMPARISON, left, mode, literal),
					precedence::COMPARISON,
				),
				Op::Lt => (
					|p, left, mode, literal| p.binary(BinOpKind::Lt, precedence::COMPARISON, left, mode, literal),
					precedence::COMPARISON,
				),
				Op::Percent => (
					|p, left, mode, literal| p.binary(BinOpKind::Rem, precedence::FACTOR, left, mode, literal),
					precedence::FACTOR,
				),
				Op::Eq => (
					|p, left, mode, literal| {
						let loc = p.next().expect("tried to parse assignment at <eof>").span;
						let value = p.expr_must(mode, literal);
						Expr {
							loc: left.loc + value.loc,
							node: ExprKind::Assign(Box::new((left, p.loc(loc), value))),
						}
					},
					precedence::ASSIGNMENT,
				),
				Op::Not => return None,
			},
			TokenKind::OpEq(op) => match op {
				Op::Eq => (
					|p, left, mode, literal| p.binary(BinOpKind::Eq, precedence::EQUALITY, left, mode, literal),
					precedence::EQUALITY,
				),
				Op::Not => (
					|p, left, mode, literal| p.binary(BinOpKind::Ne, precedence::EQUALITY, left, mode, literal),
					precedence::EQUALITY,
				),
				Op::Gt => (
					|p, left, mode, literal| p.binary(BinOpKind::Ge, precedence::COMPARISON, left, mode, literal),
					precedence::COMPARISON,
				),
				Op::Lt => (
					|p, left, mode, literal| p.binary(BinOpKind::Le, precedence::COMPARISON, left, mode, literal),
					precedence::COMPARISON,
				),
				Op::Plus => (
					|p, left, mode, literal| p.op_assignment(BinOpKind::Add, left, mode, literal),
					precedence::ASSIGNMENT,
				),
				Op::Minus => (
					|p, left, mode, literal| p.op_assignment(BinOpKind::Sub, left, mode, literal),
					precedence::ASSIGNMENT,
				),
				Op::Star => (
					|p, left, mode, literal| p.op_assignment(BinOpKind::Mul, left, mode, literal),
					precedence::ASSIGNMENT,
				),
				Op::Slash => (
					|p, left, mode, literal| p.op_assignment(BinOpKind::Div, left, mode, literal),
					precedence::ASSIGNMENT,
				),
				Op::Percent => (
					|p, left, mode, literal| p.op_assignment(BinOpKind::Rem, left, mode, literal),
					precedence::ASSIGNMENT,
				),
			},
			TokenKind::And => (
				|p, left, mode, literal| p.binary(BinOpKind::And, precedence::AND, left, mode, literal),
				precedence::AND,
			),
			TokenKind::Or => (
				|p, left, mode, literal| p.binary(BinOpKind::Or, precedence::OR, left, mode, literal),
				precedence::OR,
			),
			TokenKind::OpenDelim(Delimiter::Paren) => (
				|p, left, mode, _| {
					let exprs = if let Some(exprs) = p.delim_list(TokenKind::Comma, Delimiter::Paren, |p| {
						let expr = p.expr_must(mode, true);
						let loc = expr.loc;
						Some((expr, loc))
					}) {
						exprs
					} else {
						return Expr {
							node: ExprKind::Err,
							loc: p.loc(Default::default()),
						};
					};
					if mode != ExprMode::Behavior {
						Expr {
							loc: left.loc + exprs.1,
							node: ExprKind::Call(Box::new(left), exprs.0),
						}
					} else {
						let loc = left.loc + exprs.1;
						p.diagnostics.add(
							Diagnostic::new(Level::Error, "expected behavior expression")
								.add_label(Label::primary("", loc.into())),
						);
						Expr {
							node: ExprKind::Err,
							loc,
						}
					}
				},
				precedence::CALL,
			),
			TokenKind::As => (
				|p, left, mode, _| {
					p.next();
					let ty = p.ty();

					if mode != ExprMode::Behavior {
						Expr {
							loc: left.loc + ty.loc,
							node: ExprKind::Cast(Box::new(left), ty),
						}
					} else {
						let loc = left.loc + ty.loc;
						p.diagnostics.add(
							Diagnostic::new(Level::Error, "expected behavior expression")
								.add_label(Label::primary("", loc.into())),
						);
						Expr {
							node: ExprKind::Err,
							loc,
						}
					}
				},
				precedence::CALL,
			),
			TokenKind::Is => (
				|p, left, mode, _| {
					p.next();
					let ty = p.ty();

					if mode != ExprMode::Behavior {
						Expr {
							loc: left.loc + ty.loc,
							node: ExprKind::Is(Box::new(left), ty),
						}
					} else {
						let loc = left.loc + ty.loc;
						p.diagnostics.add(
							Diagnostic::new(Level::Error, "expected behavior expression")
								.add_label(Label::primary("", loc.into())),
						);
						Expr {
							node: ExprKind::Err,
							loc,
						}
					}
				},
				precedence::CALL,
			),
			TokenKind::Dot => (
				|p, left, mode, _| {
					p.next();
					expect!(p, TokenKind::Ident, err = "expected field name").map_or(
						Expr {
							node: ExprKind::Err,
							loc: left.loc,
						},
						|ident| {
							if mode != ExprMode::Behavior {
								Expr {
									loc: left.loc + ident.span,
									node: ExprKind::Field(Box::new(left), p.ident(ident)),
								}
							} else {
								let loc = left.loc + ident.span;
								p.diagnostics.add(
									Diagnostic::new(Level::Error, "expected behavior expression")
										.add_label(Label::primary("", loc.into())),
								);
								Expr {
									node: ExprKind::Err,
									loc,
								}
							}
						},
					)
				},
				precedence::CALL,
			),
			TokenKind::OpenDelim(Delimiter::Bracket) => (
				|p, left, mode, _| {
					p.next();
					let expr = p.expr_must(mode, true);
					let end = expect!(p, TokenKind::CloseDelim(Delimiter::Bracket), err = "expected `]`");
					if mode != ExprMode::Behavior {
						Expr {
							loc: left.loc + expr.loc + end.map(|tok| tok.span).unwrap_or(Default::default()),
							node: ExprKind::Index(Box::new((left, expr))),
						}
					} else {
						let loc = left.loc + expr.loc + end.map(|tok| tok.span).unwrap_or(Default::default());
						p.diagnostics.add(
							Diagnostic::new(Level::Error, "expected behavior expression")
								.add_label(Label::primary("", loc.into())),
						);
						Expr {
							node: ExprKind::Err,
							loc,
						}
					}
				},
				precedence::CALL,
			),
			_ => return None,
		})
	}

	fn unary(&mut self, mode: ExprMode, struct_literal: bool) -> Expr<'a> {
		let op = self.next().unwrap();
		let expr = self.precedence_must(precedence::UNARY, mode, struct_literal);
		let loc = expr.loc + op.span;
		if mode != ExprMode::Behavior {
			Expr {
				node: ExprKind::Unary(
					UnOp {
						node: match op.kind {
							TokenKind::Op(Op::Not) => UnOpKind::Not,
							TokenKind::Op(Op::Minus) => UnOpKind::Neg,
							_ => unreachable!(),
						},
						loc: self.loc(op.span),
					},
					Box::new(expr),
				),
				loc,
			}
		} else {
			self.diagnostics.add(
				Diagnostic::new(Level::Error, "a unary expression is not allowed in this context")
					.add_label(Label::primary("unexpected expression", loc.into())),
			);
			Expr {
				node: ExprKind::Err,
				loc,
			}
		}
	}

	fn binary(&mut self, op: BinOpKind, prec: u8, left: Expr<'a>, mode: ExprMode, struct_literal: bool) -> Expr<'a> {
		let span = self.next().expect("tried to parse binary expr at <eof>").span;
		let right = self.precedence_must(prec + 1, mode, struct_literal);
		let loc = left.loc + right.loc;

		if mode == ExprMode::Behavior {
			self.diagnostics.add(
				Diagnostic::new(Level::Error, "expected behavior expression").add_label(Label::primary("", loc.into())),
			);
		}

		Expr {
			node: ExprKind::Binary(Box::new((
				left,
				BinOp {
					node: op,
					loc: self.loc(span),
				},
				right,
			))),
			loc,
		}
	}

	fn op_assignment(&mut self, op: BinOpKind, left: Expr<'a>, mode: ExprMode, struct_literal: bool) -> Expr<'a> {
		let loc = self.next().expect("tried to parse assignment at <eof>").span;
		let value = self.expr_must(mode, struct_literal);
		Expr {
			loc: left.loc + value.loc,
			node: ExprKind::AssignOp(Box::new((
				left,
				BinOp {
					node: op,
					loc: self.loc(loc),
				},
				value,
			))),
		}
	}

	fn paren(&mut self, mode: ExprMode, _: bool) -> Expr<'a> {
		let loc = self.next().expect("tried to parse tuple/RPN var/group at <eof>").span;
		let mut loc = self.loc(loc);
		if let Some(tok) = peek!(self, TokenKind::CloseDelim(Delimiter::Paren)) {
			return Expr {
				node: ExprKind::Tuple(vec![]),
				loc: loc + tok.span,
			};
		}
		let expr = self.expr_must(mode, true);
		loc += expr.loc;
		let expr = if let Some(tok) = self.next() {
			loc += tok.span;
			match tok.kind {
				TokenKind::CloseDelim(Delimiter::Paren) => Expr {
					node: ExprKind::Paren(Box::new(expr)),
					loc,
				},
				TokenKind::Comma => self.tuple(expr, mode, loc),
				TokenKind::Colon if mode == ExprMode::RPN => self.rpn_var(expr, loc),
				_ => {
					self.diagnostics.add(
						Diagnostic::new(Level::Error, "unexpected token")
							.add_label(Label::primary("", self.loc(tok.span).into())),
					);
					Expr {
						node: ExprKind::Paren(Box::new(expr)),
						loc,
					}
				},
			}
		} else {
			self.diagnostics.add(Diagnostic::new(
				Level::Error,
				format!("unexpected end of file '{}'", self.file),
			));
			Expr {
				node: ExprKind::Paren(Box::new(expr)),
				loc,
			}
		};

		if mode == ExprMode::Behavior {
			self.diagnostics.add(
				Diagnostic::new(Level::Error, "expected behavior expression")
					.add_label(Label::primary("", expr.loc.into())),
			);
			expr
		} else {
			expr
		}
	}

	fn array_or_map(&mut self, mode: ExprMode, _: bool) -> Expr<'a> {
		let loc = self.next().expect("tried to parse array at <eof>").span;
		let mut loc = self.loc(loc);
		if let Some(tok) = peek!(self, TokenKind::CloseDelim(Delimiter::Bracket)) {
			loc += tok.span;
			Expr {
				node: ExprKind::EmptyMapOrArray,
				loc,
			}
		} else {
			let key = self.expr_must(mode, true);
			if peek!(self, TokenKind::Colon).is_some() {
				let value = self.expr_must(mode, true);

				let mut items = vec![(key, value)];

				loop {
					if let Some(tok) = peek!(self, TokenKind::CloseDelim(Delimiter::Bracket)) {
						loc += tok.span;
						break;
					} else if peek!(self, TokenKind::Comma).is_some() {
						if let Some(tok) = peek!(self, TokenKind::CloseDelim(Delimiter::Bracket)) {
							loc += tok.span;
							break;
						} else {
							let key = self.expr_must(mode, true);
							loc += key.loc;
							expect!(self, TokenKind::Colon, err = "expected `:` after key in map expression");
							let value = self.expr_must(mode, true);
							loc += value.loc;
							items.push((key, value));
						}
					} else {
						let key = self.expr_must(mode, true);
						loc += key.loc;
						expect!(self, TokenKind::Colon, err = "expected `:` after key in map expression");
						let value = self.expr_must(mode, true);
						loc += value.loc;
						items.push((key, value));
					}
				}

				Expr {
					node: ExprKind::Map(items),
					loc,
				}
			} else {
				let mut items = vec![key];

				loop {
					if let Some(tok) = peek!(self, TokenKind::CloseDelim(Delimiter::Bracket)) {
						loc += tok.span;
						break;
					} else if peek!(self, TokenKind::Comma).is_some() {
						if let Some(tok) = peek!(self, TokenKind::CloseDelim(Delimiter::Bracket)) {
							loc += tok.span;
							break;
						} else {
							let value = self.expr_must(mode, true);
							loc += value.loc;
							items.push(value);
						}
					} else {
						let value = self.expr_must(mode, true);
						loc += value.loc;
						items.push(value);
					}
				}

				Expr {
					node: ExprKind::Array(items),
					loc,
				}
			}
		}
	}

	fn tuple(&mut self, first: Expr<'a>, mode: ExprMode, mut loc: Loc<'a>) -> Expr<'a> {
		let mut items = vec![first];
		let insert = |p: &mut Self, items: &mut Vec<Expr<'a>>, loc: &mut Loc<'a>| {
			let val = p.expr_must(mode, true);
			*loc += val.loc;
			items.push(val);
		};

		loop {
			if let Some(tok) = peek!(self, TokenKind::CloseDelim(Delimiter::Paren)) {
				loc += tok.span;
				break;
			} else if peek!(self, TokenKind::Comma).is_some() {
				if let Some(tok) = peek!(self, TokenKind::CloseDelim(Delimiter::Paren)) {
					loc += tok.span;
					break;
				} else {
					insert(self, &mut items, &mut loc);
				}
			} else {
				insert(self, &mut items, &mut loc);
			}
		}

		Expr {
			node: ExprKind::Tuple(items),
			loc,
		}
	}

	fn rpn_var(&mut self, ty: Expr<'a>, loc: Loc<'a>) -> Expr<'a> {
		let mut loc = ty.loc + loc;
		let rpn_ty = if let ExprKind::Path(path) = ty.node {
			if path.node.len() == 1 {
				let ty = path.node.into_iter().next().unwrap();
				match ty.node {
					"A" => RPNType::Sim,
					"B" => RPNType::Input,
					"E" => RPNType::Env,
					"G" => RPNType::Gauge,
					"H" => RPNType::HTML,
					"I" => RPNType::Instrument,
					"K" => {
						let two_arg = if let Some(tok) = self.peek() {
							if let TokenKind::Literal(LiteralKind::Int) = tok.kind {
								if tok.data == "2" {
									expect!(self, TokenKind::Colon, err = "expected `colon` after `K:2`");
									true
								} else {
									false
								}
							} else {
								false
							}
						} else {
							false
						};
						RPNType::Key { two_arg }
					},
					"L" => RPNType::Local,
					"M" => RPNType::Mouse,
					"O" => RPNType::Component,
					"R" => {
						let legacy = if let Some(tok) = next!(self) {
							if let TokenKind::Literal(LiteralKind::Int) = tok.kind {
								let b = if tok.data == "1" {
									true
								} else if tok.data == "2" {
									false
								} else {
									self.diagnostics.add(
										Diagnostic::new(Level::Error, "expected resource variable type")
											.add_label(Label::primary("", self.loc(tok.span).into())),
									);
									false
								};
								expect!(self, TokenKind::Colon, err = "expected `colon` after `K:2`");
								b
							} else {
								self.diagnostics.add(
									Diagnostic::new(Level::Error, "expected resource variable type")
										.add_label(Label::primary("", self.loc(tok.span).into())),
								);
								false
							}
						} else {
							false
						};
						RPNType::Resource { legacy }
					},
					"W" => RPNType::Wwise,
					"Z" => RPNType::Custom,
					_ => {
						self.diagnostics.add(
							Diagnostic::new(Level::Error, "expected RPN variable type")
								.add_label(Label::primary("", ty.loc.into())),
						);
						RPNType::Local
					},
				}
			} else {
				self.diagnostics.add(
					Diagnostic::new(Level::Error, "expected RPN variable type")
						.add_label(Label::primary("", ty.loc.into())),
				);
				RPNType::Local
			}
		} else {
			self.diagnostics.add(
				Diagnostic::new(Level::Error, "expected RPN variable type")
					.add_label(Label::primary("", ty.loc.into())),
			);
			RPNType::Local
		};
		let var = self.expr_must(ExprMode::RPN, false);
		loc += var.loc;
		let unit = if peek!(self, TokenKind::Comma).is_some() {
			let ty = self.ty();
			loc += ty.loc;
			Some(ty)
		} else {
			None
		};

		expect!(self, TokenKind::CloseDelim(Delimiter::Paren), err = "expected `)`");

		Expr {
			node: ExprKind::RPNAccess(Box::new(RPNAccess { rpn_ty, var, unit })),
			loc,
		}
	}

	fn ifs(&mut self, mode: ExprMode, _: bool) -> Expr<'a> {
		let loc = self.next().expect("tried to parse if at <eof>").span;
		let mut loc = self.loc(loc);
		let expr = self.expr_must(if mode != ExprMode::RPN { ExprMode::Normal } else { mode }, false);
		loc += expr.loc;
		let block = self.block(mode);

		let els = if peek!(self, TokenKind::Else).is_some() {
			let expr = self.expr_must(mode, false);
			loc += expr.loc;

			if !matches!(&expr.node, ExprKind::If(..) | ExprKind::Block(..)) {
				self.diagnostics.add(
					Diagnostic::new(Level::Error, "expected `if` or block")
						.add_label(Label::primary("", expr.loc.into())),
				)
			}

			Some(Box::new(expr))
		} else {
			None
		};

		Expr {
			node: ExprKind::If(Box::new(expr), block, els),
			loc,
		}
	}

	fn whiles(&mut self, mode: ExprMode, _: bool) -> Expr<'a> {
		let loc = self.next().expect("tried to parse while at <eof>").span;
		let mut loc = self.loc(loc);
		let expr = self.expr_must(if mode != ExprMode::RPN { ExprMode::Normal } else { mode }, false);
		loc += expr.loc;
		let block = self.block(mode);
		loc += block.loc;

		Expr {
			node: ExprKind::While(Box::new(expr), block),
			loc,
		}
	}

	fn fors(&mut self, mode: ExprMode, _: bool) -> Expr<'a> {
		let loc = self.next().expect("tried to parse for at <eof>").span;
		let mut loc = self.loc(loc);
		let pat = self.pat();
		expect!(self, TokenKind::In, err = "expected `in` after pattern");
		let expr = self.expr_must(if mode != ExprMode::RPN { ExprMode::Normal } else { mode }, false);
		loc += expr.loc;
		let block = self.block(mode);

		Expr {
			node: ExprKind::For(pat, Box::new(expr), block),
			loc,
		}
	}

	fn switch(&mut self, mode: ExprMode, _: bool) -> Expr<'a> {
		let loc = self.next().expect("tried to parse switch at <eof>").span;
		let mut loc = self.loc(loc);
		let expr = self.expr_must(if mode != ExprMode::RPN { ExprMode::Normal } else { mode }, false);
		loc += expr.loc;
		let arms = if let Some(arms) = self.delim_list(TokenKind::Comma, Delimiter::Brace, |p| {
			let value = p.expr_must(mode, true);
			expect!(p, TokenKind::Arrow, err = "expected `->` after arm value");
			let body = p.expr_must(mode, true);
			let loc = value.loc + body.loc;
			Some((Arm { value, body, loc }, loc))
		}) {
			arms
		} else {
			return Expr {
				node: ExprKind::Err,
				loc: expr.loc,
			};
		};

		Expr {
			loc: expr.loc + arms.1,
			node: ExprKind::Switch(Box::new(expr), arms.0),
		}
	}

	fn closure(&mut self, mode: ExprMode, struct_literal: bool) -> Expr<'a> {
		let loc = self.next().expect("tried to parse closure at <eof>").span;
		let mut loc = self.loc(loc);
		let params = self
			.delim_list(TokenKind::Comma, Delimiter::Paren, |p| {
				let ident = expect!(p, TokenKind::Ident, err = "expected parameter identifier")?;
				let ident = p.ident(ident);
				let mut loc = ident.loc;
				let ty = peek!(p, TokenKind::Colon).map(|_| {
					let ty = p.ty();
					loc += ty.loc;
					ty
				});
				Some((
					Param {
						ident,
						ty,
						default: None,
						loc,
					},
					loc,
				))
			})
			.unwrap_or_else(|| {
				self.diagnostics.add(
					Diagnostic::new(Level::Error, "expected parameter list in closure")
						.add_label(Label::primary("", loc.into())),
				);
				(Vec::new(), loc)
			});
		loc += params.1;
		let output = if peek!(self, TokenKind::Arrow).is_some() {
			RetTy::Ty(self.ty())
		} else {
			RetTy::Default(self.loc(Span {
				start: params.1.span.end + 1,
				end: params.1.span.end + 2,
			}))
		};
		let block = Box::new(self.expr_must(mode, struct_literal));
		loc += block.loc;

		Expr {
			node: ExprKind::Func(
				FnDecl {
					inputs: params.0,
					output,
				},
				block,
			),
			loc,
		}
	}

	fn block(&mut self, mode: ExprMode) -> Block<'a> {
		let loc = if let Some(tok) = expect!(
			self,
			TokenKind::OpenDelim(Delimiter::Brace),
			err = "expected `{` to start block"
		) {
			tok.span
		} else {
			return Block {
				node: Vec::new(),
				loc: self.loc(Default::default()),
			};
		};
		let mut loc = self.loc(loc);
		let mut block = Vec::new();
		let mut ended = false;

		let expr = |p: &mut Self, block: &mut Vec<Stmt<'a>>, loc: &mut Loc<'a>| {
			let expr = p.expr_must(mode, true);
			if let ExprKind::Err = expr.node {
				p.diagnostics.add(
					Diagnostic::new(Level::Error, "expected expression").add_label(Label::primary("", expr.loc.into())),
				)
			} else {
				block.push(if let Some(tok) = peek!(p, TokenKind::Semi) {
					*loc += expr.loc + tok.span;
					Stmt {
						loc: expr.loc + tok.span,
						node: StmtKind::Semi(expr),
					}
				} else {
					*loc += expr.loc;
					Stmt {
						loc: expr.loc,
						node: StmtKind::Expr(expr),
					}
				});
			}
		};

		while let Some(tok) = self.peek() {
			match tok.kind {
				TokenKind::CloseDelim(Delimiter::Brace) => {
					self.next();
					loc += tok.span;
					ended = true;
					break;
				},
				TokenKind::Keyword(keyword) => match keyword {
					Keyword::Import
					| Keyword::Function
					| Keyword::Template
					| Keyword::Enum
					| Keyword::Struct
					| Keyword::Impl => {
						self.item(false).map(|item| {
							loc += item.loc;
							block.push(Stmt {
								loc: item.loc,
								node: StmtKind::Item(item),
							})
						});
					},
					Keyword::Let => {
						self.next();
						loc += tok.span;
						let pat = self.pat();
						let ty = peek!(self, TokenKind::Colon).map(|_| self.ty());
						let init = peek!(self, TokenKind::Op(Op::Eq))
							.map(|_| self.expr_must(if mode == ExprMode::RPN { mode } else { ExprMode::Normal }, true));

						let semi = expect!(
							self,
							TokenKind::Semi,
							err = "expected `semicolon` after local variable declaration"
						)
						.map(|tok| tok.span)
						.unwrap_or(Default::default());

						block.push(Stmt {
							loc: self.loc(tok.span) + semi,
							node: StmtKind::Local(Local { pat, ty, init }),
						});
					},
					_ => expr(self, &mut block, &mut loc),
				},
				_ => expr(self, &mut block, &mut loc),
			}
		}

		if !ended {
			self.diagnostics.add(
				Diagnostic::new(Level::Error, "expected `}` at end of block").add_label(Label::primary("", loc.into())),
			)
		}

		Block { node: block, loc }
	}

	fn struct_expr(&mut self) -> Option<(Vec<ExprField<'a>>, Loc<'a>)> {
		self.delim_list(TokenKind::Comma, Delimiter::Brace, |p| {
			let tok = expect!(p, TokenKind::Ident, err = "expected field name")?;
			expect!(p, TokenKind::Colon, err = "expected `colon`");
			let expr = p.expr_must(ExprMode::Normal, true);
			let loc = p.loc(tok.span) + expr.loc;
			Some((
				ExprField {
					ident: p.ident(tok),
					expr,
				},
				loc,
			))
		})
	}

	fn ty(&mut self) -> Ty<'a> {
		let sum = if let Some(list) = self.list(TokenKind::Pipe, Self::ty_inner) {
			list
		} else {
			return Ty {
				node: TyKind::Tuple(Vec::new()),
				loc: self.loc(Default::default()),
			};
		};
		if sum.0.len() == 1 {
			sum.0.into_iter().next().unwrap()
		} else {
			Ty {
				node: TyKind::Sum(sum.0),
				loc: sum.1,
			}
		}
	}

	fn ty_inner(&mut self) -> Option<(Ty<'a>, Loc<'a>)> {
		let tok = self.peek()?;
		let ty = match tok.kind {
			TokenKind::OpenDelim(Delimiter::Paren) => {
				let tys = self.delim_list(TokenKind::Comma, Delimiter::Paren, |p| {
					let ty = p.ty();
					let loc = ty.loc;
					Some((ty, loc))
				})?;

				(
					Ty {
						node: TyKind::Tuple(tys.0),
						loc: tys.1,
					},
					tys.1,
				)
			},
			TokenKind::OpenDelim(Delimiter::Bracket) => {
				self.next();
				let key = self.ty();
				if peek!(self, TokenKind::Colon).is_some() {
					let value = self.ty();
					let right = expect!(
						self,
						TokenKind::CloseDelim(Delimiter::Bracket),
						err = "expected `]` after map type"
					)?;
					(
						Ty {
							node: TyKind::Map(Box::new((key, value))),
							loc: self.loc(tok.span) + right.span,
						},
						self.loc(tok.span) + right.span,
					)
				} else {
					let right = expect!(
						self,
						TokenKind::CloseDelim(Delimiter::Bracket),
						err = "expected `]` after array type"
					)?;
					(
						Ty {
							node: TyKind::Array(Box::new(key)),
							loc: self.loc(tok.span) + right.span,
						},
						self.loc(tok.span) + right.span,
					)
				}
			},
			TokenKind::Keyword(Keyword::Function) => {
				self.next();
				let arg_list = self.delim_list(TokenKind::Comma, Delimiter::Paren, |p| {
					let ty = p.ty();
					let loc = ty.loc;
					Some((ty, loc))
				})?;
				let mut loc = self.loc(tok.span) + arg_list.1;
				let ret = if peek!(self, TokenKind::Arrow).is_some() {
					let ty = self.ty();
					loc += ty.loc;
					RetTy::Ty(ty)
				} else {
					RetTy::Default(self.loc(Span {
						start: arg_list.1.span.end - 1,
						end: arg_list.1.span.end,
					}))
				};

				(
					Ty {
						node: TyKind::Func(arg_list.0, Box::new(ret)),
						loc,
					},
					loc,
				)
			},
			TokenKind::Ident if tok.data == "_" => {
				self.next();
				(
					Ty {
						node: TyKind::Infer,
						loc: self.loc(tok.span),
					},
					self.loc(tok.span),
				)
			},
			TokenKind::Ident if tok.data == "rpn" => {
				self.next();
				expect!(self, TokenKind::Keyword(Keyword::Function), err = "expected `fn`")?;
				let arg_list = self.delim_list(TokenKind::Comma, Delimiter::Paren, |p| {
					let ty = p.ty();
					let loc = ty.loc;
					Some((ty, loc))
				})?;
				let mut loc = self.loc(tok.span) + arg_list.1;
				let ret = if peek!(self, TokenKind::Arrow).is_some() {
					let ty = self.ty();
					loc += ty.loc;
					RetTy::Ty(ty)
				} else {
					RetTy::Default(self.loc(Span {
						start: arg_list.1.span.end - 1,
						end: arg_list.1.span.end,
					}))
				};

				(
					Ty {
						node: TyKind::RPNFunc(arg_list.0, Box::new(ret)),
						loc,
					},
					loc,
				)
			},
			TokenKind::Ident => {
				let path = self.path()?;
				let loc = path.loc;
				(
					Ty {
						loc,
						node: TyKind::Path(path),
					},
					loc,
				)
			},
			TokenKind::Ref => {
				let ty = self.ty();
				let loc = ty.loc + tok.span;

				(
					Ty {
						loc,
						node: TyKind::Ref(Box::new(ty)),
					},
					loc,
				)
			},
			_ => {
				self.diagnostics
					.add(Diagnostic::new(Level::Error, "expected type").add_label(Label::primary(
						format!("found `{}`", tok.kind),
						self.loc(tok.span).into(),
					)));
				return None;
			},
		};

		if let Some(tok) = peek!(self, TokenKind::Question) {
			Some((
				Ty {
					node: TyKind::Optional(Box::new(ty.0)),
					loc: ty.1 + tok.span,
				},
				ty.1 + tok.span,
			))
		} else {
			Some(ty)
		}
	}

	fn item(&mut self, error_carry: bool) -> Option<Item<'a>> {
		let err = |p: &mut Self, tok: Token| {
			if !error_carry {
				p.diagnostics.add(
					Diagnostic::new(Level::Error, "expected item")
						.add_label(Label::primary(format!("found `{}`", tok.kind), p.loc(tok.span).into())),
				);
			}
			None
		};

		let tok = self.peek()?;
		match tok.kind {
			TokenKind::Keyword(Keyword::Import) => {
				let import = self.import()?;
				let ret = Some(Item {
					node: ItemKind::Import(import.0),
					loc: import.1,
				});
				expect!(self, TokenKind::Semi, err = "expected `semicolon` after `import`");
				ret
			},
			TokenKind::Keyword(Keyword::Function) => self.func(ExprMode::Normal),
			TokenKind::Ident => {
				if tok.data == "rpn" {
					self.next();
					let next = self.peek()?;
					if next.kind == TokenKind::Keyword(Keyword::Function) {
						let item = self.func(ExprMode::RPN)?;
						Some(Item {
							loc: item.loc + tok.span,
							..item
						})
					} else {
						self.diagnostics
							.add(
								Diagnostic::new(Level::Error, "expected `fn` after `rpn`").add_label(Label::primary(
									format!("found `{}`", next.kind),
									self.loc(tok.span.clone()).into(),
								)),
							);
						None
					}
				} else {
					err(self, tok)
				}
			},
			TokenKind::Keyword(Keyword::Template) => self.func(ExprMode::Behavior),
			TokenKind::Keyword(Keyword::Mod) => self.module(),
			TokenKind::Keyword(Keyword::Enum) => self.p_enum(),
			TokenKind::Keyword(Keyword::Struct) => self.p_struct(),
			TokenKind::Keyword(Keyword::Impl) => self.implementation(),
			_ => err(self, tok),
		}
	}

	fn import(&mut self) -> Option<(ImportTree<'a>, Loc<'a>)> {
		let span = self.next().expect("tried to parse import at <eof>").span;
		let mut span = self.loc(span);
		let mut prefix = Path {
			node: Vec::new(),
			loc: self.loc(Default::default()),
		};
		let mut kind = ImportTreeKind::Simple(None);

		loop {
			let tok = self.peek()?;
			match tok.kind {
				TokenKind::Ident => {
					prefix.node.push(self.ident(tok));
					prefix.loc += tok.span;
					span += tok.span;
					self.next();

					if self.peek().is_some() {
						let tok = expect!(self, TokenKind::Ident, err = "expected `ident` to import as")?;
						kind = ImportTreeKind::Simple(Some(self.ident(tok)));
						break;
					}
				},
				TokenKind::Op(Op::Star) => {
					span += tok.span;
					kind = ImportTreeKind::Glob;
					self.next();
					break;
				},
				TokenKind::OpenDelim(Delimiter::Brace) => {
					self.delim_list(TokenKind::Comma, Delimiter::Brace, |p| p.import())
						.map(|imports| {
							kind = ImportTreeKind::Nested(imports.0);
							span += imports.1;
						});
					break;
				},
				_ => {
					self.next();
					self.diagnostics.add(
						Diagnostic::new(Level::Error, "expected `ident`, glob, or nested import").add_label(
							Label::primary(format!("found `{}`", tok.kind), self.loc(tok.span).into()),
						),
					)
				},
			}

			if peek!(self, TokenKind::Separator).is_none() {
				break;
			}
		}

		Some((ImportTree { prefix, kind }, span))
	}

	fn func(&mut self, mode: ExprMode) -> Option<Item<'a>> {
		let span = self.next().expect("tried to parse function/template at <eof>").span;
		let ident = expect!(
			self,
			TokenKind::Ident,
			err = if mode == ExprMode::Behavior {
				"expected template name"
			} else {
				"expected function name"
			}
		)?;
		let params = self.delim_list(TokenKind::Comma, Delimiter::Paren, |p| p.param(mode == ExprMode::RPN))?;

		if mode == ExprMode::Behavior {
			let block = self.block(mode);
			Some(Item {
				loc: self.loc(span) + block.loc,
				node: ItemKind::Template(self.ident(ident), params.0, block),
			})
		} else {
			let ret = if peek!(self, TokenKind::Arrow).is_some() {
				RetTy::Ty(self.ty())
			} else {
				RetTy::Default(self.loc(Span {
					start: params.1.span.end - 1,
					end: params.1.span.end,
				}))
			};
			let block = self.block(mode);
			Some(Item {
				loc: self.loc(span) + block.loc,
				node: if mode == ExprMode::Normal {
					ItemKind::Fn(
						self.ident(ident),
						FnDecl {
							inputs: params.0,
							output: ret,
						},
						block,
					)
				} else {
					ItemKind::RPNFn(
						self.ident(ident),
						FnDecl {
							inputs: params.0,
							output: ret,
						},
						block,
					)
				},
			})
		}
	}

	fn param(&mut self, rpn: bool) -> Option<(Param<'a>, Loc<'a>)> {
		let ident = expect!(self, TokenKind::Ident, err = "expected `ident`")?;
		expect!(self, TokenKind::Colon, err = "expected `colon`");
		let ty = self.ty();
		let mut loc = ty.loc + ident.span;
		let default = if peek!(self, TokenKind::Op(Op::Eq)).is_some() {
			let expr = self.expr_must(if rpn { ExprMode::RPN } else { ExprMode::Normal }, true);
			if let ExprKind::Err = expr.node {
				self.diagnostics.add(
					Diagnostic::new(Level::Error, "expected expression").add_label(Label::primary("", expr.loc.into())),
				)
			}
			loc += expr.loc;
			Some(expr)
		} else {
			None
		};

		Some((
			Param {
				ident: self.ident(ident),
				ty: Some(ty),
				default,
				loc,
			},
			loc,
		))
	}

	fn pat(&mut self) -> Pat<'a> {
		let tok = if let Some(tok) = self.peek() {
			tok
		} else {
			return Pat {
				loc: self.loc(Default::default()),
				node: PatKind::Ignore,
			};
		};
		match tok.kind {
			TokenKind::Ident if tok.data == "_" => {
				self.next();
				Pat {
					loc: self.loc(tok.span),
					node: PatKind::Ignore,
				}
			},
			TokenKind::Ident => {
				self.next();
				Pat {
					loc: self.loc(tok.span),
					node: PatKind::Binding(self.ident(tok)),
				}
			},
			TokenKind::OpenDelim(Delimiter::Paren) => self
				.delim_list(TokenKind::Comma, Delimiter::Paren, |p| {
					let pat = p.pat();
					let loc = pat.loc;
					Some((pat, loc))
				})
				.map(|idents| Pat {
					node: PatKind::Destructure(idents.0),
					loc: idents.1,
				})
				.unwrap_or(Pat {
					loc: self.loc(tok.span),
					node: PatKind::Ignore,
				}),
			_ => {
				self.diagnostics.add(
					Diagnostic::new(Level::Error, "expected identifier or destructure").add_label(Label::primary(
						format!("found `{}`", tok.kind),
						self.loc(tok.span).into(),
					)),
				);
				Pat {
					loc: self.loc(tok.span),
					node: PatKind::Ignore,
				}
			},
		}
	}

	fn module(&mut self) -> Option<Item<'a>> {
		let span = self.next().expect("tried to parse module at <eof>").span;
		let loc = self.loc(span);
		let ident = expect!(self, TokenKind::Ident, err = "expected module name")?;
		let mut module = Mod {
			inline: false,
			items: Vec::new(),
		};
		let path = self
			.path
			.into_iter()
			.map(|s| *s)
			.chain(std::iter::once(ident.data))
			.collect::<Vec<_>>();
		if let Some(file) = (self.loader)(&path) {
			let parser = Parser::new(file, &path, self.loader, &mut self.diagnostics);

			module.items = parser.items();
		} else {
			self.diagnostics.add(
				Diagnostic::new(Level::Error, "could not open file for module")
					.add_label(Label::primary("", self.loc(ident.span).into())),
			)
		}
		let tok = expect!(
			self,
			TokenKind::Semi,
			err = "expected `semicolon` after module declaration"
		)?;
		Some(Item {
			node: ItemKind::Mod(self.ident(ident), module),
			loc: loc + tok.span,
		})
	}

	fn p_struct(&mut self) -> Option<Item<'a>> {
		let span = self.next().expect("tried to parse struct at <eof>").span;
		let ident = expect!(self, TokenKind::Ident, err = "expected struct name")?;
		let fields = self.delim_list(TokenKind::Comma, Delimiter::Brace, |p| p.param(false))?;
		let loc = fields.1 + span;

		Some(Item {
			node: ItemKind::Struct(self.ident(ident), fields.0),
			loc,
		})
	}

	fn p_enum(&mut self) -> Option<Item<'a>> {
		let variant = |p: &mut Self| {
			let ident = expect!(p, TokenKind::Ident, err = "expected enum variant name")?;
			let ident = p.ident(ident);
			let mut loc = ident.loc;
			let disc = if peek!(p, TokenKind::Op(Op::Eq)).is_some() {
				let lit = p.literal();
				loc += lit.loc;
				Some(lit)
			} else {
				None
			};

			Some((Variant { ident, disc, loc }, loc))
		};

		let span = self.next().expect("tried to parse enum at <eof>").span;
		let ident = expect!(self, TokenKind::Ident, err = "expected enum name")?;
		let ident = self.ident(ident);
		let variants = self.delim_list(TokenKind::Comma, Delimiter::Brace, variant)?;
		let loc = variants.1 + span;

		Some(Item {
			node: ItemKind::Enum(ident, variants.0),
			loc,
		})
	}

	fn implementation(&mut self) -> Option<Item<'a>> {
		let span = self.next().expect("tried to parse impl at <eof>").span;
		let on = self.ty();
		expect!(self, TokenKind::OpenDelim(Delimiter::Brace), err = "expected `{`");
		let mut items = Vec::new();

		let mut error = false;
		while let Some(tok) = self.peek() {
			if tok.kind == TokenKind::CloseDelim(Delimiter::Brace) {
				break;
			} else if tok.kind != TokenKind::Keyword(Keyword::Function) {
				if !error {
					self.diagnostics
						.add(
							Diagnostic::new(Level::Error, "expected function in impl").add_label(Label::primary(
								format!("found `{}`", tok.kind),
								self.loc(tok.span).into(),
							)),
						)
				}

				error = true;
				self.next();
			} else {
				error = false;

				if let Some(item) = self.func(ExprMode::Normal) {
					if let ItemKind::Fn(ident, decl, block) = item.node {
						items.push((ident, decl, block, item.loc));
					} else {
						unreachable!("we just parsed a function, how did we not get one back?");
					}
				}
			}
		}

		let loc = expect!(self, TokenKind::CloseDelim(Delimiter::Brace), err = "expected `}`")?.span;
		let loc = self.loc(span) + loc;

		Some(Item {
			node: ItemKind::Impl(ImplKind { on, items }),
			loc,
		})
	}
}

mod precedence {
	pub const ASSIGNMENT: u8 = 1;
	pub const OR: u8 = 2;
	pub const AND: u8 = 3;
	pub const EQUALITY: u8 = 4;
	pub const COMPARISON: u8 = 5;
	pub const TERM: u8 = 6;
	pub const FACTOR: u8 = 7;
	pub const UNARY: u8 = 8;
	pub const CALL: u8 = 9;
}

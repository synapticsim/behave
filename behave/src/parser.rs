use std::{iter::Peekable, ops::Range};

use crate::{
	ast::{ASTType, Behaviors, Ident, Import, ImportType, LODs, NumberLiteral, Path, StringLiteral, AST, LOD},
	diagnostic::{Diagnostic, Label, Level},
	lexer::Lexer,
	token::{Token, TokenType},
};

pub enum ParserMode {
	MainFile,
	ImportedFile,
}

pub struct Parser<'a, 'b> {
	mode: ParserMode,
	file: &'a str,
	lexer: Peekable<&'a mut Lexer<'b>>,
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

	($self:expr, $(|)? $($pattern:pat_param)|+ $(if $guard:expr)?,else $if_not:expr) => {
		{
			let tok = match $self.peek() {
				Some(tok) => tok,
				None => $if_not,
			};
			if !match tok.0 {
                $( $pattern )|+ $( if $guard )? => true,
                _ => false
            } {
				$if_not
			} else {
				$self.next();
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

macro_rules! parse_tok {
	($name:ident, $output:path, $expect:ident, $message:expr) => {
		fn $name(&mut self) -> Result<$output, Diagnostic> {
			let tok = next!(self);
			if let TokenType::$expect(val) = tok.0 {
				Ok($output(val, tok.1))
			} else {
				Err(Diagnostic::new(Level::Error, $message).add_label(Label::unexpected(self.file, &tok)))
			}
		}
	};
}

impl<'a, 'b> Parser<'a, 'b> {
	parse_tok!(parse_string, StringLiteral, String, "expected string literal");

	parse_tok!(parse_number, NumberLiteral, Number, "expected number literal");

	parse_tok!(parse_ident, Ident, Ident, "expected identifier");

	pub fn new(
		mode: ParserMode, file: &'a str, lexer: &'a mut Lexer<'b>, diagnostics: &'a mut Vec<Diagnostic>,
	) -> Self {
		Self {
			mode,
			file,
			lexer: lexer.peekable(),
			diagnostics,
		}
	}

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
					},
					ASTType::Secondary(..) => {
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "LOD description is only allowed in the main file")
								.add_label(Label::primary(
									self.file,
									"move this to the main `.behave` file",
									token.1,
								)),
						);
						error = true;
						resync!(self, self.parse_lods(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
					},
				},
				TokenType::Behavior => match ast.ast_data {
					ASTType::Main(_, ref mut behaviors) => {
						*behaviors = resync!(self, self.parse_behaviors(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						})
					},
					ASTType::Secondary(..) => {
						self.diagnostics.push(
							Diagnostic::new(Level::Error, "Behavior description is only allowed in the main file")
								.add_label(Label::primary(
									self.file,
									"move this to the main `.behave` file",
									token.1,
								)),
						);
						error = true;
						resync!(self, self.parse_lods(), until TokenType::RightBrace, else {
							error = true;
							continue 'w;
						});
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

	fn parse_lods(&mut self) -> Result<LODs, Diagnostic> {
		let mut lods = LODs(Vec::new(), no_range());
		lods.1 = expect!(self, TokenType::LeftBrace, "expected `{`");

		loop {
			let min_size = self.parse_number()?;
			expect!(self, TokenType::Arrow, "expected `->`");
			let file = self.parse_string()?;
			let (r1, r2) = (min_size.1.clone(), file.1.clone());
			lods.0.push((LOD { min_size, file }, merge_range!(r1, r2)));

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

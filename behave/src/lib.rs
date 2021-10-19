use std::{io::Read, path::PathBuf};

use crate::diagnostic::{Diagnostic, Diagnostics};

pub mod diagnostic;
// mod output;

/// The result of a compilation.
pub struct CompileResult {
	/// The diagnostics produced during compilation.
	pub diagnostics: Vec<Diagnostic>,
	/// The files to write to disk, relative to the output directory.
	pub files: Vec<(PathBuf, String)>,
}

/// Compile a `behave` file.
///
/// # Parameters:
/// `file`: The file to compile.
/// `name`: The name of the file.
/// `loader`: A function to load files relative to the folder that contains `file`.
pub fn compile<R, F>(mut file: R, name: &str, loader: F) -> CompileResult
where
	R: Read,
	F: FnMut(PathBuf) -> Option<R>,
{
	let mut buf: PathBuf = name.into();
	buf.set_extension("xml");
	let mut s = String::new();
	file.read_to_string(&mut s).unwrap();

	CompileResult {
		diagnostics: Vec::new(),
		files: vec![(buf, s)],
	}
}

use std::{
	collections::HashMap,
	fs::{read_to_string, write},
	path::PathBuf,
};

use behave::diagnostic::{Diagnostic, Level};
use clap::{crate_version, AppSettings, Clap};
use diagnostic::display_diagnostics;

mod diagnostic;

/// A compiler for Microsoft Flight Simulator ModelBehaviors and ModelInfo.
/// It also compiles to RPN.
#[derive(Clap)]
#[clap(version = crate_version!(), author = "Synaptic Simulations")]
#[clap(setting = AppSettings::ColoredHelp)]
struct Options {
	/// The `behave` file to compile.
	file: PathBuf,
	/// The output path of the compiled XML.
	#[clap(short, long)]
	output: PathBuf,
}

struct CompileResult {
	pub files_opened: HashMap<String, String>,
	pub diagnostics: Vec<Diagnostic>,
}

fn main() {
	let options = Options::parse();
	let result = compile(&options);
	display_diagnostics(result.files_opened, result.diagnostics);
}

fn compile(options: &Options) -> CompileResult {
	let mut files_opened = HashMap::new();
	let mut diagnostics = Vec::new();

	let file_data = match read_to_string(&options.file) {
		Ok(s) => s,
		Err(err) => {
			return CompileResult {
				files_opened,
				diagnostics: vec![Diagnostic::new(
					Level::Error,
					format!("Failed to open file '{}': {}", options.file.display(), err),
				)],
			}
		},
	};
	let file_name = options.file.file_stem().unwrap();
	let file_root = options.file.parent().unwrap();

	files_opened.insert(file_name.to_string_lossy().into(), file_data.clone());

	let result = behave::compile(file_name.to_string_lossy(), file_data, |path| {
		let mut to_path = PathBuf::from(file_root);
		to_path.push(path.replace('.', "/"));
		to_path.set_extension("beh");

		match read_to_string(&to_path) {
			Ok(s) => {
				files_opened.insert(path.to_string(), s.clone());
				Ok(s)
			},
			Err(err) => Err((to_path.to_string_lossy().to_string(), err)),
		}
	});
	diagnostics.extend(result.diagnostics);

	if let Some(output) = result.compiled {
		match write(&options.output, output) {
			Err(err) => {
				return CompileResult {
					files_opened,
					diagnostics: vec![Diagnostic::new(
						Level::Error,
						format!("Failed to write to file '{}': {}", options.output.display(), err),
					)],
				}
			},
			_ => {},
		};
	}

	CompileResult {
		files_opened,
		diagnostics,
	}
}

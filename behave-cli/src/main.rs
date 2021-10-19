use std::{
	fs::{create_dir_all, read_to_string, write},
	panic::set_hook,
	path::PathBuf,
};

use behave::diagnostic::{Diagnostic, Level};
use clap::{crate_version, AppSettings, Clap};
use diagnostic::display_diagnostics;

mod diagnostic;

/// A compiler for Microsoft Flight Simulator ModelBehaviors and ModelInfo.
#[derive(Clap)]
#[clap(version = crate_version!(), author = "Synaptic Simulations")]
#[clap(setting = AppSettings::ColoredHelp)]
struct Options {
	/// The `behave` file to compile.
	file: PathBuf,
	/// The output folder of the compiled XML.
	#[clap(short, long)]
	output: PathBuf,
}

fn main() {
	set_hook(Box::new(|info| {
		println!("Internal Compiler Error: This is a bug, please report it to https://github.com/Synaptic-Simulations/behave/issues");
		println!("{}", info);
	}));

	let options = Options::parse();
	let (file, diag) = compile(options);
	display_diagnostics(file, diag);
}

fn compile(options: Options) -> (Vec<(String, String)>, Vec<Diagnostic>) {
	let s = match read_to_string(&options.file) {
		Ok(s) => s,
		Err(_) => {
			return (
				Vec::new(),
				vec![Diagnostic::new(
					Level::Error,
					format!("cannot open file '{}'", options.file.display()),
				)],
			)
		},
	};

	let name = options.file.file_name().unwrap().to_str().unwrap();
	let result = behave::compile(s.as_bytes(), name, |_| None);

	for file in result.files {
		let final_path = {
			let mut path = options.output.clone();
			path.extend(&file.0);
			path
		};
		create_dir_all(final_path.parent().unwrap()).unwrap();
		write(final_path, file.1).unwrap();
	}

	(vec![(name.into(), s)], result.diagnostics)
}

use std::collections::HashMap;

use behave::diagnostic::{Diagnostic, LabelType, Level};
use codespan_reporting::{
	diagnostic::{self, Label, LabelStyle, Severity},
	files::SimpleFiles,
	term::{
		self,
		termcolor::{ColorChoice, StandardStream},
		Config,
	},
};

pub fn display_diagnostics(file_map: Vec<(String, String)>, diagnostics: Vec<Diagnostic>) {
	let (files, id_map) = {
		let mut files = SimpleFiles::new();
		let mut id_map = HashMap::new();
		for file in file_map {
			id_map.insert(file.0.clone(), files.add(file.0, file.1));
		}

		(files, id_map)
	};

	let mut writer = StandardStream::stderr(ColorChoice::Auto);
	let config = Config::default();

	for diagnostic in diagnostics {
		let diagnostic = diagnostic::Diagnostic {
			severity: match diagnostic.level {
				Level::Error => Severity::Error,
				Level::Warning => Severity::Warning,
				Level::Note => Severity::Note,
				Level::Help => Severity::Help,
			},
			code: None,
			message: diagnostic.message,
			labels: diagnostic
				.labels
				.into_iter()
				.map(|label| Label {
					style: match label.ty {
						LabelType::Primary => LabelStyle::Primary,
						LabelType::Secondary => LabelStyle::Secondary,
					},
					file_id: id_map[&label.loc.file],
					range: label.loc.span.start as usize..label.loc.span.end as usize,
					message: label.message,
				})
				.collect(),
			notes: diagnostic.notes,
		};

		term::emit(&mut writer, &config, &files, &diagnostic).unwrap();
	}
}

use std::ops::Range;

use crate::token::Token;

/// The type of a label.
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum LabelType {
	/// A primary label that shows the primary cause of a diagnostic.
	Primary,
	/// A secondary label that provides secondary information.
	Secondary,
}

/// A label in source code.
#[derive(Clone, Debug)]
pub struct Label {
	/// The type of the label.
	pub label_type: LabelType,
	/// The message associated with the label.
	pub message: String,
	/// The file that the label belongs to.
	pub file: Vec<String>,
	/// The byte-range of the label.
	pub range: Range<usize>,
}

impl Label {
	pub fn primary(file: &[impl ToString + Clone], message: impl ToString, range: Range<usize>) -> Self {
		Label {
			label_type: LabelType::Primary,
			message: message.to_string(),
			file: file.to_vec().into_iter().map(|s| s.to_string()).collect(),
			range,
		}
	}

	pub fn secondary(file: &[impl ToString + Clone], message: impl ToString, range: Range<usize>) -> Self {
		Label {
			label_type: LabelType::Secondary,
			message: message.to_string(),
			file: file.to_vec().into_iter().map(|s| s.to_string()).collect(),
			range,
		}
	}

	pub fn unexpected(file: &[impl ToString + Clone], token: &Token) -> Self {
		Label {
			label_type: LabelType::Primary,
			message: format!("found `{}`", token.to_type()),
			file: file.to_vec().into_iter().map(|s| s.to_string()).collect(),
			range: token.1.clone(),
		}
	}
}

/// The level of a diagnostic.
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Level {
	Error,
	Warning,
	Note,
	Help,
}

/// A diagnostic.
#[derive(Clone, Debug)]
pub struct Diagnostic {
	/// The diagnostic message.
	pub message: String,
	/// The leve; of the diagnostic.
	pub level: Level,
	/// The labels associated with the diagnostic.
	pub labels: Vec<Label>,
	/// Additional information.
	pub notes: Vec<String>,
}

impl Diagnostic {
	pub fn new(level: Level, message: impl ToString) -> Self {
		Self {
			message: message.to_string(),
			level,
			labels: Vec::new(),
			notes: Vec::new(),
		}
	}

	pub fn add_labels<I>(mut self, labels: I) -> Self
	where
		I: IntoIterator<Item = Label>,
	{
		self.labels.extend(labels);
		self
	}

	pub fn add_label(mut self, label: Label) -> Self {
		self.labels.push(label);
		self
	}

	pub fn add_notes<I, S>(mut self, notes: I) -> Self
	where
		I: IntoIterator<Item = S>,
		S: ToString,
	{
		self.notes.extend(notes.into_iter().map(|s| s.to_string()));
		self
	}

	pub fn add_note(mut self, note: impl ToString) -> Self {
		self.notes.push(note.to_string());
		self
	}
}

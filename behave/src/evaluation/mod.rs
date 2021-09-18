use crate::ast::{ASTTree, ASTType, AST};
use crate::evaluation::expression::ExpressionEvaluator;

mod expression;
mod scope;

pub fn evaluate(main_file: &[String], main: AST, others: ASTTree) -> Option<String> {
	let mut main_evaluator = ExpressionEvaluator::new(main_file);
	if let ASTType::Main(lods, behavior) = main.ast_data {
		for lod in lods.0 {
			println!(
				"{:?}: {:?}",
				main_evaluator.evaluate_expression(&lod.min_size),
				main_evaluator.evaluate_expression(&lod.file)
			);
		}
	} else {
		unreachable!()
	}

	None
}

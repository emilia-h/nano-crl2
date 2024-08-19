
use crate::analysis::context::AnalysisContext;
use crate::core::lexer::{Token, tokenize};
use crate::core::parser::Parser;
use crate::ir::module::ModuleId;
use crate::model::module::Module;

use std::sync::Arc;

pub fn query_token_list(
    context: &AnalysisContext,
    module: ModuleId,
) -> Result<Arc<Vec<Token>>, ()> {
    match context.token_lists.get_or_lock(&module) {
        Ok(Some(value)) => value,
        Ok(None) => {
            let (_, input) = context.get_model_input(module);
            let result = match tokenize(input) {
                Ok(value) => Ok(Arc::new(value)),
                Err(error) => {
                    context.error();
                    Err(())
                }
            };

            context.token_lists.unlock(&module, result.clone());
            result        
        },
        Err(()) => {
            context.error();
            Err(())
        },
    }
}

pub fn query_ast_module(
    context: &AnalysisContext,
    module: ModuleId,
) -> Result<Arc<Module>, ()> {
    match context.ast_modules.get_or_lock(&module) {
        Ok(Some(value)) => value,
        Ok(None) => {
            let result = calculate_ast_module(context, module);
            context.ast_modules.unlock(&module, result.clone());
            result
        },
        Err(()) => {
            context.error();
            Err(())
        },
    }
}

fn calculate_ast_module(
    context: &AnalysisContext,
    module: ModuleId,
) -> Result<Arc<Module>, ()> {
    let tokens = query_token_list(context, module)?;
    let mut parser = Parser::new(&tokens);
    match parser.parse::<Module>() {
        Ok(value) => Ok(Arc::new(value)),
        Err(error) => {
            context.error();
            Err(())
        }
    }
}

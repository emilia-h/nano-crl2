
use crate::analysis::context::AnalysisContext;
use crate::core::lexer::{Token, tokenize};
use crate::core::parser::Parser;
use crate::core::syntax::{ModuleId, SourceRange};
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
                Ok(tokens) => Ok(Arc::new(tokens)),
                Err(error) => {
                    let loc = SourceRange::new(
                        error.loc.get_line(),
                        error.loc.get_char(),
                        error.loc.get_line(),
                        error.loc.get_char(),
                    );
                    context.error(module, loc, error.message)
                }
            };

            context.token_lists.unlock(&module, result.clone());
            result        
        },
        Err(()) => {
            context.error_cyclic_dependency(
                SourceRange::new(0, 0, 0, 0),
                module.into(),
            )
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
            context.error_cyclic_dependency(
                SourceRange::new(0, 0, 0, 0),
                module.into(),
            )
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
        Ok(value) => {
            // we want the parser to consume the entire file, which is not
            // guaranteed (and not supposed to be) when calling .parse(), so we
            // check it here
            if parser.has_token() {
                let error = "expected end of file but found more tokens".to_owned();
                return context.error(module, parser.get_loc(), error);
            }
            Ok(Arc::new(value))
        },
        Err(error) => {
            context.error(module, error.loc, error.message)
        }
    }
}

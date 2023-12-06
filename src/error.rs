use pest::iterators::Pair;
use crate::{parse::Rule, ast::{AstProperties, Function}};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFINE CUSTOM ERROR ENUMS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// An enum representing all types of errors that may occur
/// in the compilation process. All compile passes that may fail
/// should return one of these kinds of errors as the error variant
/// of a `Result<>`.
#[derive(Clone, Debug)]
pub enum CompileError{
  Parse(String),
  Type(String),
  Glslify(String),
}

impl CompileError{
  /// Returns a `ParseError`, filling a predefined template with the expected
  /// construct and properties of the construct found. These include its line
  /// and column in the input, its text and the parser rule it matched against.
  pub fn throw_parse(expected: &str, found:Pair<'_, Rule>)->Self{
    CompileError::Parse(format!(
      "Expected {} at line {}, column {}. Instead found '{}', which is a '{:?}'", 
      expected, found.line_col().0, found.line_col().1, found.as_str(), found.as_rule()
    ))
  }

  /// Returns a `TypeError`, filling a predefined template with the content that has an
  /// undefined type
  pub fn throw_type_undefined(content: &str)->Self{
    CompileError::Type(format!("Type of '{}' could not be infered.", content))
  }

  /// Returns a `TypeError`, filling a predefined template with the content that has a
  /// conflicting type
  pub fn throw_type_conflict(content: &AstProperties)->Self{
    CompileError::Type(format!(
      "Type of '{}' at line {}, colum {} is unexpected.", 
      content.source, 
      content.source_start.0, 
      content.source_start.1
    ))
  }

  /// Returns a `GlslifyError`, informing users that the conversion from an
  /// AST node to the final GLSL code has failed.
  pub fn throw_glslify_error(content: &AstProperties)->Self{
    CompileError::Glslify(format!(
      "Error compiling '{}' to GLSL at line {}, column {}.", 
      content.source, 
      content.source_start.0, 
      content.source_start.1
    ))
  }

  /// Returns a `TypeError`, informing the user that the return type of a function detected
  /// from the expression in the function body does not match the function signature.
  pub fn throw_fn_signature_mismatch(function: &Function)->Self{
    CompileError::Type(format!(
      "Return type of function '{}' does not match the function signature at line {}, column {}",
      function.ident,
      function.properties.source_start.0,
      function.properties.source_start.1
    ))
  }
}

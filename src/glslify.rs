use std::collections::HashSet;

use deunicode::deunicode;

use crate::{ast::Expression, error::CompileError, ast::{Uniform, PrefixUnaryOperator, PostfixUnaryOperator, InfixBinaryOperator, Scope}, types::{Type, Dimension}};


pub trait Glslify{
  /// Creates a GLSL representation of an AST node recursively.
  /// 
  /// The AST must have undergone typechecking before this function is invoked.
  fn to_glsl(&self)->Result<String, CompileError>;
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ EXPRESSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impl Glslify for Expression{
  fn to_glsl(&self)->Result<String, CompileError>{
    match self{
      // atoms return strings immediately
      Expression::Identifier(ident) => {
        Ok(identifier_utf8_to_ascii(&ident.name))
      },
      Expression::Uniform(unif) => {
        match unif{
          Uniform::UV(_) => Ok("uv".to_owned()),
          Uniform::Time(_) => Ok("iTime".to_owned()),
        }
      },
      Expression::Number(num) => {
        // use pretty print {:?} to make sure at least one decimal place is printed
        Ok(format!("{:?}",num.value))
      },
      Expression::PreUnaryOp { op, val, properties: _ } => {
        match op{
          PrefixUnaryOperator::Negate => { Ok(format!("(-{})", val.to_glsl()?))},
        }
      },
      Expression::PostUnaryOp { op, val, properties: _ } => {
        match op{
          PostfixUnaryOperator::ProjectX => {Ok(format!("{}.x", val.to_glsl()?))},
          PostfixUnaryOperator::ProjectY => {Ok(format!("{}.y", val.to_glsl()?))},
          PostfixUnaryOperator::ProjectZ => {Ok(format!("{}.z", val.to_glsl()?))},
          PostfixUnaryOperator::ProjectW => {Ok(format!("{}.w", val.to_glsl()?))},
          PostfixUnaryOperator::Sin => {Ok(format!("sin({})", val.to_glsl()?))},
          PostfixUnaryOperator::Fract => {Ok(format!("fract({})", val.to_glsl()?))},
          PostfixUnaryOperator::Length => {Ok(format!("length({})", val.to_glsl()?))},
        }
      },
      Expression::InfixBinaryOp { lhs, op, rhs, properties: _ } => {
        match op{
          InfixBinaryOperator::Add => {Ok(format!("({}+{})", lhs.to_glsl()?, rhs.to_glsl()?))},
          InfixBinaryOperator::Subtract =>{Ok(format!("({}-{})", lhs.to_glsl()?, rhs.to_glsl()?))},
          InfixBinaryOperator::Multiply => {Ok(format!("({}*{})", lhs.to_glsl()?, rhs.to_glsl()?))},
          InfixBinaryOperator::Divide => {Ok(format!("({}/{})", lhs.to_glsl()?, rhs.to_glsl()?))},
        }
      },
    }
  }
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TYPES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impl Glslify for Type{
  fn to_glsl(&self)->Result<String, CompileError> {
    match self {
      Type::Number(dim) =>  {
        match dim{
          Dimension::One => Ok("float".to_string()),
          Dimension::Two => Ok("vec2".to_string()),
          Dimension::Three => Ok("vec3".to_string()),
          Dimension::Four => Ok("vec4".to_string()),
        }
      },
      Type::Function { args: _, returns: _ } => {
        Err(CompileError::Glslify(
          "Compiling Function Types to GLSL is currently unsupported.".to_owned()
        ))
      },
    }
  }
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OTHERS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// Convert an UTF-8 identifier to its ASCII representation, transliterating emojis etc.
/// 
/// Spaces that might result from the transliteration are converted to underscores, turning
/// `🤩` into `star_struck` instead of `star struck`.
pub fn identifier_utf8_to_ascii(ident: &str)->String{
  deunicode(ident).replace(' ', "_")
}

impl Glslify for Scope {
  fn to_glsl(&self)->Result<String, CompileError> {
    let mut result = String::new();
    let mut defined_identifiers = HashSet::with_capacity(self.assign.len());
    for assign in &self.assign{
      // when previously defined identifiers are overwritten, the type in front of the
      // assignment must be omitted.
      if defined_identifiers.contains(&assign.ident) {
        result.push_str(&format!(
          "{} = {};\n", 
          identifier_utf8_to_ascii(&assign.ident), 
          assign.val.to_glsl()?
        ));
      } else {
        defined_identifiers.insert(&assign.ident);
        result.push_str(&format!(
          "{} {} = {};\n", 
          (*assign.val).get_own_type().clone().ok_or(
            CompileError::throw_glslify_error(&assign.properties)
          )?.to_glsl()?,
          identifier_utf8_to_ascii(&assign.ident), 
          assign.val.to_glsl()?
        ));
      }
    }
    result.push_str(&format!("return {}",self.expr.to_glsl()?));
    Ok(result)
  }
}

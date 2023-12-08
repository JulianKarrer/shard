use std::collections::HashSet;

use deunicode::deunicode;

use crate::{ast::Expression, error::CompileError, ast::{Uniform, UnaryOperator, InfixBinaryOperator, Function, Program}, types::{Type, Dimension}};


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
      Expression::UnaryOp { op, val, properties: _ } => {
        match op{
          UnaryOperator::Negate => {Ok(format!("(-{})", val.to_glsl()?))},
          UnaryOperator::ProjectX => {Ok(format!("{}.x", val.to_glsl()?))},
          UnaryOperator::ProjectY => {Ok(format!("{}.y", val.to_glsl()?))},
          UnaryOperator::ProjectZ => {Ok(format!("{}.z", val.to_glsl()?))},
          UnaryOperator::ProjectW => {Ok(format!("{}.w", val.to_glsl()?))},
          UnaryOperator::Sin => {Ok(format!("sin({})", val.to_glsl()?))},
          UnaryOperator::Fract => {Ok(format!("fract({})", val.to_glsl()?))},
          UnaryOperator::Length => {Ok(format!("length({})", val.to_glsl()?))},
        }
      },
      Expression::BinaryOp { lhs, op, rhs, properties: _ } => {
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
          "Can't compile type 'Function' to GLSL using only type information. 
The function struct containing the arguments' identifiers is required.".to_owned()
        ))
      },
    }
  }
}

impl Glslify for Dimension{
  fn to_glsl(&self)->Result<String, CompileError> {
    match self {
      Dimension::One => Ok("float".to_string()),
      Dimension::Two => Ok("vec2".to_string()),
      Dimension::Three => Ok("vec3".to_string()),
      Dimension::Four => Ok("vec4".to_string()),
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

impl Glslify for Function {
  fn to_glsl(&self)->Result<String, CompileError> {
    let mut result = String::new();
    // add the function signature
     if let Some(Type::Function { args: _, returns }) = &self.properties.own_type{
      result.push_str(&format!("{} {}({}){{",
        // exception: the function with identifier 'main' is the only function with a void return type
        if self.ident=="main"{ "void".to_owned() }else{ returns.to_glsl()? },
        self.ident,
        {
          // join together format strings for each argument identifier and GLSL type
          let mut arguments_glsl:Vec<String> = vec![];
          for (arg_name, arg_type) in self.get_args_clone()?{
            arguments_glsl.push(format!("in {} {}", arg_type.to_glsl()?, arg_name))
          }
          arguments_glsl.join(",")
        }
      ));
    } else {
      return Err(CompileError::throw_glslify_error(&self.properties));
    };

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
    // exception: the function with identifier 'main' is the only function that assigns to 
    // gl_FragColor instead of returning a value
    result.push_str( 
      &if self.ident=="main"{ 
        format!("gl_FragColor = {};}}",self.expr.to_glsl()?)
      } else{ 
        format!("return {};}}",self.expr.to_glsl()?)
      }
    );

    Ok(result)
  }
}

impl Glslify for Program{
  fn to_glsl(&self)->Result<String, CompileError> {
    // add the head of the glsl file
    let mut result = String::from("
precision highp float; 
uniform vec2 resolution; 
uniform float time;
#define uv 2.0*gl_FragCoord.xy/resolution.xy-1.0
");
    // add the main body, containing function definitions
    let mut function_strigs = vec![];
    for function in &self.functions{
      function_strigs.push(function.to_glsl()?)
    }
    result.push_str(&function_strigs.join("\n"));

    Ok(result)
  }
}
use error::CompileError;
use parse::parse_program;
use crate::{types::set_types, glslify::Glslify, builtins::{set_of_used_built_ins, glslify_all_builtins}};

mod parse;
mod ast;
mod types;
mod error;
mod glslify;
mod builtins;

/// Compiles a Shard program to GLSL code, returning a String with the entire program
/// code for a fragment shader on success and a `CompileError` otherwise. 
/// 
/// This is the main entry point of the library.
pub fn compile(source: &str)->Result<String, CompileError>{
  // parse the input source code, creating an AST with no type information
  let mut program =  parse_program(source)?;
  // collect all built-in functions used in the program
  let mut built_ins_used = set_of_used_built_ins(&program);
  // fill the AST with type information, filling Option<Type> with Some(Type::...)
  println!("{:#?}", program);
  set_types(&mut program, &built_ins_used)?;
  println!("{:#?}", program);
  // add boilerplate GLSL
  let mut result = String::from("precision highp float; uniform vec2 resolution; uniform float time;
#define uv ((gl_FragCoord.xy/resolution.xy*2.-1.)*vec2(max(resolution.x/resolution.y,1.),max(resolution.y/resolution.x,1.)))
");
  // add the implementations of all built-in functions used
  result.push_str(&glslify_all_builtins(&mut built_ins_used));
  // add the implementation of the main program
  result.push_str(&program.to_glsl()?);
  
  Ok(result)
}

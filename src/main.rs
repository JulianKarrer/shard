use error::CompileError;
use parse::parse_program;

use crate::{types::set_types, glslify::Glslify};

mod parse;
mod ast;
mod types;
mod error;
mod glslify;

pub fn compile(source: &str)->Result<String, CompileError>{
  let mut program =  parse_program(source)?;
  set_types(&mut program)?;
  println!("{:#?}", program);
  program.to_glsl()
}

const SOURCE: &str = "

fn sdf(pos:3, eps:1)->1
  pos.length - eps

fn main()->2
  🤩:= uv
  -🤩.sin
";

fn main(){
  match compile(SOURCE){
    Ok(glsl) => {println!("{}", glsl)},
    Err(err) => println!("{:#?}", err),
  } 
}

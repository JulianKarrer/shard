use std::sync::OnceLock;

use ahash::{AHashSet, AHashMap};
use strum::{EnumIter, IntoEnumIterator};

use crate::{types::{Type, Dimension}, parse::to_identifier, ast::{Program, Expression}};

/// An enum containing all built in functions of the Shard programming language.
/// 
/// In order to leverage Rust's type system when implementing new built-in functions,
/// they are represented by an enum that has methods implemented on it, using exhaustive
/// `match` patterns. This way, a new enum variant can be added and the compiler will gently
/// guide the developer to all the places that need implementing. 
/// 
/// This prevents spaghetti code of maps from Strings to stuff with dangling content, 
/// missing entries and the likes.
#[derive(EnumIter, PartialEq, Eq, Hash, Debug, Copy, Clone)]
pub enum BuiltInFunctions{
  To2d,
  To3d,
  To4d,
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DEFINE BUILT-IN FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impl BuiltInFunctions{
  fn name(&self)->String{
    match self{
      BuiltInFunctions::To2d => to_identifier("2d"),
      BuiltInFunctions::To3d => to_identifier("3d"),
      BuiltInFunctions::To4d => to_identifier("4d"),
    }
  }

  fn signature(&self)->Type{
    match self {
      BuiltInFunctions::To2d => Type::Function{
        args:vec![Type::Number(Dimension::One),Type::Number(Dimension::One)], 
        returns: Dimension::Two
      },
      BuiltInFunctions::To3d => Type::Function{
        args:vec![Type::Number(Dimension::One),Type::Number(Dimension::One),Type::Number(Dimension::One)], 
        returns: Dimension::Three
      },
      BuiltInFunctions::To4d => Type::Function{
        args:vec![Type::Number(Dimension::One),Type::Number(Dimension::One),Type::Number(Dimension::One),Type::Number(Dimension::One)], 
        returns: Dimension::Four
      },
    }
  }

  fn implementation(&self)->String{
    match self{
      BuiltInFunctions::To2d => format!("vec2 {}(in float a, in float b){{return vec2(a,b);}}\n" ,self.name()),
      BuiltInFunctions::To3d => format!("vec3 {}(in float a, in float b, in float c){{return vec3(a,b,c);}}\n",self.name()),
      BuiltInFunctions::To4d => format!("vec4 {}(in float a, in float b, in float c, in float d){{return vec4(a,b,c,d);}}\n",self.name()),
    }
  }

}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TYPECHECK BUILT-IN FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// From a set of built-in functions used, get all the relevant function signatures.
/// 
/// This can be used when type-checking to make sure that arguments to built-in functions match
/// the expected type and the return type fits.
pub fn built_ins_signatures(built_ins_used: &AHashSet<BuiltInFunctions>)->Vec<(String, Type)>{
  BuiltInFunctions::iter()
    .filter(|f| built_ins_used.contains(f))
    .map(|f|(f.name(), f.signature()))
    .collect()
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~ CHECK WHAT BUILT-INS ARE USED ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// From a program, detect all the built in functions used in it and return them in a set.
pub fn set_of_used_built_ins(program: &Program)->AHashSet<BuiltInFunctions>{
  let mut result = AHashSet::new();
  for func in &program.functions{
    collect_used_builtins(&func.expr, &mut result);
    for assign in &func.assign{
      collect_used_builtins(&assign.val, &mut result);
    }
  }
  result
}

static BUILT_INS_BY_NAME:OnceLock<AHashMap<String, BuiltInFunctions>> = OnceLock::new();
/// Recursively add all built-in functions used in an expression to the given set.
/// 
/// This may initialize a OnceLock on the first call in which a built-in is added, creating
/// a hashmap from function identifiers belonging to built-ins to their respective `BuiltInFunction`
/// enum variant.
fn collect_used_builtins(expr: &Expression, set: &mut AHashSet<BuiltInFunctions>){
  match expr{
    Expression::Identifier(_) => (),
    Expression::Uniform(_) => (),
    Expression::Number(_) => (),
    Expression::UnaryOp { op: _, val, properties: _ } => {
      collect_used_builtins(val, set);
    },
    Expression::BinaryOp { lhs, op: _, rhs, properties: _ } => {
      collect_used_builtins(lhs, set);
      collect_used_builtins(rhs, set);
    },
    Expression::FunctionCall(call) => {
      if let Some(built_in) = BUILT_INS_BY_NAME.get_or_init(||{
        AHashMap::from_iter(BuiltInFunctions::iter().map(|f|(f.name(), f)))
      }).get(&call.function_ident){
        set.insert(*built_in);
      };
    },
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GLSLIFY ALL USED BUILT-INS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// Returns GLSL code implementing all the built-in functions given in the
/// `built_ins_used` set. 
/// 
/// This drains the set, leaving it empty, which means this function 
/// should be used only when keeping track of the built-ins used is no longer required.
pub fn glslify_all_builtins(built_ins_used: &mut AHashSet<BuiltInFunctions>)->String{
  let mut result = String::new();
  for built_in in built_ins_used.drain(){
    result.push_str(&built_in.implementation())
  }
  result
}
use enum_map::{EnumMap, enum_map, Enum};
use halfbrown::HashMap;
use lazy_static::lazy_static;

use crate::{ast::{AstNode, Uniform, PrefixUnaryOperator, InfixBinaryOperator, PostfixUnaryOperator}, Error};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ WHAT IS A TYPE? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// The dimensionality of the *Number* type. This is also
/// used to specify the return type of functions since functions
/// returning other functions are not allowed.
#[derive(Enum, Copy, Clone, PartialEq, PartialOrd, Debug)]
pub enum Dimension{
  // dimensionality must be defined in ascending order top to bottom
  // so that derive(PartialOrd) can work correctly
  One,
  Two,
  Three,
  Four
}

/// The types of data. This is either a *Number*, which is what 
/// an expression evaluates to, or a *Function*, which is basically
/// an expression with placeholders that requires parameters to be given
/// in order to fill the placeholders and evaluate to a number.
/// 
/// - A number is the only primitive type and is implemented as a floating
/// point number of the specified dimensionality 
/// (like float, vec2, vec3, vec4 in GLSL)
/// 
/// - A function consists of a (possibly empty) vector of arguments 
/// of any type, including functions, and returns a number. 
/// 
/// A function may not return another function.
#[derive(Debug, Clone)]
pub enum Type{
  Number(Dimension),
  Function{
    args: Vec<Type>,
    returns: Dimension
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ WHAT OBJECT HAS WHAT TYPE? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lazy_static!(
  /// Specify the dimensionality of all available uniforms exhaustively.
  static ref UNIFORM_DIMS:EnumMap<Uniform, Dimension> = enum_map! {
    Uniform::Time => Dimension::One,
    Uniform::UV => Dimension::Two,
  };
);

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~ WHAT FUNCTION YIELDS WHAT TYPE? ~~~~~~~~~~~~~~~~~~~~~~~~~~

/// Type signatures must be implemented by any nodes in the AST that represent
/// a function, including built-in operators and functions. 
/// 
/// Given the types of the input arguments, the node must yield a unique output type,
/// which may not be a function.
trait TypeSignature{
  fn output_type(&self, arguments: &[Type])->Result<Type, Error>;
}

impl TypeSignature for PrefixUnaryOperator{
  fn output_type(&self, arguments: &[Type])->Result<Type, Error> {
    let error = Error::throw_type_conflict(&format!("{:#?}", self));
    if arguments.len() != 1 {return Err(error)}
    let arg_type =  arguments.first().ok_or(error.clone())?;
    match self{
      // negation works on any number type
      PrefixUnaryOperator::Negate => {
        if let Type::Number(dim) = arg_type{
          Ok(Type::Number(*dim))
        } else {
          Err(error)
        }
      },
    }
  }
}

impl TypeSignature for InfixBinaryOperator{
  fn output_type(&self, arguments: &[Type])->Result<Type, Error> {
    let error = Error::throw_type_conflict(&format!("{:#?}", self));
    if arguments.len() != 2 {return Err(error)}
    let arg_1_type =  arguments.first().ok_or(error.clone())?;
    let arg_2_type =  arguments.last().ok_or(error.clone())?;
    match self{
      // addition and subtraction require two numbers of equal dimensionality to work
      InfixBinaryOperator::Add => {
        if let Type::Number(dim1) = arg_1_type {
          if let Type::Number(dim2) = arg_2_type {
            if dim1 == dim2 {return Ok(Type::Number(*dim1))}
          }
        }
        Err(error)
      },
      InfixBinaryOperator::Subtract => {
        if let Type::Number(dim1) = arg_1_type {
          if let Type::Number(dim2) = arg_2_type {
            if dim1 == dim2 {return Ok(Type::Number(*dim1))}
          }
        }
        Err(error)
      },
      // multiplication is interpreted as component-wise multiplication of numbers of
      // equal dimensionality, or with a scalar
      InfixBinaryOperator::Multiply => {
        if let Type::Number(dim1) = arg_1_type {
          if let Type::Number(dim2) = arg_2_type {
            // component-wise multiplication
            if dim1 == dim2 {return Ok(Type::Number(*dim1))}
            else {
              // scalar multiplication, dim1 != dim2
              // the result has the higher dimensionality amongst dim1 and dim2
              if *dim1 == Dimension::One { return Ok(Type::Number(*dim2)); }
              if *dim2 == Dimension::One { return Ok(Type::Number(*dim1)); }
            }
          }
        }
        Err(error)
      },
      // for division, the second operand must have equal dimensionality to the first or
      // be scalar
      InfixBinaryOperator::Divide => {
        if let Type::Number(dim1) = arg_1_type {
          if let Type::Number(dim2) = arg_2_type {
            if dim1 == dim2 {return Ok(Type::Number(*dim1))}
            else if *dim2 == Dimension::One { return Ok(Type::Number(*dim1)); }
          }
        }
        Err(error)
      },
    }
  }
}

impl TypeSignature for PostfixUnaryOperator{
  fn output_type(&self, arguments: &[Type])->Result<Type, Error> {
    let error = Error::throw_type_conflict(&format!("{:#?}", self));
    if arguments.len() != 1 {return Err(error)}
    let arg_type =  arguments.first().ok_or(error.clone())?;
    match self{
      // projection operators always return a one-dimensional number
      // their arguments must have at least n dimensions for the nth operator
      // eg.  .x requires one dimension, .y requires two, .z three and so on
      PostfixUnaryOperator::ProjectX => {
        if let Type::Number(_) = arg_type { Ok(Type::Number(Dimension::One)) } else { Err(error) }
      },
      PostfixUnaryOperator::ProjectY => {
        if let Type::Number(dim) = arg_type{
          if *dim > Dimension::One {return Ok(Type::Number(Dimension::One))}
        } 
        Err(error) 
      },
      PostfixUnaryOperator::ProjectZ => {
        if let Type::Number(dim) = arg_type{
          if *dim > Dimension::Two {return Ok(Type::Number(Dimension::One))}
        } 
        Err(error) 
      },
      PostfixUnaryOperator::ProjectW => {
        if let Type::Number(dim) = arg_type{
          if *dim > Dimension::Three {return Ok(Type::Number(Dimension::One))}
        } 
        Err(error) 
      },
      // functions that accept any number type from one to four dimensions and 
      // return the a scalar:
      PostfixUnaryOperator::Length => 
        if let Type::Number(_) = arg_type { Ok(Type::Number(Dimension::One)) } else { Err(error) },
      // functions that accept any number type from one to four dimensions and 
      // return the same type:
      PostfixUnaryOperator::Sin => 
        if let Type::Number(dim) = arg_type { Ok(Type::Number(*dim)) } else { Err(error) },
      PostfixUnaryOperator::Fract =>
        if let Type::Number(dim) = arg_type { Ok(Type::Number(*dim)) } else { Err(error) },
    }
  }
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ LABEL THE AST WITH TYPES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// TypedAstNodes wrap the AstNode enum, additionally providing a type
/// at each node, basically labeling the abstract syntex tree with types.
#[derive(Debug)]
pub struct TypedAstNode{
  node: AstNode,
  type_of_node: Type,
  children: Vec<TypedAstNode>
}

impl TypedAstNode{
  /// Label an AST with type information, returning a TypedAstNode for every
  /// AstNode in the original tree. This function recursively descends down the tree,
  /// labeling the leaves that should all have known types, then working its way back up.
  pub fn from_ast(ast: AstNode, identifier_types: &mut HashMap<String, Type>)->Result<Self, Error>{
    match ast{
      AstNode::Scope(scope) => {
        let node = AstNode::Scope(scope.clone());
        // a new scope means the list of defined identifiers is reset.
        identifier_types.clear();
        let mut children = vec![];
        for asgn in &scope.assign{
          children.push(TypedAstNode::from_ast(AstNode::Assignment((*asgn).clone()), identifier_types)?)
        }
        let child_expr = TypedAstNode::from_ast(AstNode::Expression((*scope.expr).clone()), identifier_types)?;
        let scope_type = child_expr.type_of_node.clone();
        children.push(child_expr);
        Ok(TypedAstNode{ node, type_of_node: scope_type, children})
      },
      AstNode::Assignment(asgn) => {
        let node = AstNode::Assignment(asgn.clone());
        let rhs = TypedAstNode::from_ast(AstNode::Expression(*asgn.val), identifier_types)?;
        // add the type that was assigned to the identifier to the identifier_types map
        identifier_types.insert(asgn.ident, rhs.type_of_node.clone());
        Ok(TypedAstNode{ node, type_of_node: rhs.type_of_node.clone(), children: vec![rhs] })
      },
      AstNode::Expression(expr) => {
        let node = AstNode::Expression(expr.clone());
        match &expr {
          crate::ast::Expression::PreUnaryOp { op, val } => {
            let child = TypedAstNode::from_ast(AstNode::Expression((**val).clone()), identifier_types)?;
            let child_type = child.type_of_node.clone();
            Ok(TypedAstNode { 
              node, 
              type_of_node: op.output_type(&[child_type])?, 
              children: vec![child] 
            })
          },
          crate::ast::Expression::InfixBinaryOp { lhs, op, rhs } => {
            let l_child = TypedAstNode::from_ast(AstNode::Expression((**lhs).clone()), identifier_types)?;
            let r_child = TypedAstNode::from_ast(AstNode::Expression((**rhs).clone()), identifier_types)?;
            let l_type = l_child.type_of_node.clone();
            let r_type = r_child.type_of_node.clone();
            Ok(TypedAstNode { 
              node, 
              type_of_node: op.output_type(&[l_type, r_type])?, 
              children: vec![l_child, r_child] 
            })
          },
          crate::ast::Expression::PostUnaryOp { op, val } => {
            let child = TypedAstNode::from_ast(AstNode::Expression((**val).clone()), identifier_types)?;
            let child_type = child.type_of_node.clone();
            Ok(TypedAstNode { 
              node, 
              type_of_node: op.output_type(&[child_type])?, 
              children: vec![child] 
            })
          },
          // atomic expressions should have known types
          crate::ast::Expression::Identifier(ident) => 
            Ok({
              let type_of_node = identifier_types.get(&ident.name)
                .ok_or(Error::throw_type_undefined(&ident.name))?.clone();
              TypedAstNode{ node, type_of_node, children: vec![] }
            }),
          crate::ast::Expression::Uniform(unif) =>  Ok({
            TypedAstNode{ node, type_of_node: Type::Number(UNIFORM_DIMS[*unif]), children: vec![] }
          }),
          crate::ast::Expression::Number(_) => Ok({
            TypedAstNode{ node, type_of_node: Type::Number(Dimension::One), children: vec![] }
          }),
        }
      },
    }
  }
}


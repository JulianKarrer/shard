use crate::{ast::{Uniform, PrefixUnaryOperator, InfixBinaryOperator, PostfixUnaryOperator, Number, Identifier, Expression, Assignment, Scope}, CompileError};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ WHAT IS A TYPE? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// The dimensionality of the *Number* type. This is also
/// used to specify the return type of functions since functions
/// returning other functions are not allowed.
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
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

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~ WHAT FUNCTION YIELDS WHAT TYPE? ~~~~~~~~~~~~~~~~~~~~~~~~~~

/// Type signatures must be implemented by any nodes in the AST that represent
/// a function, including built-in operators and functions. 
/// 
/// Given the types of the input arguments, the node must yield a unique output type,
/// which may not be a function.
trait InfersType{
  /// Infer the type of `self` based on information available or
  /// recursive function calls to `infer_own_types` on child nodes of the AST.
  /// 
  /// This function sets the type for a node, replacing the `None` in `Option<Type>` 
  /// with `Some(Type:: ...)`, returning the type set or propagating errors.
  fn set_own_type(&mut self, idents: &mut Vec<(String, Type)>)->Result<Type, CompileError>;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ATOMS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impl InfersType for Uniform{
  fn set_own_type(&mut self, _idents: &mut Vec<(String, Type)>)->Result<Type, CompileError> {
    // get own type
    let own_type = match self{
      Uniform::UV(_) => { Type::Number(Dimension::Two) },
      Uniform::Time(_) => { Type::Number(Dimension::One) },
    };
    // set own type
    match self {
      Uniform::UV(prop) => {prop.own_type = Some(own_type.clone())},
      Uniform::Time(prop) => {prop.own_type = Some(own_type.clone())},
    };
    // return own type
    Ok(own_type)
  }
}

impl InfersType for Number{
  fn set_own_type(&mut self, _idents: &mut Vec<(String, Type)>)->Result<Type, CompileError> {
    self.properties.own_type = Some(Type::Number(Dimension::One));
    Ok(Type::Number(Dimension::One))
  }
}

impl InfersType for Identifier{
  fn set_own_type(&mut self, idents: &mut Vec<(String, Type)>)->Result<Type, CompileError> {
    let error =  CompileError::throw_type_undefined(&self.name);
    let own_types:Vec<&Type> = idents.iter()
      .filter(|(name,_)| *name==self.name )
      .map(|(_, owntype)| owntype)
      .collect();
    // if own_types.len() != 1 {return Err(error)}  // TODO: something like this to note redifinitions
    let own_type = *(own_types.last().ok_or(error)?);
    self.properties.own_type = Some(own_type.clone());
    Ok(own_type.clone())
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ EXPRESSIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impl InfersType for Expression{
  fn set_own_type(&mut self, idents: &mut Vec<(String, Type)>)->Result<Type, CompileError> {
    match self {
      Expression::Identifier(ident) => ident.set_own_type(idents),
      Expression::Uniform(unif) => unif.set_own_type(idents),
      Expression::Number(num) => num.set_own_type(idents),
      Expression::PreUnaryOp { op, val, properties } => {
        let operand_type = val.set_own_type(idents)?;
        match op{
          PrefixUnaryOperator::Negate => {
            properties.own_type = Some(operand_type.clone());
            Ok(operand_type)
          },
        }
      },
      Expression::PostUnaryOp { op, val, properties } => {
        let operand = val.set_own_type(idents)?;
        let error = CompileError::throw_type_conflict(properties);
        match op {
          // projection operators always return a one-dimensional number
          // their arguments must have at least n dimensions for the nth operator
          // eg.  .x requires one dimension, .y requires two, .z three and so on
          PostfixUnaryOperator::ProjectX => {
            if let Type::Number(_) = operand { 
              let own_type = Type::Number(Dimension::One);
              properties.own_type = Some(own_type.clone());
              Ok(own_type) 
            }
            else { Err(error) }
          },
          PostfixUnaryOperator::ProjectY => {
            if let Type::Number(dim) = operand {
              if dim > Dimension::One { 
                let own_type = Type::Number(Dimension::One);
                properties.own_type = Some(own_type.clone());
                return Ok(own_type) 
              }
            }
            Err(error)
          },
          PostfixUnaryOperator::ProjectZ => {
            if let Type::Number(dim) = operand {
              if dim > Dimension::Two { 
                let own_type = Type::Number(Dimension::One);
                properties.own_type = Some(own_type.clone());
                return Ok(own_type) 
              }
            }
            Err(error)
          },
          PostfixUnaryOperator::ProjectW => {
            if let Type::Number(dim) = operand {
              if dim > Dimension::Three { 
                let own_type = Type::Number(Dimension::One);
                properties.own_type = Some(own_type.clone());
                return Ok(own_type) 
              }
            }
            Err(error)
          },
          // functions that accept any number type from one to four dimensions and 
          // return a scalar:
          PostfixUnaryOperator::Length=> {
            if let Type::Number(_) = operand { 
              let own_type = Type::Number(Dimension::One);
              properties.own_type = Some(own_type.clone());
              Ok(own_type) 
            }
            else { Err(error) }
          },
          // functions that accept any number type from one to four dimensions and 
          // return the same type:
          PostfixUnaryOperator::Sin => {
            if let Type::Number(dim) = operand { 
              let own_type = Type::Number(dim);
              properties.own_type = Some(own_type.clone());
              Ok(own_type) 
            }
            else { Err(error) }
          },
          PostfixUnaryOperator::Fract => {
            if let Type::Number(dim) = operand { 
              let own_type = Type::Number(dim);
              properties.own_type = Some(own_type.clone());
              Ok(own_type) 
            }
            else { Err(error) }
          },
        }
      },
      Expression::InfixBinaryOp { lhs, op, rhs, properties } => {
        let lhs = lhs.set_own_type(idents)?;
        let rhs = rhs.set_own_type(idents)?;
        let error = CompileError::throw_type_conflict(properties);
        match op{
          // addition and subtraction require two numbers of equal dimensionality to work
          InfixBinaryOperator::Add => {
            if let Type::Number(dim_l) = lhs{
              if let Type::Number(dim_r) = rhs{
                if dim_l == dim_r {
                  let own_type = Type::Number(dim_l);
                  properties.own_type = Some(own_type.clone());
                  return Ok(own_type) 
                }
              }
            }
            Err(error)
          },
          InfixBinaryOperator::Subtract => {
            if let Type::Number(dim_l) = lhs{
              if let Type::Number(dim_r) = rhs{
                if dim_l == dim_r {
                  let own_type = Type::Number(dim_l);
                  properties.own_type = Some(own_type.clone());
                  return Ok(own_type) 
                }
              }
            }
            Err(error)
          },
          // multiplication is interpreted as component-wise multiplication of numbers of
          // equal dimensionality, or with a scalar
          InfixBinaryOperator::Multiply => {
            if let Type::Number(dim_l) = lhs{
              if let Type::Number(dim_r) = rhs{
                if dim_l == dim_r {return Ok(Type::Number(dim_l))}
                else {
                  // scalar multiplication, dim_l != dim_r
                  // the result has the higher dimensionality amongst dim1 and dim2
                  if dim_l == Dimension::One { {
                    let own_type = Type::Number(dim_r);
                    properties.own_type = Some(own_type.clone());
                    return Ok(own_type) 
                  } }
                  else if dim_r == Dimension::One { {
                    let own_type = Type::Number(dim_l);
                    properties.own_type = Some(own_type.clone());
                    return Ok(own_type) 
                  } }
                }
              }
            }
            Err(error)
          },
          // for division, the second operand must have equal dimensionality to the first or
          // be scalar
          InfixBinaryOperator::Divide => {
            if let Type::Number(dim_l) = lhs{
              if let Type::Number(dim_r) = rhs{
                // component-wise division
                if dim_l == dim_r || dim_r == Dimension::One {{
                  let own_type = Type::Number(dim_l);
                  properties.own_type = Some(own_type.clone());
                  return Ok(own_type) 
                }}
              }
            }
            Err(error)
          },
        }
      },
    }
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ASSIGNMENTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impl InfersType for Assignment{
  fn set_own_type(&mut self, idents: &mut Vec<(String, Type)>)->Result<Type, CompileError> {
    let expr_type = self.val.set_own_type(idents)?;
    self.properties.own_type = Some(expr_type.clone());
    // add the type of the identifier inferred from the expression to the
    // stack of identifier types
    idents.push((self.ident.clone(), expr_type.clone()));
    Ok(expr_type)
  }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SCOPES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impl InfersType for Scope {
  fn set_own_type(&mut self, idents: &mut Vec<(String, Type)>)->Result<Type, CompileError> {
    for assign in &mut self.assign{
      assign.set_own_type(idents)?;
    }
    let expr_type = self.expr.set_own_type(idents)?;
    self.properties.own_type = Some(expr_type.clone());
    Ok(expr_type)
  }
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FUNCTIONS FOR CONVENIENCE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

impl Expression{
  /// Returns a reference to the Type of the expression.
  /// This is wrapper for convenience that abstacts away the specifics
  /// of accessing the AstProperties field in each of the Expression variants.
  pub fn get_own_type(&self)->&Option<Type>{
    match self{
      Expression::Identifier(ident) => {&ident.properties.own_type},
      Expression::Uniform(unif) => {match unif {
        Uniform::UV(prop) => &prop.own_type,
        Uniform::Time(prop) => &prop.own_type,
      }},
      Expression::Number(num) => &num.properties.own_type,
      Expression::PreUnaryOp { op: _, val: _, properties } => {
        &properties.own_type
      },
      Expression::PostUnaryOp { op: _, val: _, properties } => {
        &properties.own_type
      },
      Expression::InfixBinaryOp { lhs: _, op: _, rhs: _, properties } => {
        &properties.own_type
      },
  }
  }
}

/// Infer the types of every node in the AST, check if they match up with operator
/// and function definitions and label every node in the AST with its type.
/// 
/// This replaces all `None` variants with `Some(Type)` in every `AstProperties` struct
/// of the AST.
pub fn set_types(scope: &mut Scope)->Result<Type, CompileError>{
  scope.set_own_type(&mut vec![])
}

use crate::{ast::{Uniform, InfixBinaryOperator, UnaryOperator, Number, Identifier, Expression, Assignment, Function, Program}, CompileError};

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
#[derive(Debug, Clone, PartialEq)]
pub enum Type{
  Number(Dimension),
  Function{
    args: Vec<Type>,
    returns: Dimension
  }
}

pub fn dimension_from_str(to_parse: &str)->Result<Dimension, CompileError>{
  match to_parse{
    "1" => Ok(Dimension::One),
    "2" => Ok(Dimension::Two),
    "3" => Ok(Dimension::Three),
    "4" => Ok(Dimension::Four),
    _ => Err(
      CompileError::Parse(format!("Error parsing number type, found: '{}'", to_parse))
    )
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
      Expression::FunctionCall(call) => {
        let error = CompileError::throw_type_conflict(&call.properties);
        // check if each of the types of the arguments matches the function being called
        let matching_fn_names = idents.iter()
          .filter(|(n,_)| *n == call.function_ident)
          .map(|(_, t)| t)
          .collect::<Vec<&Type>>();
        // throw an error if the function called is ambiguous / redefined somewhere
        if matching_fn_names.len() != 1{return Err(CompileError::throw_fn_call_ambiguous(&call.properties))}
        let called_fn_type = matching_fn_names
          .last()
          .ok_or(error.clone())?
          .to_owned().to_owned();

        if let Type::Function { args, returns } = called_fn_type{
          // set types for all argument expressions and check if they match the signature of the called function
          for (arg_expression, expected_type) in call.args.iter_mut().zip(args){
            let arg_type = arg_expression.set_own_type(idents)?;
            if arg_type != expected_type {return Err(error)}
          }
          // set the type of the function call to the return type of the called function
          call.properties.own_type = Some(Type::Number(returns));
          Ok(Type::Number(returns))
        } else {
          Err(error)
        }
      }
      Expression::UnaryOp { op, val, properties } => {
        let operand = val.set_own_type(idents)?;
        let error = CompileError::throw_type_conflict(properties);
        match op {
          // prefix operators that return the input type
          UnaryOperator::Negate => {
            if let Type::Number(dim) = operand { 
              let own_type = Type::Number(dim);
              properties.own_type = Some(own_type.clone());
              Ok(own_type) 
            }
            else { Err(error) }
          },
          // projection operators always return a one-dimensional number
          // their arguments must have at least n dimensions for the nth operator
          // eg.  .x requires one dimension, .y requires two, .z three and so on
          UnaryOperator::ProjectX => {
            if let Type::Number(_) = operand { 
              let own_type = Type::Number(Dimension::One);
              properties.own_type = Some(own_type.clone());
              Ok(own_type) 
            }
            else { Err(error) }
          },
          UnaryOperator::ProjectY => {
            if let Type::Number(dim) = operand {
              if dim > Dimension::One { 
                let own_type = Type::Number(Dimension::One);
                properties.own_type = Some(own_type.clone());
                return Ok(own_type) 
              }
            }
            Err(error)
          },
          UnaryOperator::ProjectZ => {
            if let Type::Number(dim) = operand {
              if dim > Dimension::Two { 
                let own_type = Type::Number(Dimension::One);
                properties.own_type = Some(own_type.clone());
                return Ok(own_type) 
              }
            }
            Err(error)
          },
          UnaryOperator::ProjectW => {
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
          UnaryOperator::Length=> {
            if let Type::Number(_) = operand { 
              let own_type = Type::Number(Dimension::One);
              properties.own_type = Some(own_type.clone());
              Ok(own_type) 
            }
            else { Err(error) }
          },
          // functions that accept any number type from one to four dimensions and 
          // return the same type:
          UnaryOperator::Sin => {
            if let Type::Number(dim) = operand { 
              let own_type = Type::Number(dim);
              properties.own_type = Some(own_type.clone());
              Ok(own_type) 
            }
            else { Err(error) }
          },
          UnaryOperator::Fract => {
            if let Type::Number(dim) = operand { 
              let own_type = Type::Number(dim);
              properties.own_type = Some(own_type.clone());
              Ok(own_type) 
            }
            else { Err(error) }
          },
        }
      },
      Expression::BinaryOp { lhs, op, rhs, properties } => {
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

impl InfersType for Function {
  fn set_own_type(&mut self, idents: &mut Vec<(String, Type)>)->Result<Type, CompileError> {
    // add the arguments of the funtion to the list of defined, useable identifiers
    idents.append(&mut self.get_args_clone()?);

    // set types for all assignments in the function body, updating the list of defined
    // identifiers along the way
    for assign in &mut self.assign{
      assign.set_own_type(idents)?;
    }
    // finally, the expression in the function body can be evaluated
    let expr_type = self.expr.set_own_type(idents)?;

    // check if the type of the expression matches the function signature
    let mut passes_check = false;
    if let Some(Type::Function { args: _, returns }) = &self.properties.own_type{
      if let Type::Number(dim) = expr_type{
        if dim == *returns{ passes_check = true;}
      }
    }

    if passes_check {Ok(expr_type)} 
    else {Err(CompileError::throw_fn_signature_mismatch(self))}
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
      Expression::UnaryOp { op: _, val: _, properties } => {
        &properties.own_type
      },
      Expression::BinaryOp { lhs: _, op: _, rhs: _, properties } => {
        &properties.own_type
      },
      Expression::FunctionCall(call) => &call.properties.own_type,
    }
  }
}

/// Infer the types of every node in the AST, check if they match up with operator
/// and function definitions and label every node in the AST with its type.
/// 
/// This replaces all `None` variants with `Some(Type)` in every `AstProperties` struct
/// of the AST.
pub fn set_types(program: &mut Program)->Result<(), CompileError>{
  // function calls use function identifiers, the types of which are already known
  // from when the functions were parsed. Add these to the list of defined identifiers
  // that may be used in some function body.
  let mut function_identifiers:Vec<(String, Type)> = vec![];
  for f in &program.functions{
    function_identifiers.push((
      f.ident.clone(), 
      f.properties.own_type.clone().ok_or(CompileError::Catastrophic(
        format!("Function type was not correctly set while parsing for {:?}", f))
      )?
    ))
  };
    
  // set all types in the function body and check if the return type matches the signature
  for function in &mut program.functions{
    function.set_own_type(&mut function_identifiers.clone())?;
  }
  Ok(())
}

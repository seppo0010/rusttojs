use super::TreeNode;
use types::{BinaryOperation, DeclLocalType, BlockType, FieldInitType, ReturnType, TokenTree};

#[derive(Debug, Clone)]
pub enum ExprType {
  ExprStruct(ExprStructType),
  ExprIf(ExprIfType),
  ExprRet(ExprRetType),
  ExprLit(ExprLitType),
  ExprCall(Box<ExprCallType>),
  ExprMac(MacroType),
  ExprAssign(ExprAssignType),
  ExprBinaryOp(ExprBinaryOpType),
  DeclLocal(DeclLocalType),
  ExprPath(Vec<String>),
  ExprBlock(BlockType),
}

impl ExprType {
  pub fn from_tree_r<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    match &*value.get_name() {
      "ExprMac" => ExprType::ExprMac(MacroType::from_tree(&value.get_nodes()[0], return_type)),
      "DeclLocal" => ExprType::DeclLocal(DeclLocalType::from_tree(value)),
      "ExprRet" => ExprType::ExprRet(ExprRetType::from_tree(value.get_nodes().first())),
      "ExprLit" => ExprType::ExprLit(ExprLitType::from_tree(&value.get_nodes()[0], return_type)),
      "ExprCall" => ExprType::ExprCall(Box::new(ExprCallType::from_tree(value, return_type))),
      "ExprPath" => ExprType::ExprPath(value.get_components_ident()),
      "ExprAssign" => ExprType::ExprAssign(ExprAssignType::from_tree(value, return_type)),
      "ExprBinary" => ExprType::ExprBinaryOp(ExprBinaryOpType::from_tree(value, return_type)),
      "ExprIf" => ExprType::ExprIf(ExprIfType::from_tree(value, return_type)),
      "ExprBlock" => ExprType::ExprBlock(BlockType::from_tree(Some(value), return_type)),
      "ExprStruct" => ExprType::ExprStruct(ExprStructType::from_tree(value)),
      _ => panic!("{:?}", value),
    }
  }

  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    ExprType::from_tree_r(value, ReturnType::None)
  }

  pub fn is_ret(&self) -> bool {
    match *self {
      ExprType::ExprRet(_) => true,
      _ => false,
    }
  }

  pub fn use_semicolon(&self) -> bool {
    match *self {
      ExprType::ExprIf(_) => false,
      _ => true,
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    match *self {
      ExprType::ExprMac(ref e) => e.unknown_type_count(),
      ExprType::DeclLocal(_) => 0,
      ExprType::ExprRet(_) => 0,
      ExprType::ExprLit(ref e) => e.unknown_type_count(),
      ExprType::ExprCall(ref e) => e.unknown_type_count(),
      ExprType::ExprPath(_) => 0,
      ExprType::ExprAssign(ref e) => e.unknown_type_count(),
      ExprType::ExprBinaryOp(ref e) => e.unknown_type_count(),
      ExprType::ExprIf(ref e) => e.unknown_type_count(),
      ExprType::ExprBlock(ref e) => e.unknown_type_count(),
      ExprType::ExprStruct(_) => 0,
    }
  }
}

#[derive(Debug, Clone)]
pub struct ExprStructType {
  pub name: String,
  pub fields: Vec<FieldInitType>,
}

impl ExprStructType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    ExprStructType {
      name: value.get_components_ident_joined(),
      fields: value.get_node_by_name("FieldInits")
        .map(|node| node.get_nodes().iter().map(|field| {
          FieldInitType {
            name: field.get_node_by_name("ident").unwrap()
              .get_string_nodes().join(""),
            value: ExprType::from_tree(&field.get_nodes()[1]),
          }
        }).collect::<Vec<_>>())
        .unwrap_or(vec![]),
    }
  }
}

#[derive(Debug, Clone)]
pub struct ExprIfType {
  pub return_type: ReturnType,
  pub cond: Box<ExprType>,
  pub true_block: Box<BlockType>,
}

impl ExprIfType {
  fn from_tree<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    let nodes = value.get_nodes();
    ExprIfType {
      return_type: return_type,
      cond: Box::new(ExprType::from_tree(&nodes[0])),
      true_block: Box::new(BlockType::from_tree(nodes.get(1), ReturnType::None)),
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    if self.return_type.is_some() { 0 } else { 1 }
  }
}

#[derive(Debug, Clone)]
pub struct ExprRetType {
  pub value: Option<Box<ExprType>>,
}

impl ExprRetType {
  pub fn with_value(value: Option<Box<ExprType>>) -> Self {
    ExprRetType {
      value: value,
    }
  }

  pub fn from_tree<T: TreeNode>(value: Option<&T>) -> Self {
    ExprRetType {
      value: value.map(|node| Box::new(ExprType::from_tree(node)))
    }
  }
}

#[derive(Debug, Clone)]
pub enum ExprLitType {
  LitInteger(String, ReturnType),
  LitStr(String),
  LitBool(bool),
}

impl ExprLitType {
  fn from_tree<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    match &*value.get_name() {
      "LitInteger" => ExprLitType::LitInteger(value.get_string_nodes().join("").to_owned(), return_type),
      "LitStr" => ExprLitType::LitStr(value.get_string_nodes().join("").to_owned()),
      "LitBool" => ExprLitType::LitBool(match &*value.get_string_nodes().join("") {
        "true" => true,
        "false" => false,
        _ => panic!("{:?}", value),
      }),
      _ => panic!("{:?}", value),
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    match *self {
      ExprLitType::LitInteger(_, ref t) => if t.is_some() { 0 } else { 1 },
      _ => 0,
    }
  }
}

#[derive(Debug, Clone)]
pub struct ExprCallType {
  pub return_type: ReturnType,
  pub function: ExprType,
  pub parameters: Vec<ExprType>,
}

impl ExprCallType {
  fn from_tree<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    let nodes = value.get_nodes();
    assert_eq!(nodes.len(), 2);
    ExprCallType {
      return_type: return_type,
      function: ExprType::from_tree(&nodes[0]),
      parameters: nodes.get(1).map(|node| node.get_nodes().iter().map(|node| ExprType::from_tree(node)).collect()).unwrap_or(Vec::new())
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    if self.return_type.is_some() { 0 } else { 1 }
  }
}

#[derive(Debug)]
pub struct MacroType {
  pub return_type: ReturnType,
  pub path_expr: Vec<String>,
  pub delimited_token_trees: [TokenTree; 3],
}

impl MacroType {
  pub fn from_tree<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    let path_expr = value.get_components_ident();
    let delimited_token_trees = value
      .get_node_by_name("TTDelim").unwrap()
      .get_nodes();
    MacroType {
      return_type: return_type,
      path_expr: path_expr,
      delimited_token_trees: [
        TokenTree::from_tree(&delimited_token_trees[0]),
        TokenTree::from_tree(&delimited_token_trees[1]),
        TokenTree::from_tree(&delimited_token_trees[2]),
      ]
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    if self.return_type.is_some() { 0 } else { 1 }
  }
}

impl Clone for MacroType {
  fn clone(&self) -> Self {
    MacroType {
      return_type: self.return_type.clone(),
      path_expr: self.path_expr.clone(),
      delimited_token_trees: [
        self.delimited_token_trees[0].clone(),
        self.delimited_token_trees[1].clone(),
        self.delimited_token_trees[2].clone(),
      ],
    }
  }
}

#[derive(Debug, Clone)]
pub struct ExprAssignType {
  pub return_type: ReturnType,
  pub target: Box<ExprType>,
  pub source: Box<ExprType>,
}
impl ExprAssignType {
  fn from_tree<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    ExprAssignType {
      return_type: return_type,
      target: Box::new(ExprType::from_tree(&value.get_nodes()[0])),
      source: Box::new(ExprType::from_tree(&value.get_nodes()[1])),
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    if self.return_type.is_some() { 0 } else { 1 }
  }
}

#[derive(Debug, Clone)]
pub struct ExprBinaryOpType {
  pub return_type: ReturnType,
  pub operation: BinaryOperation,
  pub lhs: Box<ExprType>,
  pub rhs: Box<ExprType>,
}

impl ExprBinaryOpType {
  fn from_tree<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    ExprBinaryOpType {
      return_type: return_type,
      operation: BinaryOperation::from_tree(value),
      lhs: Box::new(ExprType::from_tree(&value.get_nodes()[1])),
      rhs: Box::new(ExprType::from_tree(&value.get_nodes()[2])),
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    if self.return_type.is_some() { 0 } else { 1 }
  }
}

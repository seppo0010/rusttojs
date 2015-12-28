use super::TreeNode;
use types::{BinaryOperation, DeclLocalType, BlockType, ReturnType, TokenTree};

#[derive(Debug, Clone)]
pub struct ExprAssignType {
  pub target: Box<ExprType>,
  pub source: Box<ExprType>,
}
impl ExprAssignType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    ExprAssignType {
      target: Box::new(ExprType::from_tree(&value.get_nodes()[0])),
      source: Box::new(ExprType::from_tree(&value.get_nodes()[1])),
    }
  }
}

#[derive(Debug, Clone)]
pub struct ExprBinaryOpType {
  pub operation: BinaryOperation,
  pub lhs: Box<ExprType>,
  pub rhs: Box<ExprType>,
}

impl ExprBinaryOpType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    ExprBinaryOpType {
      operation: BinaryOperation::from_tree(value),
      lhs: Box::new(ExprType::from_tree(&value.get_nodes()[1])),
      rhs: Box::new(ExprType::from_tree(&value.get_nodes()[2])),
    }
  }
}

#[derive(Debug, Clone)]
pub enum ExprType {
  ExprMac(MacroType),
  DeclLocal(DeclLocalType),
  ExprRet(ExprRetType),
  ExprLit(ExprLitType),
  ExprCall(Box<ExprCallType>),
  ExprPath(String),
  ExprAssign(ExprAssignType),
  ExprBinaryOp(ExprBinaryOpType),
  ExprIf(ExprIfType),
  ExprBlock(BlockType),
}

impl ExprType {
  pub fn from_tree_r<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    match &*value.get_name() {
      "ExprMac" => ExprType::ExprMac(MacroType::from_tree(&value.get_nodes()[0])),
      "DeclLocal" => ExprType::DeclLocal(DeclLocalType::from_tree(value)),
      "ExprRet" => ExprType::ExprRet(ExprRetType::from_tree(value.get_nodes().first())),
      "ExprLit" => ExprType::ExprLit(ExprLitType::from_tree(&value.get_nodes()[0])),
      "ExprCall" => ExprType::ExprCall(Box::new(ExprCallType::from_tree(value))),
      "ExprPath" => ExprType::ExprPath(value.get_components_ident_joined()),
      "ExprAssign" => ExprType::ExprAssign(ExprAssignType::from_tree(value)),
      "ExprBinary" => ExprType::ExprBinaryOp(ExprBinaryOpType::from_tree(value)),
      "ExprIf" => ExprType::ExprIf(ExprIfType::from_tree(value)),
      "ExprBlock" => ExprType::ExprBlock(BlockType::from_tree(Some(value), return_type)),
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
}

#[derive(Debug, Clone)]
pub struct ExprIfType {
  pub cond: Box<ExprType>,
  pub true_block: Box<BlockType>,
}

impl ExprIfType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    let nodes = value.get_nodes();
    ExprIfType {
      cond: Box::new(ExprType::from_tree(&nodes[0])),
      true_block: Box::new(BlockType::from_tree(nodes.get(1), ReturnType::None)),
    }
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
  LitInteger(String),
  LitStr(String),
  LitBool(bool),
}

impl ExprLitType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "LitInteger" => ExprLitType::LitInteger(value.get_string_nodes().join("").to_owned()),
      "LitStr" => ExprLitType::LitStr(value.get_string_nodes().join("").to_owned()),
      "LitBool" => ExprLitType::LitBool(match &*value.get_string_nodes().join("") {
        "true" => true,
        "false" => false,
        _ => panic!("{:?}", value),
      }),
      _ => panic!("{:?}", value),
    }
  }
}

#[derive(Debug, Clone)]
pub struct ExprCallType {
  pub function: ExprType,
  pub parameters: Vec<ExprType>,
}

impl ExprCallType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    let nodes = value.get_nodes();
    assert_eq!(nodes.len(), 2);
    ExprCallType {
      function: ExprType::from_tree(&nodes[0]),
      parameters: nodes.get(1).map(|node| node.get_nodes().iter().map(|node| ExprType::from_tree(node)).collect()).unwrap_or(Vec::new())
    }
  }
}

#[derive(Debug)]
pub struct MacroType {
  pub path_expr: String,
  pub delimited_token_trees: [TokenTree; 3],
}

impl MacroType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    let path_expr = value.get_components_ident_joined();
    let delimited_token_trees = value
      .get_node_by_name("TTDelim").unwrap()
      .get_nodes();
    MacroType {
      path_expr: path_expr,
      delimited_token_trees: [
        TokenTree::from_tree(&delimited_token_trees[0]),
        TokenTree::from_tree(&delimited_token_trees[1]),
        TokenTree::from_tree(&delimited_token_trees[2]),
      ]
    }
  }
}

impl Clone for MacroType {
  fn clone(&self) -> Self {
    MacroType {
      path_expr: self.path_expr.clone(),
      delimited_token_trees: [
        self.delimited_token_trees[0].clone(),
        self.delimited_token_trees[1].clone(),
        self.delimited_token_trees[2].clone(),
      ],
    }
  }
}

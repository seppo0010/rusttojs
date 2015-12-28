use super::TreeNode;
use exprs::ExprType;

#[derive(Debug, Clone)]
pub struct AttrType {
  pub doc_comment: Option<String>,
}

#[derive(Debug, Clone)]
pub struct AttrsAndVisType {
  pub attrs: Option<String>,
  pub vis: String,
}

impl AttrsAndVisType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    let mut args = value.get_string_nodes();
    assert!(args.len() == 1 || args.len() == 2);
    let vis = args.pop().unwrap();
    AttrsAndVisType {
      attrs: args.pop(),
      vis: vis,
    }
  }
}

#[derive(Debug, Clone)]
pub enum ReturnType {
  None,
  Unknown,
  Some(String),
}

impl ReturnType {
  pub fn is_some(&self) -> bool {
    match *self {
      ReturnType::None => false,
      _ => true,
    }
  }
}

#[derive(Debug, Clone)]
pub struct BlockType {
  pub stmts: Vec<ExprType>,
  pub return_type: ReturnType,
}

impl BlockType {
  pub fn from_tree<T: TreeNode>(value: Option<&T>, return_type: ReturnType) -> Self {
    BlockType {
      stmts: value
        .map(|node| if node.maybe_get_nodes().and_then(|n| n.last().map(|n| n.get_name())) == Some("stmts".to_owned()) {
          node.maybe_get_nodes().unwrap().last().unwrap().get_nodes()
        } else {
          node.get_nodes().pop().map(|x| vec![x]).unwrap_or(Vec::new())
        })
        .unwrap_or(vec![])
        .into_iter()
        .filter(|node| !node.is_null())
        .map(|node| ExprType::from_tree(&node))
        .collect(),
      return_type: return_type,
    }
  }
}

#[derive(Debug, Clone)]
pub struct AttrsAndBlockType {
  pub attrs: Vec<AttrType>,
  pub block: BlockType
}

impl AttrsAndBlockType {
  pub fn from_tree<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    AttrsAndBlockType {
      attrs: Vec::new(),
      block: BlockType::from_tree(value.get_node_by_name("ExprBlock"), return_type),
    }
  }
}

#[derive(Debug, Clone)]
pub enum BinaryOperation {
  BiOr,
  BiAnd,
  BiEq,
  BiNe,
  BiLt,
  BiGt,
  BiLe,
  BiGe,
  BiBitOr,
  BiBitXor,
  BiBitAnd,
  BiShl,
  BiShr,
  BiAdd,
  BiSub,
  BiMul,
  BiDiv,
  BiRem,
}

impl BinaryOperation {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_string_nodes().join("") {
      "BiOr" => BinaryOperation::BiOr,
      "BiAnd" => BinaryOperation::BiAnd,
      "BiEq" => BinaryOperation::BiEq,
      "BiNe" => BinaryOperation::BiNe,
      "BiLt" => BinaryOperation::BiLt,
      "BiGt" => BinaryOperation::BiGt,
      "BiLe" => BinaryOperation::BiLe,
      "BiGe" => BinaryOperation::BiGe,
      "BiBitOr" => BinaryOperation::BiBitOr,
      "BiBitXor" => BinaryOperation::BiBitXor,
      "BiBitAnd" => BinaryOperation::BiBitAnd,
      "BiShl" => BinaryOperation::BiShl,
      "BiShr" => BinaryOperation::BiShr,
      "BiAdd" => BinaryOperation::BiAdd,
      "BiSub" => BinaryOperation::BiSub,
      "BiMul" => BinaryOperation::BiMul,
      "BiDiv" => BinaryOperation::BiDiv,
      "BiRem" => BinaryOperation::BiRem,
      _ => panic!("{:?}", value),
    }
  }
}

#[derive(Debug, Clone)]
pub enum BindingType {
  Ref,
  RefMut,
  Mut,
}

#[derive(Debug, Clone)]
pub enum PatType {
  PatLit(String),
  PatIdent(BindingType, String),
}

impl PatType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    let nodes = value.get_nodes();
    match &*value.get_name() {
      "PatLit" => PatType::PatLit(value.get_components_ident_joined()),
      "PatIdent" => PatType::PatIdent(match &*nodes[0].get_name() {
        "BindByValue" => BindingType::Mut,
        "BindByRef" => match &*nodes[0].get_name() {
          "MutMutable" => BindingType::RefMut,
          "MutImmutable" => BindingType::Ref,
          _ => panic!("{:?}", value),
        },
        _ => panic!("{:?}", value),
      },
      value.get_node_by_name("ident").unwrap()
        .get_string_nodes().join("").to_owned()),
      _ => panic!("{:?}", value),
    }
  }
}


#[derive(Debug, Clone)]
pub struct DeclLocalType {
  pub pat: PatType,
  pub value_type: Option<String>,
  pub value: Option<Box<ExprType>>,
}

impl DeclLocalType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "DeclLocal");
    let pat = PatType::from_tree(&value.get_nodes()[0]);
    let value_type: Option<String> = None;

    let initial_value = value.get_nodes()
      .get(2)
      .and_then(|node| if node.is_null() { None } else { Some(node) })
      .map(|node| Box::new(ExprType::from_tree_r(node, match value_type {
              Some(ref v) => ReturnType::Some(v.clone()),
              None => ReturnType::Unknown,
              })));
    DeclLocalType {
      pat: pat,
      value_type: value_type,
      value: initial_value,
    }
  }
}

#[derive(Debug, Clone)]
pub enum TokenTree {
  Tok(String),
  TokenTrees(Vec<TokenTree>),
}

impl TokenTree {
  pub fn get_string(&self) -> Option<&String> {
    match *self {
      TokenTree::Tok(ref s) => Some(s),
      _ => None,
    }
  }
}

impl TokenTree {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "TTTok" => TokenTree::Tok(value.get_string_nodes().join("")),
      "TokenTrees" => TokenTree::TokenTrees(value.get_nodes().iter().map(
            |node| TokenTree::from_tree(node)).collect()),
      _ => panic!("{:?}", value),
    }
  }
}

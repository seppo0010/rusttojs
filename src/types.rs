use super::TreeNode;
use exprs::{ExprType, ExprScope};
use items::CrateType;

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

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnType {
  None,
  Unknown,
  Some(Vec<String>),
}

impl ReturnType {
  pub fn is_known(&self) -> bool {
    match *self {
      ReturnType::Unknown => false,
      _ => true,
    }
  }

  pub fn is_some(&self) -> bool {
    match *self {
      ReturnType::None => false,
      _ => true,
    }
  }
}

#[derive(Debug, Clone)]
pub struct BlockType<'a> {
  pub stmts: Vec<ExprType>,
  pub scope: ExprScope<'a>,
}

impl<'a> BlockType<'a> {
  pub fn from_tree<T: TreeNode>(value: Option<&T>, return_type: ReturnType, scope: Option<ExprScope>) -> Self {
    let stmts: Vec<_> = value
      .map(|node| if node.maybe_get_nodes().and_then(|n| n.last().map(|n| n.get_name())) == Some("stmts".to_owned()) {
        node.maybe_get_nodes().unwrap().last().unwrap().get_nodes()
      } else {
        node.get_nodes().pop().map(|x| vec![x]).unwrap_or(Vec::new())
      })
      .unwrap_or(vec![])
      .into_iter()
      .filter(|node| !node.is_null())
      .collect();
    let count = stmts.len();
    BlockType {
      stmts: stmts.into_iter()
        .enumerate()
        .map(|(i, node)| ExprType::from_tree_r(&node, if i + 1 == count { return_type.clone() } else { ReturnType::None }))
        .collect(),
      scope: scope.unwrap_or(ExprScope::default()),
    }
  }

  pub fn get_return_type(&self, scopes: &Vec<&mut ExprScope>) -> ReturnType {
    self.stmts.last().map(|x| x.get_return_type(scopes)).unwrap_or(ReturnType::None)
  }

  pub fn unknown_type_count(&self) -> u32 {
    self.stmts.iter().fold(0, |acc, stmt| acc + stmt.unknown_type_count())
  }

  pub fn identify_types(&mut self, krate: &CrateType, scopes: &Vec<&mut ExprScope>) {
    let mut local_scopes = vec![&mut self.scope];
    local_scopes.extend(scopes.iter().cloned());
    for stmt in self.stmts.iter_mut() {
      stmt.identify_types(krate, local_scopes);
    }
  }
}

#[derive(Debug, Clone)]
pub struct AttrsAndBlockType {
  pub attrs: Vec<AttrType>,
  pub block: BlockType,
}

impl AttrsAndBlockType {
  pub fn from_tree<T: TreeNode>(value: &T, return_type: ReturnType, scope: ExprScope) -> Self {
    AttrsAndBlockType {
      attrs: Vec::new(),
      block: BlockType::from_tree(value.get_node_by_name("ExprBlock"), return_type, Some(scope)),
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

  pub fn get_return_type(&self, t1: ReturnType, t2: ReturnType) -> ReturnType {
    if !t1.is_known() || !t2.is_known() {
      return ReturnType::Unknown;
    }

    if t1 != t2 {
      panic!("Cannot apply {:?} to {:?} and {:?}", self, t1, t2);
    }

    match *self {
      BinaryOperation::BiOr => ReturnType::Some(vec!["bool".to_owned()]),
      BinaryOperation::BiAnd => ReturnType::Some(vec!["bool".to_owned()]),
      BinaryOperation::BiEq => ReturnType::Some(vec!["bool".to_owned()]),
      BinaryOperation::BiNe => ReturnType::Some(vec!["bool".to_owned()]),
      BinaryOperation::BiLt => ReturnType::Some(vec!["bool".to_owned()]),
      BinaryOperation::BiGt => ReturnType::Some(vec!["bool".to_owned()]),
      BinaryOperation::BiLe => ReturnType::Some(vec!["bool".to_owned()]),
      BinaryOperation::BiGe => ReturnType::Some(vec!["bool".to_owned()]),
      BinaryOperation::BiBitOr => t1.clone(),
      BinaryOperation::BiBitXor => t1.clone(),
      BinaryOperation::BiBitAnd => t1.clone(),
      BinaryOperation::BiShl => panic!("Unhandle operation shl"),
      BinaryOperation::BiShr => panic!("Unhandle operation shr"),
      BinaryOperation::BiAdd => t1.clone(),
      BinaryOperation::BiSub => t1.clone(),
      BinaryOperation::BiMul => t1.clone(),
      BinaryOperation::BiDiv => t1.clone(),
      BinaryOperation::BiRem => t1.clone(),
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
  PatEnum(String, Vec<Box<PatType>>),
  PatWild,
}

impl PatType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    let nodes = value.get_nodes();
    match &*value.get_name() {
      "PatLit" => PatType::PatLit(value.get_components_ident_joined()),
      "PatIdent" => PatType::PatIdent(match &*nodes[0].get_name() {
        "BindByValue" => BindingType::Mut,
        "BindByRef" => match &*nodes[0].get_string_nodes().join("") {
          "MutMutable" => BindingType::RefMut,
          "MutImmutable" => BindingType::Ref,
          _ => panic!("{:?}", value),
        },
        _ => panic!("{:?}", value),
      },
      value.get_node_by_name("ident").unwrap()
        .get_string_nodes().join("").to_owned()),
      "PatEnum" => PatType::PatEnum(value.get_components_ident_joined(),
          value.get_nodes()[1].get_nodes().iter().map(|node| {
            Box::new(PatType::from_tree(node))
          }).collect()),
      _ => {
        if value.get_string_value() == Some("PatWild") {
          PatType::PatWild
        } else {
          panic!("{:?}", value)
        }
      },
    }
  }

  pub fn to_scope_string(&self) -> &str {
    match *self {
      PatType::PatLit(ref s) => &**s,
      PatType::PatIdent(_, ref s) => &**s,
      PatType::PatEnum(ref s, _) => &**s,
      PatType::PatWild => "",
    }
  }
}


#[derive(Debug, Clone)]
pub struct DeclLocalType {
  pub pat: PatType,
  pub value_type: Option<Vec<String>>,
  pub value: Option<Box<ExprType>>,
}

impl DeclLocalType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "DeclLocal");
    let pat = PatType::from_tree(&value.get_nodes()[0]);
    let value_type: Option<Vec<String>> = None;

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

  pub fn unknown_type_count(&self) -> u32 {
    match self.value {
      Some(ref x) => x.unknown_type_count(),
      None => 0,
    }
  }

  pub fn identify_types(&mut self, krate: &CrateType, scopes: &Vec<&mut ExprScope>) {
    match self.value {
      Some(ref mut x) => x.identify_types(krate, scopes),
      None => (),
    }
    match self.value {
      Some(ref x) => scopes[0].set(self.pat.to_scope_string().to_owned(), x.clone().get_return_type(scopes)),
      None => (),
    }
  }
}

#[derive(Debug, Clone)]
pub enum TokenTree {
  None,
  Delim(String, Box<TokenTree>, String),
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
    let nodes = value.get_nodes();
    match &*value.get_name() {
      "TTTok" => TokenTree::Tok(value.get_string_nodes().join("")),
      "TokenTrees" => TokenTree::TokenTrees(value.get_nodes().iter().map(
            |node| TokenTree::from_tree(node)).collect()),
      "TTDelim" => TokenTree::Delim(nodes[0].get_string_nodes().join(""),
          Box::new(TokenTree::from_tree(&nodes[1])),
          nodes[2].get_string_nodes().join("")),
      _ => TokenTree::None,
    }
  }
}

#[derive(Debug, Clone)]
pub struct FieldInitType {
  pub name: String,
  pub value: ExprType,
}

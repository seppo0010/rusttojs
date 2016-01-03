use std::collections::HashMap;

use super::TreeNode;
use items::CrateType;
use types::{BinaryOperation, DeclLocalType, BlockType, FieldInitType, ReturnType, TokenTree};

#[derive(Debug, Clone, Default)]
pub struct ExprScope {
  vars: HashMap<String, ReturnType>,
}

impl ExprScope {
  pub fn with_self(selfcontext: String) -> ExprScope {
    let mut scope = ExprScope::default();
    scope.vars.insert("self".to_owned(), ReturnType::Some(vec![selfcontext]));
    scope
  }

  pub fn get(&self, key: &String) -> ReturnType {
    match self.vars.get(key) {
      Some(t) => t.clone(),
      None => ReturnType::None,
    }
  }

  pub fn set(&mut self, key: String, value: ReturnType) {
    self.vars.insert(key, value);
  }
}

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
  ExprField(ExprFieldType),
}

impl ExprType {
  pub fn from_tree_r<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    match &*value.get_name() {
      "ExprMac" => ExprType::ExprMac(MacroType::from_tree(&value.get_nodes()[0], return_type)),
      "DeclLocal" => ExprType::DeclLocal(DeclLocalType::from_tree(value)),
      "ExprRet" => ExprType::ExprRet(ExprRetType::from_tree(value.get_nodes().first())),
      "ExprLit" => ExprType::ExprLit(ExprLitType::from_tree(&value.get_nodes()[0], return_type)),
      "ExprCall" => ExprType::ExprCall(Box::new(ExprCallType::from_tree(value, return_type))),
      "ExprPath" => ExprType::ExprPath(value.get_node_by_name("components").unwrap_or(value)
          .get_nodes().iter()
          .map(|node| node.get_string_nodes().join(""))
          .collect()),
      "ExprField" => ExprType::ExprField(ExprFieldType::from_tree_r(value, return_type)),
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
      ExprType::DeclLocal(ref e) => e.unknown_type_count(),
      ExprType::ExprRet(_) => 0,
      ExprType::ExprLit(ref e) => e.unknown_type_count(),
      ExprType::ExprCall(ref e) => e.unknown_type_count(),
      ExprType::ExprPath(_) => 0,
      ExprType::ExprAssign(ref e) => e.unknown_type_count(),
      ExprType::ExprBinaryOp(ref e) => e.unknown_type_count(),
      ExprType::ExprIf(ref e) => e.unknown_type_count(),
      ExprType::ExprBlock(ref e) => e.unknown_type_count(),
      ExprType::ExprStruct(_) => 0,
      ExprType::ExprField(ref e) => e.unknown_type_count(),
    }
  }

  pub fn get_return_type(&self) -> ReturnType {
    match *self {
      ExprType::ExprMac(ref e) => e.return_type.clone(),
      ExprType::DeclLocal(_) => ReturnType::None,
      ExprType::ExprRet(_) => ReturnType::None,
      ExprType::ExprLit(ref e) => e.get_return_type(),
      ExprType::ExprCall(ref e) => e.return_type.clone(),
      ExprType::ExprPath(ref e) => ReturnType::Some(e.clone()),
      ExprType::ExprAssign(ref e) => e.target.get_return_type(),
      ExprType::ExprBinaryOp(ref e) => e.return_type.clone(),
      ExprType::ExprIf(ref e) => e.return_type.clone(),
      ExprType::ExprBlock(ref e) => e.get_return_type(),
      ExprType::ExprStruct(ref s) => ReturnType::Some(vec![s.name.clone()]),
      ExprType::ExprField(ref e) => e.return_type.clone(),
    }
  }

  pub fn identify_types(&mut self, krate: &CrateType, scope: &mut ExprScope) {
    match *self {
      ExprType::ExprMac(ref mut e) => e.identify_types(),
      ExprType::DeclLocal(ref mut e) => e.identify_types(krate, scope),
      ExprType::ExprRet(_) => (),
      ExprType::ExprLit(ref mut e) => e.identify_types(),
      ExprType::ExprCall(ref mut e) => e.identify_types(krate, scope),
      ExprType::ExprPath(_) => (),
      ExprType::ExprAssign(ref mut e) => e.identify_types(krate, scope),
      ExprType::ExprBinaryOp(ref mut e) => e.identify_types(krate, scope),
      ExprType::ExprIf(ref mut e) => e.identify_types(krate, scope),
      ExprType::ExprBlock(ref mut e) => e.identify_types(krate, scope),
      ExprType::ExprStruct(_) => (),
      ExprType::ExprField(ref mut e) => e.identify_types(krate, scope),
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
    let c = if self.return_type.is_known() { 0 } else { 1 };
    c + self.cond.unknown_type_count() + self.true_block.unknown_type_count()
  }

  pub fn identify_types(&mut self, krate: &CrateType, scope: &mut ExprScope) {
    self.cond.identify_types(krate, scope);
    self.true_block.identify_types(krate, scope);
    self.return_type = self.return_type.clone();
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
      ExprLitType::LitInteger(_, ref t) => if t.is_known() { 0 } else { 1 },
      _ => 0,
    }
  }

  fn get_return_type(&self) -> ReturnType {
    match *self {
      ExprLitType::LitBool(_) => ReturnType::Some(vec!["bool".to_owned()]),
      ExprLitType::LitStr(_) => ReturnType::Some(vec!["&str".to_owned()]),
      ExprLitType::LitInteger(_, ref t) => t.clone(),
    }
  }

  pub fn identify_types(&mut self) {
    match *self {
      ExprLitType::LitBool(_) => return,
      ExprLitType::LitStr(_) => return,
      ExprLitType::LitInteger(_, ref mut t) => match *t {
        ReturnType::Unknown => *t = ReturnType::Some(vec!["i16".to_owned()]),
        _ => return,
      }
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
    if self.return_type.is_known() { 0 } else { 1 }
  }

  fn find_return_type(&self, path: &[String], krate: &CrateType, _scope: &mut ExprScope) -> ReturnType {
    match path.len() {
      1 => match krate.get_function_with_name(&*path[0]) {
        Some(ref f) => match f.fn_decl.1 {
          Some(ref r) => ReturnType::Some(r.clone()),
          None => ReturnType::None,
        },
        None => panic!("{:?} {:?}", path, krate),
      },
      2 => match krate.get_method_with_name(&*path[0], &*path[1]) {
        Some(ref f) => match f.fn_decl.1 {
          Some(ref r) => ReturnType::Some(r.clone()),
          None => ReturnType::None,
        },
        None => panic!("{:?} {:?}", path, krate),
      },
      _ => panic!("{:?} {:?}", path, krate),
    }
  }

  pub fn identify_types(&mut self, krate: &CrateType, scope: &mut ExprScope) {
    let return_type = match self.return_type {
      ReturnType::Unknown => match self.function {
        ExprType::ExprPath(ref path) => self.find_return_type(path, krate, scope),
        ExprType::ExprField(ref f) => self.find_return_type(&{
          let mut v = match *f.obj {
            ExprType::ExprPath(ref p) => match scope.get(&p.join("::")) {
              ReturnType::Some(ref t) => t.clone(),
              ReturnType::None => panic!("{:?}", p),
              ReturnType::Unknown => return,
            },
            _ => panic!("{:?}", f),
          };
          v.push(f.path.to_owned());
          v
        }[..], krate, scope),
        _ => panic!("{:?}, {:?}", self.function, scope),
      },
      _ => return,
    };
    self.return_type = return_type;
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
    if self.return_type.is_known() { 0 } else { 1 }
  }

  pub fn identify_types(&mut self) {
    // TODO
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
  pub target: Box<ExprType>,
  pub source: Box<ExprType>,
}
impl ExprAssignType {
  fn from_tree<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    ExprAssignType {
      target: Box::new(ExprType::from_tree_r(&value.get_nodes()[0], return_type.clone())),
      source: Box::new(ExprType::from_tree_r(&value.get_nodes()[1], return_type)),
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    self.target.unknown_type_count() + self.source.unknown_type_count()
  }

  pub fn identify_types(&mut self, krate: &CrateType, scope: &mut ExprScope) {
    self.target.identify_types(krate, scope);
    self.source.identify_types(krate, scope);
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
    if self.return_type.is_known() { 0 } else { 1 }
  }

  pub fn identify_types(&mut self, krate: &CrateType, scope: &mut ExprScope) {
    if self.return_type.is_known() {
      return;
    }
    self.lhs.identify_types(krate, scope);
    self.rhs.identify_types(krate, scope);
    self.operation.get_return_type(self.lhs.get_return_type(), self.rhs.get_return_type());
  }
}

#[derive(Debug, Clone)]
pub struct ExprFieldType {
  pub return_type: ReturnType,
  pub obj: Box<ExprType>,
  pub path: String,
}

impl ExprFieldType {
  pub fn from_tree_r<T: TreeNode>(value: &T, return_type: ReturnType) -> Self {
    let nodes = value.get_nodes();
    ExprFieldType {
      return_type: return_type,
      obj: Box::new(ExprType::from_tree_r(&nodes[0], ReturnType::Unknown)),
      path: nodes[1].get_node_by_name("ident").unwrap()
        .get_string_nodes().join(""),
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    self.obj.unknown_type_count() + if self.return_type.is_known() { 0 } else { 1 }
  }

  pub fn identify_types(&mut self, krate: &CrateType, scope: &mut ExprScope) {
    self.obj.identify_types(krate, scope);
    match self.obj.get_return_type() {
      ReturnType::Some(ref t) => self.return_type = if t.len() == 1 {
        match scope.vars.get(&t[0]) {
          Some(t) => match *t {
            ReturnType::Some(ref struct_name) => match krate.get_property_type_for_field(struct_name, &*self.path) {
              Some(v) => ReturnType::Some(v),
              _ => ReturnType::Unknown,
            },
            _ => ReturnType::Unknown,
          },
          None => ReturnType::Unknown,
        }
      } else {
        ReturnType::Some(krate.get_property_type_for_field(t, &*self.path).unwrap())
      },
      ReturnType::Unknown => (),
      ReturnType::None => panic!("Expected obj to return something"),
    }
  }
}

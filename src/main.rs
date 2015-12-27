extern crate serde_json;

mod formatter;

use std::io;
use std::iter;
use std::fmt::Debug;

use serde_json::Value;

use formatter::format_str;

trait TreeNode : Debug + Sized {
  fn get_name(&self) -> String;
  fn get_nodes(&self) -> Vec<Self>;
  fn maybe_get_nodes(&self) -> Option<&Vec<Self>>;
  fn get_string_nodes(&self) -> Vec<String>;
  fn get_node_by_name(&self, name: &str) -> Option<&Self>;
  fn is_null(&self) -> bool;

  fn get_components_ident_joined(&self) -> String {
    self
      .get_node_by_name("components").unwrap()
      .get_node_by_name("ident").unwrap()
      .get_string_nodes().join("").to_owned()
  }
}

impl TreeNode for Value {
  fn get_name(&self) -> String {
    self.as_object().and_then(|map| map.get("name"))
      .and_then(|name| name.as_string())
      .unwrap_or("").to_owned()
  }

  fn get_node_by_name(&self, name: &str) -> Option<&Self> {
    self.as_object().and_then(|map| map.get("nodes")
        ).and_then(|nodes| nodes.as_array()
          ).and_then(|nodes| nodes.iter().find(|x| x.get_name() == name))
  }

  fn maybe_get_nodes(&self) -> Option<&Vec<Self>> {
    self.as_object().and_then(|map| map.get("nodes")
        ).and_then(|nodes| nodes.as_array())
  }

  fn get_nodes(&self) -> Vec<Self> {
    self.maybe_get_nodes().cloned().unwrap_or(Vec::new())
  }

  fn is_null(&self) -> bool {
    self.is_null()
  }

  fn get_string_nodes(&self) -> Vec<String> {
    self.as_object().and_then(|map| map.get("nodes")
        ).and_then(|nodes| nodes.as_array()
          ).unwrap_or(&Vec::new()).into_iter().filter_map(
            |val| val.as_string().map(|s| s.to_owned())).collect()
  }
}

trait RustToJs {
  fn to_js(&self, indent: usize) -> String;
}

#[derive(Debug, Clone)]
struct CrateType {
  inner_attrs: Vec<AttrType>,
  mod_items: Vec<ModItemType>,
}

impl CrateType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    let mod_items = value.get_nodes().into_iter().next().map(|items|
      items.get_nodes().into_iter().map(|item| ModItemType::from_tree(&item)).collect()).unwrap_or(Vec::new());
    CrateType {
      inner_attrs: vec![],
      mod_items: mod_items,
    }
  }
}

impl RustToJs for CrateType {
  fn to_js(&self, indent: usize) -> String {
    self.mod_items.iter().map(|item| item.to_js(indent)).collect::<Vec<_>>().join("\n")
  }
}

#[derive(Debug, Clone)]
struct AttrType {
  doc_comment: Option<String>,
}

#[derive(Debug, Clone)]
struct AttrsAndVisType {
  attrs: Option<String>,
  vis: String,
}

impl AttrsAndVisType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    let mut args = value.get_string_nodes();
    assert!(args.len() == 1 || args.len() == 2);
    let vis = args.pop().unwrap();
    AttrsAndVisType {
      attrs: args.pop(),
      vis: vis,
    }
  }
}

impl RustToJs for AttrsAndVisType {
  fn to_js(&self, _indent: usize) -> String {
    "".to_owned()
  }
}

#[derive(Debug, Clone)]
struct ModItemType {
  attrs_and_vis: AttrsAndVisType,
  item: ItemType,
}

impl ModItemType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "Item");
    let nodes = value.get_nodes();
    assert_eq!(nodes.len(), 2);
    ModItemType {
      attrs_and_vis: AttrsAndVisType::from_tree(&nodes[0]),
      item: ItemType::from_tree(&nodes[1]),
    }
  }
}

impl RustToJs for ModItemType {
  fn to_js(&self, indent: usize) -> String {
    self.item.to_js(indent)
  }
}

#[derive(Debug, Clone)]
enum ItemType {
  ItemFn(ItemFn),
  ItemMacro(MacroType),
}

impl ItemType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "ItemFn" => ItemType::ItemFn(ItemFn::from_tree(value)),
      "ItemMacro" => ItemType::ItemMacro(MacroType::from_tree(value)),
      _ => panic!("{:?}", value),
    }
  }
}

impl RustToJs for ItemType {
  fn to_js(&self, indent: usize) -> String {
    match *self {
      ItemType::ItemFn(ref i) => i.to_js(indent),
      ItemType::ItemMacro(ref i) => i.to_js(indent),
    }
  }
}

#[derive(Debug, Clone)]
struct ParameterType {
  name: String,
  parameter_type: String,
}

#[derive(Debug, Clone)]
struct ItemFn {
  name: String,
  generic_params: (Vec<String>, Vec<String>),
  fn_decl: (Vec<ParameterType>, Option<String>),
  inner_attrs_and_block: AttrsAndBlockType,
}

impl ItemFn {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "ItemFn");
    let name = value.get_node_by_name("ident").unwrap()
      .get_string_nodes().into_iter().next().unwrap();
    let fn_decl: (Vec<ParameterType>, Option<String>) = value.
      get_node_by_name("FnDecl").map(|node| {
          (node.get_node_by_name("Args")
           .map(|args|
             args.get_nodes().iter().map(|arg| {
               let name = arg
                 .get_node_by_name("PatLit").unwrap()
                 .get_components_ident_joined();
               let parameter_type = arg
                 .get_node_by_name("TySum").unwrap()
                 .get_node_by_name("TyPath").unwrap()
                 .get_components_ident_joined();
               ParameterType { name: name, parameter_type: parameter_type }
               }).collect()
             ).unwrap_or(vec![]),
           node.get_node_by_name("ret-ty")
             .and_then(|node| node.get_node_by_name("TyPath"))
             .map(|node| node.get_components_ident_joined())
           )
        }).unwrap_or((vec![], None));

    let return_type = fn_decl.1.clone();
    ItemFn {
      name: name,
      generic_params: (Vec::new(), Vec::new()),
      fn_decl: fn_decl,
      inner_attrs_and_block: AttrsAndBlockType::from_tree(value, return_type),
    }
  }
}

impl RustToJs for ItemFn {
  fn to_js(&self, indent: usize) -> String {
    let bl = self.inner_attrs_and_block.to_js(indent + 1);
    format!("function {}({}) {}\n{}{}{}",
        self.name,
        self.fn_decl.0.iter().map(|p| p.name.clone()).collect::<Vec<_>>().join(", "),
        "{",
        bl,
        if bl.len() == 0 { "" } else { ";\n" },
        "}"
        )
  }
}

#[derive(Debug, Clone)]
struct BlockType {
  stmts: Vec<ExprType>,
  return_type: Option<String>,
}

impl BlockType {
  fn from_tree<T: TreeNode>(value: &T, return_type: Option<String>) -> Self {
    BlockType {
      stmts: value.get_node_by_name("ExprBlock")
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

impl RustToJs for BlockType {
  fn to_js(&self, indent: usize) -> String {
    let count = self.stmts.len();
    self.stmts.iter().enumerate()
      .map(|(i, s)| if self.return_type.is_some() && i == count - 1 && !s.is_ret() {
          ExprRetType { value: Some(Box::new(s.clone())) }.to_js(indent)
      } else {
        s.to_js(indent)
      })
      .collect::<Vec<_>>()
      .join(";\n")
      .to_owned()
  }
}

#[derive(Debug, Clone)]
struct AttrsAndBlockType {
  attrs: Vec<AttrType>,
  block: BlockType
}

impl AttrsAndBlockType {
  fn from_tree<T: TreeNode>(value: &T, return_type: Option<String>) -> Self {
    AttrsAndBlockType {
      attrs: Vec::new(),
      block: BlockType::from_tree(value, return_type),
    }
  }
}

impl RustToJs for AttrsAndBlockType {
  fn to_js(&self, indent: usize) -> String {
    self.block.to_js(indent)
  }
}

#[derive(Debug, Clone)]
enum ExprType {
  ExprMac(MacroType),
  DeclLocal(DeclLocalType),
  ExprRet(ExprRetType),
  ExprLit(ExprLitType),
  ExprCall(Box<ExprCallType>),
  ExprPath(String),
}

impl ExprType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "ExprMac" => ExprType::ExprMac(MacroType::from_tree(&value.get_nodes()[0])),
      "DeclLocal" => ExprType::DeclLocal(DeclLocalType::from_tree(value)),
      "ExprRet" => ExprType::ExprRet(ExprRetType::from_tree(value.get_nodes().first())),
      "ExprLit" => ExprType::ExprLit(ExprLitType::from_tree(&value.get_nodes()[0])),
      "ExprCall" => ExprType::ExprCall(Box::new(ExprCallType::from_tree(value))),
      "ExprPath" => ExprType::ExprPath(value.get_components_ident_joined()),
      _ => panic!("{:?}", value),
    }
  }

  fn is_ret(&self) -> bool {
    match *self {
      ExprType::ExprRet(_) => true,
      _ => false,
    }
  }
}

impl RustToJs for ExprType {
  fn to_js(&self, indent: usize) -> String {
    match *self {
      ExprType::ExprMac(ref m) => m.to_js(indent),
      ExprType::DeclLocal(ref m) => m.to_js(indent),
      ExprType::ExprLit(ref m) => m.to_js(indent),
      ExprType::ExprRet(ref m) => m.to_js(indent),
      ExprType::ExprCall(ref e) => e.to_js(indent),
      ExprType::ExprPath(ref e) => e.clone(),
    }
  }
}

#[derive(Debug, Clone)]
struct ExprRetType {
  value: Option<Box<ExprType>>,
}

impl ExprRetType {
  fn from_tree<T: TreeNode>(value: Option<&T>) -> Self {
    ExprRetType {
      value: value.map(|node| Box::new(ExprType::from_tree(node)))
    }
  }
}

impl RustToJs for ExprRetType {
  fn to_js(&self, indent: usize) -> String {
    match self.value {
      Some(ref v) => format!("{}return {}",
          iter::repeat("  ").take(indent).collect::<Vec<_>>().join(""),
          v.to_js(indent),
          ),
      None => format!("{}return",
          iter::repeat("  ").take(indent).collect::<Vec<_>>().join(""),
          ),
    }
  }
}

#[derive(Debug, Clone)]
enum ExprLitType {
  LitInteger(String),
  LitStr(String),
}

impl ExprLitType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "LitInteger" => ExprLitType::LitInteger(value.get_string_nodes().join("").to_owned()),
      "LitStr" => ExprLitType::LitStr(value.get_string_nodes().join("").to_owned()),
      _ => panic!("{:?}", value),
    }
  }
}

impl RustToJs for ExprLitType {
  fn to_js(&self, _indent: usize) -> String {
    match *self {
      ExprLitType::LitInteger(ref e) => e.clone(),
      ExprLitType::LitStr(ref e) => e.clone(),
    }
  }
}

#[derive(Debug, Clone)]
struct ExprCallType {
  function: ExprType,
  parameters: Vec<ExprType>,
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

impl RustToJs for ExprCallType {
  fn to_js(&self, indent: usize) -> String {
    format!("{}({})",
        self.function.to_js(indent),
        self.parameters.iter().map(|p| p.to_js(indent + 1)).collect::<Vec<_>>().join(", ")
        )
  }
}

#[derive(Debug, Clone)]
enum BindingType {
  Ref,
  RefMut,
  Mut,
}

#[derive(Debug, Clone)]
enum PatType {
  PatLit(String),
  PatIdent(BindingType, String),
}

impl PatType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
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

  fn get_name(&self) -> &str {
    match *self {
      PatType::PatLit(ref s) => s,
      PatType::PatIdent(_, ref s) => s,
    }
  }
}


#[derive(Debug, Clone)]
struct DeclLocalType {
  pat: PatType,
  value_type: Option<String>,
  value: Option<Box<ExprType>>,
}

impl DeclLocalType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "DeclLocal");
    let pat = PatType::from_tree(&value.get_nodes()[0]);

    let initial_value = value.get_nodes()
      .get(2)
      .and_then(|node| if node.is_null() { None } else { Some(node) })
      .map(|node| Box::new(ExprType::from_tree(node)));
    DeclLocalType {
      pat: pat,
      value_type: None,
      value: initial_value,
    }
  }
}

impl RustToJs for DeclLocalType {
  fn to_js(&self, indent: usize) -> String {
    format!("{}var {}{}",
        iter::repeat("  ").take(indent).collect::<Vec<_>>().join(""),
        self.pat.get_name(),
        match self.value {
          Some(ref v) => format!(" = {}", v.to_js(indent)),
          None => "".to_owned(),
        }
        )
  }
}

#[derive(Debug)]
struct MacroType {
  path_expr: String,
  delimited_token_trees: [TokenTree; 3],
}

impl MacroType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
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

impl RustToJs for MacroType {
  fn to_js(&self, indent: usize) -> String {
    format!("{}{}",
        iter::repeat("  ").take(indent).collect::<Vec<_>>().join(""),
        match &*self.path_expr {
          "println" => format!("console.log({})", format_str(&self.delimited_token_trees[1])),
          _ => format!("{}({})",
            self.path_expr,
            self.delimited_token_trees[1].to_js(indent)
            )
        }
    )
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

#[derive(Debug, Clone)]
pub enum TokenTree {
  Tok(String),
  TokenTrees(Vec<TokenTree>),
}

impl TokenTree {
  fn get_string(&self) -> Option<&String> {
    match *self {
      TokenTree::Tok(ref s) => Some(s),
      _ => None,
    }
  }
}

impl TokenTree {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "TTTok" => TokenTree::Tok(value.get_string_nodes().join("")),
      "TokenTrees" => TokenTree::TokenTrees(value.get_nodes().iter().map(
            |node| TokenTree::from_tree(node)).collect()),
      _ => panic!("{:?}", value),
    }
  }
}

impl RustToJs for TokenTree {
  fn to_js(&self, indent: usize) -> String {
    match *self {
      TokenTree::Tok(ref s) => s.clone(),
      TokenTree::TokenTrees(ref v) => v.iter().map(|t| t.to_js(indent)).collect::<Vec<_>>().join(" "),
    }
  }
}

fn main() {
  let decoded: Value = serde_json::from_reader(io::stdin()).unwrap();
  let cr = CrateType::from_tree(&decoded);
  println!("{}", cr.to_js(0));
}

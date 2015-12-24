extern crate serde_json;

use std::io;
use std::fmt::Debug;

use serde_json::Value;

trait TreeNode : Debug + Sized {
  fn get_name(&self) -> String;
  fn get_nodes(&self) -> Vec<Self>;
  fn get_string_nodes(&self) -> Vec<String>;
  fn get_node_by_name(&self, name: &str) -> Option<&Self>;
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

  fn get_nodes(&self) -> Vec<Self> {
    self.as_object().and_then(|map| map.get("nodes")
        ).and_then(|nodes| nodes.as_array()
          ).cloned().unwrap_or(Vec::new())
  }

  fn get_string_nodes(&self) -> Vec<String> {
    self.as_object().and_then(|map| map.get("nodes")
        ).and_then(|nodes| nodes.as_array()
          ).unwrap_or(&Vec::new()).into_iter().filter_map(
            |val| val.as_string().map(|s| s.to_owned())).collect()
  }
}

trait RustToJs {
  fn from_tree<T: TreeNode>(value: &T) -> Self;
}

#[derive(Debug)]
struct CrateType {
  shebang: Option<MetaItemType>,
  inner_attrs: Vec<AttrType>,
  mod_items: Vec<ModItemType>,
}

impl RustToJs for CrateType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    let mod_items = value.get_nodes().into_iter().next().map(|items|
      items.get_nodes().into_iter().map(|item| ModItemType::from_tree(&item)).collect()).unwrap_or(Vec::new());
    // println!("{:?}", mod_items);
    CrateType {
      shebang: None,
      inner_attrs: vec![],
      mod_items: mod_items,
    }
  }
}

#[derive(Debug)]
struct AttrType {
  shebang: Option<MetaItemType>,
  doc_comment: Option<String>,
}

#[derive(Debug)]
enum MetaItemType {
  MetaWord(String),
  MetaNameValue(String, String),
  MetaList(String, Vec<String>),
}

#[derive(Debug)]
struct AttrsAndVisType {
  attrs: Option<String>,
  vis: String,
}

impl RustToJs for AttrsAndVisType {
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

#[derive(Debug)]
struct ModItemType {
  attrs_and_vis: AttrsAndVisType,
  item: ItemType,
}

impl RustToJs for ModItemType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "Item");
    let nodes = value.get_nodes();
    assert_eq!(nodes.len(), 2);
    ModItemType {
      attrs_and_vis: AttrsAndVisType::from_tree(&nodes[0]),
      item: ItemType::ItemFn(ItemFn::from_tree(&nodes[1])),
    }
  }
}

#[derive(Debug)]
enum ItemType {
  ItemFn(ItemFn)
}

#[derive(Debug)]
struct ParameterType {
  name: String,
  parameter_type: String,
}

#[derive(Debug)]
struct ItemFn {
  name: String,
  generic_params: (Vec<String>, Vec<String>),
  fn_decl: (Vec<ParameterType>, Option<String>),
  inner_attrs_and_block: AttrsAndBlockType,
}

impl RustToJs for ItemFn {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "ItemFn");
    let name = value.get_node_by_name("ident").unwrap()
      .get_string_nodes().into_iter().next().unwrap();
    let fn_decl: (Vec<ParameterType>, Option<String>) = value.
      get_node_by_name("FnDecl").map(|_node| { (vec![], None)
        }).unwrap_or((vec![], None));
    ItemFn {
      name: name,
      generic_params: (Vec::new(), Vec::new()),
      fn_decl: fn_decl,
      inner_attrs_and_block: AttrsAndBlockType {
        attrs: Vec::new(),
        stmts: value.get_node_by_name("ExprBlock")
          .and_then(|node| node.get_node_by_name("stmts"))
          .map(|node| node.get_nodes().iter().map(|node| StmtType::from_tree(node)).collect()
              ).unwrap_or(Vec::new()),
      },
    }
  }
}

#[derive(Debug)]
struct AttrsAndBlockType {
  attrs: Vec<AttrType>,
  stmts: Vec<StmtType>,
}

#[derive(Debug)]
enum StmtType {
  ExprMac(MacroExprType),
}

impl RustToJs for StmtType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "ExprMac" => StmtType::ExprMac(MacroExprType::from_tree(&value.get_nodes()[0])),
      _ => panic!("{:?}", value),
    }
  }
}

#[derive(Debug)]
struct MacroExprType {
  path_expr: String,
  delimited_token_trees: [TokenTree; 3],
}

impl RustToJs for MacroExprType {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    let path_expr = value
      .get_node_by_name("components").unwrap()
      .get_node_by_name("ident").unwrap()
      .get_string_nodes().join("").to_owned();
    let delimited_token_trees = value
      .get_node_by_name("TTDelim").unwrap()
      .get_nodes();
    MacroExprType {
      path_expr: path_expr,
      delimited_token_trees: [
        TokenTree::from_tree(&delimited_token_trees[0]),
        TokenTree::from_tree(&delimited_token_trees[1]),
        TokenTree::from_tree(&delimited_token_trees[2]),
      ]
    }
  }
}

#[derive(Debug)]
enum TokenTree {
  Tok(String),
  TokenTrees(Vec<TokenTree>),
}

impl RustToJs for TokenTree {
  fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "TTTok" => TokenTree::Tok(value.get_string_nodes().join("")),
      "TokenTrees" => TokenTree::TokenTrees(value.get_nodes().iter().map(
            |node| TokenTree::from_tree(node)).collect()),
      _ => panic!("{:?}", value),
    }
  }
}

fn main() {
  let decoded: Value = serde_json::from_reader(io::stdin()).unwrap();
  let cr = CrateType::from_tree(&decoded);
  println!("{:?}", cr);
}

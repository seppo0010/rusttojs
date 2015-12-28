use super::TreeNode;
use types::{AttrsAndVisType, AttrsAndBlockType, MacroType, ReturnType, AttrType};

#[derive(Debug, Clone)]
pub struct CrateType {
  pub inner_attrs: Vec<AttrType>,
  pub mod_items: Vec<ModItemType>,
}

impl CrateType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    let mod_items = value.get_nodes().into_iter().next().map(|items|
      items.get_nodes().into_iter().map(|item| ModItemType::from_tree(&item)).collect()).unwrap_or(Vec::new());
    CrateType {
      inner_attrs: vec![],
      mod_items: mod_items,
    }
  }
}

#[derive(Debug, Clone)]
pub struct ModItemType {
  pub attrs_and_vis: AttrsAndVisType,
  pub item: ItemType,
}

impl ModItemType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "Item");
    let nodes = value.get_nodes();
    assert_eq!(nodes.len(), 2);
    ModItemType {
      attrs_and_vis: AttrsAndVisType::from_tree(&nodes[0]),
      item: ItemType::from_tree(&nodes[1]),
    }
  }
}

#[derive(Debug, Clone)]
pub enum ItemType {
  ItemFn(ItemFn),
  ItemMacro(MacroType),
}

impl ItemType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "ItemFn" => ItemType::ItemFn(ItemFn::from_tree(value)),
      "ItemMacro" => ItemType::ItemMacro(MacroType::from_tree(value)),
      _ => panic!("{:?}", value),
    }
  }
}

#[derive(Debug, Clone)]
pub struct ParameterType {
  pub name: String,
  pub parameter_type: String,
}

#[derive(Debug, Clone)]
pub struct ItemFn {
  pub name: String,
  pub generic_params: (Vec<String>, Vec<String>),
  pub fn_decl: (Vec<ParameterType>, Option<String>),
  pub inner_attrs_and_block: AttrsAndBlockType,
}

impl ItemFn {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
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

    let return_type = match fn_decl.1 {
      Some(ref v) => ReturnType::Some(v.clone()),
      None => ReturnType::None,
    };
    ItemFn {
      name: name,
      generic_params: (Vec::new(), Vec::new()),
      fn_decl: fn_decl,
      inner_attrs_and_block: AttrsAndBlockType::from_tree(value, return_type),
    }
  }
}

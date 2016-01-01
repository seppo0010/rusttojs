use super::TreeNode;
use exprs::MacroType;
use types::{AttrsAndVisType, AttrsAndBlockType, ReturnType, AttrType};

#[derive(Debug, Clone)]
pub struct CrateType {
  pub inner_attrs: Vec<AttrType>,
  pub mod_items: Vec<ModItemType>,
}

impl CrateType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    let mod_items = value.get_nodes().into_iter().next().map(|items|
      items.get_nodes().into_iter().map(|item| ModItemType::from_tree(&item)).collect()).unwrap_or(Vec::new());
    let mut krate = CrateType {
      inner_attrs: vec![],
      mod_items: mod_items,
    };
    krate.identify_types();
    krate
  }

  pub fn unknown_type_count(&self) -> u32 {
    self.mod_items.iter().fold(0, |acc, item| acc + item.unknown_type_count())
  }

  pub fn identify_types(&mut self) {
    loop {
      let before = self.unknown_type_count();
      let mut mod_items = self.mod_items.clone();
      for mod_item in mod_items.iter_mut() {
        mod_item.identify_types(self);
      }
      self.mod_items = mod_items;
      let after = self.unknown_type_count();
      if after == 0 {
        break;
      }
      if before == after {
        panic!("Cannot identify {} types in {:?}", after, self);
      }
    }
  }

  pub fn get_by_name(&self, name: &str) -> Option<&ModItemType> {
    for item in self.mod_items.iter() {
      if item.get_name() == Some(name) {
        return Some(item)
      }
    }
    None
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

  pub fn unknown_type_count(&self) -> u32 {
    self.item.unknown_type_count()
  }

  fn identify_types(&mut self, krate: &CrateType) {
    self.item.identify_types(krate);
  }

  fn get_name(&self) -> Option<&str> {
    self.item.get_name()
  }

  pub fn get_return_type_for_path(&self, path: &[String]) -> ReturnType {
    if path.len() == 0 {
      match self.item {
        ItemType::ItemFn(ref f) => match f.fn_decl.1 {
          Some(ref r) => ReturnType::Some(r.clone()),
          None => ReturnType::None,
        },
        _ => panic!("{:?} {:?}", self, path),
      }
    } else {
      panic!("{:?} {:?}", self, path);
    }
  }
}

#[derive(Debug, Clone)]
pub struct ImplMethodType {
  pub attrs_and_vis: AttrsAndVisType,
  pub is_static: bool,
  pub is_unsafe: bool,
  pub name: String,
  pub generic_params: (Vec<String>, Vec<String>),
  pub fn_decl: (Vec<ParameterType>, Option<Vec<String>>),
  pub where_clause: Vec<String>,
  pub inner_attrs_and_block: AttrsAndBlockType,
}

impl ImplMethodType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    let fn_decl_node = value.get_node_by_name("FnDecl").unwrap();
    let is_static = fn_decl_node.get_node_by_name("SelfStatic").is_some();
    let fn_decl: (Vec<ParameterType>, Option<Vec<String>>) = {
      (fn_decl_node.get_nodes()[0].get_nodes()[0].get_nodes().iter().map(|arg| {
         let name = arg
           .get_node_by_name("PatLit").unwrap()
           .get_components_ident_joined();
         let parameter_type = arg
           .get_node_by_name("TySum").unwrap()
           .get_node_by_name("TyPath").unwrap()
           .get_components_ident();
         ParameterType { name: name, parameter_type: parameter_type }
         }).collect(),
       fn_decl_node.get_node_by_name("ret-ty")
         .and_then(|node| node.get_node_by_name("TyPath"))
         .map(|node| node.get_components_ident())
       )
    };

    let return_type = match fn_decl.1 {
      Some(ref v) => ReturnType::Some(v.clone()),
      None => ReturnType::None,
    };

    ImplMethodType {
      attrs_and_vis: AttrsAndVisType::from_tree(value.get_node_by_name("AttrsAndVis").unwrap()),
      is_static: is_static,
      is_unsafe: !value.get_nodes()[1].is_null(),
      name: value.get_node_by_name("ident").unwrap()
        .get_string_nodes().into_iter().next().unwrap(),
      generic_params: (vec![], vec![]),
      where_clause: vec![],
      fn_decl: fn_decl,
      inner_attrs_and_block: AttrsAndBlockType::from_tree(value, return_type),
    }
  }
}

#[derive(Debug, Clone)]
pub enum ImplItemType {
  ImplMethod(ImplMethodType)
}

impl ImplItemType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "Method" => ImplItemType::ImplMethod(ImplMethodType::from_tree(value)),
      _ => panic!("{:?}", value),
    }
  }

  pub fn get_name(&self) -> String {
    match *self {
      ImplItemType::ImplMethod(ref t) => t.name.clone(),
    }
  }

  pub fn is_static(&self) -> bool {
    match *self {
      ImplItemType::ImplMethod(ref t) => t.is_static,
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    match *self {
      ImplItemType::ImplMethod(ref t) => t.inner_attrs_and_block.block.unknown_type_count(),
    }
  }

  fn identify_types(&mut self, krate: &CrateType) {
    match *self {
      ImplItemType::ImplMethod(ref mut t) => t.inner_attrs_and_block.block.identify_types(krate),
    }
  }
}

#[derive(Debug, Clone)]
pub struct ImplType {
  pub is_unsafe: bool,
  pub name: String,
  pub generic_params: (Vec<String>, Vec<String>),
  pub attr: AttrType,
  pub items: Vec<ImplItemType>
}

impl ImplType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    let nodes = value.get_nodes();
    ImplType {
      is_unsafe: !nodes[0].is_null(),
      name: value
        .get_node_by_name("TySum").unwrap()
        .get_node_by_name("TyPath").unwrap()
        .get_components_ident_joined(),
      generic_params: (vec![], vec![]),
      attr: AttrType { doc_comment: None },
      items: value.get_node_by_name("ImplItems")
        .map(|items| items.get_nodes().iter().map(|item| ImplItemType::from_tree(item)).collect())
        .unwrap_or(vec![])
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    self.items.iter().fold(0, |acc, item| acc + item.unknown_type_count())
  }

  pub fn identify_types(&mut self, krate: &CrateType) {
    for item in self.items.iter_mut() {
      item.identify_types(krate);
    }
  }
}

#[derive(Debug, Clone)]
pub struct StructType {
  pub name: String,
  pub generic_params: (Vec<String>, Vec<String>),
  pub where_clause: Vec<String>,
  pub fields: Vec<StructFieldType>,
}

#[derive(Debug, Clone)]
pub struct StructFieldType {
  pub attrs_and_vis: AttrsAndVisType,
  pub name: String,
  pub parameter_type: Vec<String>,
}

impl StructType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "ItemStruct");
    assert_eq!(value.get_nodes().len(), 4);
    let name = value.get_node_by_name("ident").unwrap()
      .get_string_nodes().into_iter().next().unwrap();
    StructType {
      name: name,
      generic_params: (vec![], vec![]),
      where_clause: vec![],
      fields: value.get_node_by_name("StructFields")
        .map(|fields| fields.get_nodes())
        .unwrap_or(vec![])
        .iter()
        .map(|node| {
          StructFieldType {
            attrs_and_vis: AttrsAndVisType::from_tree(node.get_node_by_name("AttrsAndVis").unwrap()),
            name: node
              .get_node_by_name("ident").unwrap()
              .get_string_nodes().join(""),
            parameter_type: node
              .get_node_by_name("TySum").unwrap()
              .get_node_by_name("TyPath").unwrap()
              .get_components_ident(),
          }
        })
        .collect(),
    }
  }
}

#[derive(Debug, Clone)]
pub enum ItemType {
  ItemFn(ItemFnType),
  ItemStruct(StructType),
  ItemMacro(MacroType),
  ItemImpl(ImplType),
  ViewItemExternCrate(ViewItemExternCrateType),
  ItemMod(ModType),
}

impl ItemType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    match &*value.get_name() {
      "ItemFn" => ItemType::ItemFn(ItemFnType::from_tree(value)),
      "ItemMacro" => ItemType::ItemMacro(MacroType::from_tree(value, ReturnType::None)),
      "ItemStruct" => ItemType::ItemStruct(StructType::from_tree(value)),
      "ItemImpl" => ItemType::ItemImpl(ImplType::from_tree(value)),
      "ViewItemExternCrate" => ItemType::ViewItemExternCrate(ViewItemExternCrateType::from_tree(value)),
      "ItemMod" => ItemType::ItemMod(ModType::from_tree(value)),
      _ => panic!("{:?}", value),
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    match *self {
      ItemType::ItemFn(ref i) => i.unknown_type_count(),
      ItemType::ItemStruct(_) => 0,
      ItemType::ItemMacro(_) => 0,
      ItemType::ItemImpl(ref i) => i.unknown_type_count(),
      ItemType::ViewItemExternCrate(_) => 0,
      ItemType::ItemMod(_) => 0,
    }
  }

  pub fn identify_types(&mut self, krate: &CrateType) {
    match *self {
      ItemType::ItemFn(ref mut i) => i.identify_types(krate),
      ItemType::ItemStruct(_) => (),
      ItemType::ItemMacro(_) => (),
      ItemType::ItemImpl(ref mut i) => i.identify_types(krate),
      ItemType::ViewItemExternCrate(_) => (),
      ItemType::ItemMod(_) => (),
    }
  }

  pub fn get_name(&self) -> Option<&str> {
    match *self {
      ItemType::ItemFn(ref i) => Some(&*i.name),
      _ => None,
    }
  }
}

#[derive(Debug, Clone)]
pub struct ParameterType {
  pub name: String,
  pub parameter_type: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ItemFnType {
  pub name: String,
  pub generic_params: (Vec<String>, Vec<String>),
  pub fn_decl: (Vec<ParameterType>, Option<Vec<String>>),
  pub inner_attrs_and_block: AttrsAndBlockType,
}

impl ItemFnType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "ItemFn");
    let name = value.get_node_by_name("ident").unwrap()
      .get_string_nodes().into_iter().next().unwrap();
    let fn_decl: (Vec<ParameterType>, Option<Vec<String>>) = value.
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
                 .get_components_ident();
               ParameterType { name: name, parameter_type: parameter_type }
               }).collect()
             ).unwrap_or(vec![]),
           node.get_node_by_name("ret-ty")
             .and_then(|node| node.get_node_by_name("TyPath"))
             .map(|node| node.get_components_ident())
           )
        }).unwrap_or((vec![], None));

    let return_type = match fn_decl.1 {
      Some(ref v) => ReturnType::Some(v.clone()),
      None => ReturnType::None,
    };
    ItemFnType {
      name: name,
      generic_params: (Vec::new(), Vec::new()),
      fn_decl: fn_decl,
      inner_attrs_and_block: AttrsAndBlockType::from_tree(value, return_type),
    }
  }

  pub fn unknown_type_count(&self) -> u32 {
    self.inner_attrs_and_block.block.unknown_type_count()
  }

  pub fn identify_types(&mut self, krate: &CrateType) {
    self.inner_attrs_and_block.block.identify_types(krate);
  }
}

#[derive(Debug, Clone)]
pub struct ViewItemExternCrateType {
  pub name: String,
  pub local_name: Option<String>,
}

impl ViewItemExternCrateType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    let nodes = value.get_nodes();
    assert!(nodes.len() == 1 || nodes.len() == 2);
    ViewItemExternCrateType {
      name: nodes[0].get_string_nodes().join(""),
      local_name: nodes.get(1).map(|node| node.get_string_nodes().join("")),
    }
  }
}

#[derive(Debug, Clone)]
pub struct ModType {
  pub name: String,
}

impl ModType {
  pub fn from_tree<T: TreeNode>(value: &T) -> Self {
    assert_eq!(value.get_name(), "ItemMod");
    ModType {
      name: value
        .get_nodes().iter()
        .map(|node|
          node.get_string_nodes().join("")
        )
        .collect::<Vec<_>>()
        .join("")
    }
  }
}

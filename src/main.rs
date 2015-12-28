extern crate serde_json;

mod exprs;
mod formatter;
mod items;
mod js;
mod types;

use std::io;
use std::fmt::Debug;

use serde_json::Value;

use items::CrateType;
use js::RustToJs;

pub trait TreeNode : Debug + Sized {
  fn get_name(&self) -> String;
  fn get_nodes(&self) -> Vec<Self>;
  fn maybe_get_nodes(&self) -> Option<&Vec<Self>>;
  fn get_string_nodes(&self) -> Vec<String>;
  fn get_node_by_name(&self, name: &str) -> Option<&Self>;
  fn is_null(&self) -> bool;

  fn get_components_ident(&self) -> Vec<String> {
    self
      .get_node_by_name("components").unwrap()
      .get_nodes().iter()
      .map(|node|
        node.get_string_nodes().join("")
      )
      .collect()
  }

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

fn main() {
  let decoded: Value = serde_json::from_reader(io::stdin()).unwrap();
  let cr = CrateType::from_tree(&decoded);
  println!("{}", cr.to_js(0));
}

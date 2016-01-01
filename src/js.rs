use std::iter;

use items::{CrateType, ModItemType, ImplItemType, ImplMethodType, ImplType, ItemType, ItemFnType, ModType, StructType, ViewItemExternCrateType};
use exprs::{ExprAssignType, ExprBinaryOpType, ExprType, ExprIfType, ExprRetType, ExprStructType, ExprLitType, ExprCallType, MacroType};
use formatter::format_str;
use types::{AttrsAndVisType, AttrsAndBlockType, BlockType, BinaryOperation, DeclLocalType, PatType, TokenTree};

const KEYWORDS: &'static [&'static str] = &[
  "abstract", "arguments", "boolean", "break", "byte",
  "case", "catch", "char", "class", "const",
  "continue", "debugger", "default", "delete", "do",
  "double", "else", "enum", "eval", "export",
  "extends", "false", "final", "finally", "float",
  "for", "function", "goto", "if", "implements",
  "import", "in", "instanceof", "int", "interface",
  "let", "long", "native", "new", "null",
  "package", "private", "protected", "public", "return",
  "short", "static", "super", "switch", "synchronized",
  "this", "throw", "throws", "transient", "true",
  "try", "typeof", "var", "void", "volatile",
  "while", "with", "yield",
];

fn escape(s: &str) -> String {
  if KEYWORDS.contains(&&*s) { format!("$rtj_{}", s) } else { s.to_owned() }
}

fn indentation(indent: usize) -> String {
  iter::repeat("  ").take(indent).collect::<Vec<_>>().join("")
}

pub trait RustToJs {
  fn to_js(&self, indent: usize) -> String;
}

impl RustToJs for ExprAssignType {
  fn to_js(&self, indent: usize) -> String {
      format!("{}{} = {}",
          indentation(indent),
          self.target.to_js(0),
          self.source.to_js(0))
  }
}

impl RustToJs for ExprBinaryOpType {
  fn to_js(&self, indent: usize) -> String {
    format!(
        "{}{} {} {}",
        indentation(indent),
        self.lhs.to_js(0),
        self.operation.to_js(0),
        self.rhs.to_js(0)
        )
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
      ExprType::ExprIf(ref e) => e.to_js(indent),
      ExprType::ExprAssign(ref e) => e.to_js(indent),
      ExprType::ExprStruct(ref e) => e.to_js(indent),
      ExprType::ExprBinaryOp(ref e) => e.to_js(indent),
      ExprType::ExprBlock(ref e) =>
        format!("(function() {}\n{}\n{}{})()",
            "{",
            e.to_js(indent + 1),
            indentation(indent),
            "}",
            ),
      ExprType::ExprPath(ref e) => e.iter().map(|s| escape(s)).collect::<Vec<_>>().join("."),
    }
  }
}

impl RustToJs for ExprIfType {
  fn to_js(&self, indent: usize) -> String {
    let true_block = self.true_block.to_js(indent + 1);
    format!("{}if ({}) {}\n{}{}{}{}",
          indentation(indent),
          self.cond.to_js(0),
          "{",
          true_block,
          if true_block.len() > 0 { "\n" } else { "" },
          indentation(indent),
          "}",
        )
  }
}

impl RustToJs for ExprStructType {
  fn to_js(&self, indent: usize) -> String {
    format!("{}return new {}({}\n{}\n{}{})",
        indentation(indent),
        self.name,
        "{",
        self.fields.iter().map(|field| format!("{}{}: {},",
            indentation(indent + 1),
            field.name,
            field.value.to_js(0),
            )).collect::<Vec<_>>().join("\n"),
        indentation(indent),
        "}",
        )
  }
}

impl RustToJs for ExprRetType {
  fn to_js(&self, indent: usize) -> String {
    match self.value {
      Some(ref v) => format!("{}return {}",
          indentation(indent),
          v.to_js(indent),
          ),
      None => format!("{}return",
          indentation(indent),
          ),
    }
  }
}

impl RustToJs for ExprLitType {
  fn to_js(&self, _indent: usize) -> String {
    match *self {
      ExprLitType::LitInteger(ref e, _) => e.clone(),
      ExprLitType::LitStr(ref e) => e.clone(),
      ExprLitType::LitBool(b) => { if b { "true" } else { "false" } }.to_owned()
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

impl RustToJs for CrateType {
  fn to_js(&self, indent: usize) -> String {
    self.mod_items.iter().map(|item| item.to_js(indent)).collect::<Vec<_>>().join("\n")
  }
}

impl RustToJs for ModItemType {
  fn to_js(&self, indent: usize) -> String {
    self.item.to_js(indent)
  }
}

impl RustToJs for ItemType {
  fn to_js(&self, indent: usize) -> String {
    match *self {
      ItemType::ItemFn(ref i) => i.to_js(indent),
      ItemType::ItemMacro(ref i) => i.to_js(indent),
      ItemType::ItemStruct(ref i) => i.to_js(indent),
      ItemType::ItemImpl(ref i) => i.to_js(indent),
      ItemType::ViewItemExternCrate(ref i) => i.to_js(indent),
      ItemType::ItemMod(ref i) => i.to_js(indent),
    }
  }
}

impl RustToJs for ItemFnType {
  fn to_js(&self, indent: usize) -> String {
    let bl = self.inner_attrs_and_block.to_js(indent + 1);
    format!("function {}({}) {}\n{}{}{}",
        self.name,
        self.fn_decl.0.iter().map(|p| p.name.clone()).collect::<Vec<_>>().join(", "),
        "{",
        bl,
        if bl.len() == 0 { "" } else { "\n" },
        "}"
        )
  }
}

impl RustToJs for AttrsAndVisType {
  fn to_js(&self, _indent: usize) -> String {
    "".to_owned()
  }
}

impl RustToJs for BlockType {
  fn to_js(&self, indent: usize) -> String {
    let count = self.stmts.len();
    self.stmts.iter().enumerate()
      .map(|(i, s)| {
        if self.get_return_type().is_some() && i == count - 1 && !s.is_ret() {
          format!("{};",
            ExprRetType::with_value(Some(Box::new(s.clone()))).to_js(indent))
        } else {
          format!("{}{}",
            s.to_js(indent),
            if s.use_semicolon() { ";" } else { "" }
            )
        }
      })
      .collect::<Vec<_>>()
      .join("\n")
      .to_owned()
  }
}

impl RustToJs for AttrsAndBlockType {
  fn to_js(&self, indent: usize) -> String {
    self.block.to_js(indent)
  }
}

impl RustToJs for BinaryOperation {
  fn to_js(&self, _indent: usize) -> String {
    match *self {
      BinaryOperation::BiOr => "||",
      BinaryOperation::BiAnd => "&&",
      BinaryOperation::BiEq => "===",
      BinaryOperation::BiNe => "!=",
      BinaryOperation::BiLt => "<",
      BinaryOperation::BiGt => ">",
      BinaryOperation::BiLe => "<=",
      BinaryOperation::BiGe => ">=",
      BinaryOperation::BiBitOr => "|",
      BinaryOperation::BiBitXor => "^",
      BinaryOperation::BiBitAnd => "&",
      BinaryOperation::BiShl => "",
      BinaryOperation::BiShr => "",
      BinaryOperation::BiAdd => "+",
      BinaryOperation::BiSub => "-",
      BinaryOperation::BiMul => "*",
      BinaryOperation::BiDiv => "/",
      BinaryOperation::BiRem => "%",
    }.to_owned()
  }
}

impl RustToJs for DeclLocalType {
  fn to_js(&self, indent: usize) -> String {
    format!("{}var {}{}",
        indentation(indent),
        match self.pat {
          PatType::PatLit(ref s) => escape(&*s),
          PatType::PatIdent(_, ref s) => s.clone(),
        },
        match self.value {
          Some(ref v) => format!(" = {}", v.to_js(indent)),
          None => "".to_owned(),
        }
        )
  }
}

impl RustToJs for MacroType {
  fn to_js(&self, indent: usize) -> String {
    let joined_path = self.path_expr.iter().map(|s| escape(s)).collect::<Vec<_>>().join(".");
    format!("{}{}",
        indentation(indent),
        match &*joined_path {
          "println" => format!("console.log({})", format_str(&self.delimited_token_trees[1])),
          _ => format!("{}({})",
            joined_path,
            self.delimited_token_trees[1].to_js(indent)
            )
        }
    )
  }
}

impl RustToJs for StructType {
  fn to_js(&self, indent: usize) -> String {
    format!("{}var {} = function(values) {}\n{}{};",
        indentation(indent),
        self.name,
        "{",
        self.fields.iter().map(|field| format!(
            "{}this.{} = values.{};\n",
            indentation(indent + 1),
            field.name,
            field.name
            )).collect::<Vec<_>>().join(""),
        "}",
    )
  }
}

impl RustToJs for ViewItemExternCrateType {
  fn to_js(&self, indent: usize) -> String {
    let local_name = self.local_name.clone().unwrap_or(self.name.clone());
    format!("{}var {} = require('{}');",
        indentation(indent),
        escape(&*local_name),
        self.name,
        )
  }
}

impl RustToJs for ImplItemType {
  fn to_js(&self, indent: usize) -> String {
    match *self {
      ImplItemType::ImplMethod(ref s) => s.to_js(indent),
    }
  }
}

impl RustToJs for ImplMethodType {
  fn to_js(&self, indent: usize) -> String {
    let bl = self.inner_attrs_and_block.to_js(indent + 1);
    format!("function {}({}) {}\n{}{}{}",
        escape(&*self.name),
        self.fn_decl.0.iter().map(|p| p.name.clone()).collect::<Vec<_>>().join(", "),
        "{",
        bl,
        if bl.len() == 0 { "" } else { "\n" },
        "}"
        )
  }
}

impl RustToJs for ImplType {
  fn to_js(&self, indent: usize) -> String {
    self.items.iter().map(|item| {
      let name = item.get_name();
        format!("{}.{}{} = {};",
          self.name,
          if item.is_static() { "" } else { "prototype." },
          escape(&*name),
          item.to_js(indent)
          )
        }).collect::<Vec<_>>().join("")
  }
}

impl RustToJs for ModType {
  fn to_js(&self, indent: usize) -> String {
    format!("{}var {} = require('./{}');",
      indentation(indent),
      escape(&*self.name),
      self.name,
      )
  }
}

impl RustToJs for TokenTree {
  fn to_js(&self, indent: usize) -> String {
    match *self {
      TokenTree::Tok(ref s) => escape(s),
      TokenTree::TokenTrees(ref v) => v.iter().map(|t| t.to_js(indent)).collect::<Vec<_>>().join(" "),
    }
  }
}

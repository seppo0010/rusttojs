use std::iter;

use items::{CrateType, ModItemType, ItemType, ItemFn};
use exprs::{ExprAssignType, ExprBinaryOpType, ExprType, ExprIfType, ExprRetType, ExprLitType, ExprCallType};
use formatter::format_str;
use types::{AttrsAndVisType, AttrsAndBlockType, BlockType, BinaryOperation, DeclLocalType, MacroType, PatType, TokenTree};

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
  format!("$rtj_{}", s)
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
      ExprType::ExprBinaryOp(ref e) => e.to_js(indent),
      ExprType::ExprBlock(ref e) =>
        format!("(function() {}\n{}\n{}{})()",
            "{",
            e.to_js(indent + 1),
            indentation(indent),
            "}",
            ),
      ExprType::ExprPath(ref e) => e.clone(),
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
      ExprLitType::LitInteger(ref e) => e.clone(),
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
        if self.return_type.is_some() && i == count - 1 && !s.is_ret() {
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
          PatType::PatLit(ref s) => if KEYWORDS.contains(&&**s) { escape(&*s) } else { s.clone() },
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
    format!("{}{}",
        indentation(indent),
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

impl RustToJs for TokenTree {
  fn to_js(&self, indent: usize) -> String {
    match *self {
      TokenTree::Tok(ref s) => if KEYWORDS.contains(&&**s) { escape(s) } else { s.clone() },
      TokenTree::TokenTrees(ref v) => v.iter().map(|t| t.to_js(indent)).collect::<Vec<_>>().join(" "),
    }
  }
}

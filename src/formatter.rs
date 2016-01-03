use types::TokenTree;
use js::RustToJs;

pub fn parse_arguments(args: &Vec<TokenTree>) -> Vec<TokenTree> {
  let positions = args.iter().enumerate().fold(Vec::new(),
      |mut positions, (index, ref item)| {
        if item.get_string().map(|s| &**s) == Some(",") {
          positions.push(index);
        }
        positions
      });

  positions.iter().zip(positions.iter().skip(1).map(
        |x| Some(*x)).chain(vec![None])).map(|(&start, end)| {
      match end {
        Some(e) => TokenTree::TokenTrees(args[start + 1..e].to_vec()),
        None => TokenTree::TokenTrees(args[start + 1..].to_vec()),
      }
      }).collect()
}

pub fn format_str(args: &TokenTree) -> String {
  match *args {
    TokenTree::Tok(ref t) => t.to_owned(),
    TokenTree::TokenTrees(ref v) => {
      let segments = v[0].get_string().unwrap().split(|c| c == '{' || c == '}').collect::<Vec<_>>();
      let arguments = parse_arguments(&v);
      segments.iter().enumerate().map(|(i, &s)| {
        if i % 2 == 0 {
          s.to_owned()
        } else {
          format!("\" + ({}) + \"", arguments[(i - 1) / 2].to_js(0))
        }
      }).collect::<Vec<_>>().join("")
    },
    TokenTree::Delim(ref d1, ref t, ref d2) => format!("{}{}{}", d1, format_str(t), d2),
    TokenTree::None => "".to_owned(),
  }
}

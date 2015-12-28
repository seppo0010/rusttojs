struct MyStruct {
  k: i64,
  k2: i64,
}

impl MyStruct {
  fn new(v: i64) -> Self {
    MyStruct {
      k: v,
      k2: v * 2,
    }
  }
}

fn main() {
  let v = MyStruct::new(1);
  println!("{}", v.k);
}

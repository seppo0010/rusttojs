struct MyStruct { n: i8 }

impl MyStruct {
  fn get(&self) -> i8 { self.n }
}

fn main() {
  let myvar = MyStruct { n: 3 };
  let v = myvar.get();
  println!("{}", v);
}

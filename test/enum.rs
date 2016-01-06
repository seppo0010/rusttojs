enum MyEnum {
  A(u32),
  B(i64),
  C(i64, i32),
}

impl MyEnum {
  fn print(&self) {
    match *self {
      MyEnum::A(ref s) => println!("{}", s),
      MyEnum::B(ref s) | MyEnum::C(ref s, _) => {
        let c = s + 1;
        println!("{}", c);
      },
    }
  }
}

fn main() {
  let a = MyEnum::A(3);
  a.print();
  let b = MyEnum::B(8);
  b.print();
  let c = MyEnum::C(123, 1);
  c.print();
}

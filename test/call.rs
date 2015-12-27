fn myfunc() -> i32 {
  123
}

fn myfunc2() -> i32 {
  return 456;
}

fn main() {
  let myvar = myfunc();
  let myvar2 = myfunc2();
  println!("{}, {}", myvar, myvar2);
}

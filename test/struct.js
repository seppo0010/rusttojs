var MyStruct = function(values) {
  this.k = values.k;
  this.k2 = values.k2;
};
MyStruct.$rtj_new = function $rtj_new(v) {
  return new MyStruct({
    k: v,
    k2: v * 2,
  });
};
function main() {
  var v = MyStruct.$rtj_new(1);
  console.log("" + (v . k) + "");
}

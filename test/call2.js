var MyStruct = function(values) {
  this.n = values.n;
};
MyStruct.prototype.get = function get() {
  return this.n;
};
function main() {
  var myvar = new MyStruct({
    n: 3,
  });
  var v = myvar.get();
  console.log("" + (v) + "");
}

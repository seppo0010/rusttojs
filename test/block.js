function main() {
  var b = (function() {
    var c = 1;
    return 2;
  })();
  console.log("" + (b) + "");
}

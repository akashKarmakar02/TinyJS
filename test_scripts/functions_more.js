let obj = { x: 7 };
function getX() { return this.x; }
obj.getX = getX;
console.log("dot =", obj.getX());
console.log("bracket =", obj["getX"]());

let inc = function(a) { return a + 1; };
console.log("inc =", inc(2));

let fact = function fact(n) {
  if (n <= 1) return 1;
  return n * fact(n - 1);
};
console.log("fact =", fact(5));


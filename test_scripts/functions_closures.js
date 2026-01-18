function add(a, b) { return a + b; }
console.log("add =", add(10, 20));

function makeAdder(x) {
  function inner(y) { return x + y; }
  return inner;
}

let add7 = makeAdder(7);
console.log("add7(5) =", add7(5));

function getThis() { return this; }
console.log("this_is_undefined =", getThis() === undefined);
console.log("this_is_globalThis =", getThis() === globalThis);
console.log(add.toString())
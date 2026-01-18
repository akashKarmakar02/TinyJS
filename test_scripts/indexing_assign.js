let obj = { a: 0 };
obj.a = 1;
obj["b"] = 2;
console.log("obj =", obj.a + obj["b"]); // 3

let arr = [0];
arr[0] = 5;
arr[1] = 6;
console.log("arr =", arr[0] + arr[1]); // 11


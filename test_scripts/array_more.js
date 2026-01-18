let a = [1, 2, 3, 2];

console.log("indexOf_2 =", a.indexOf(2));
console.log("indexOf_2_from2 =", a.indexOf(2, 2));
console.log("indexOf_2_fromNeg2 =", a.indexOf(2, -2));
console.log("indexOf_9 =", a.indexOf(9));

console.log("includes_3 =", a.includes(3));
console.log("includes_9 =", a.includes(9));
console.log("includes_2_from3 =", a.includes(2, 3));
console.log("includes_2_fromNeg1 =", a.includes(2, -1));

let nans = [0, 1, 0 / 0];
console.log("indexOf_nan =", nans.indexOf(0 / 0));
console.log("includes_nan =", nans.includes(0 / 0));

let nums: any = [1, 2, 3];

console.log("push_ret =", nums.push(4));
console.log("after_push =", nums);

console.log("pop_ret =", nums.pop());
console.log("after_pop =", nums);

let sum: any = 0;
nums.forEach(function (n: any) {
    sum += n;
});
console.log("sum_forEach =", sum);

let doubled: any = nums.map(function (n: any) {
    return n * 2;
});
console.log("doubled =", doubled);

console.error("stderr =", "hello from console.error");

console.log("str_len =", "hello".length);
console.log("Math.PI =", Math.PI);
console.log("Math.abs =", Math.abs(-3));
console.log("Math.floor =", Math.floor(1.9));
console.log("Math.ceil =", Math.ceil(1.1));
console.log("Math.max =", Math.max(1, 5, 2));
console.log("Math.min =", Math.min(1, 5, 2));

console.log("Number(\" 12.5 \") =", Number(" 12.5 "));
console.log("Number(\"x\") =", Number("x"));

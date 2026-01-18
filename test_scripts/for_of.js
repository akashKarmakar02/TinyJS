let nums = [1, 2, 3, 4];
let sum = 0;

for (const n of nums) {
  if (n % 2 === 0) sum = sum + n;
}

console.log("sum =", sum); // 6


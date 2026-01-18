let nums: number[] = [1, 2, 3, 4];
let sum: number = 0;

for (const n of nums) {
  if (n % 2 === 0) sum = sum + n;
}

console.log("sum =", sum as number); // 6


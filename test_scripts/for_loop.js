let sum = 0;

for (let i = 0; i < 10; i++) {
  if (i == 3) continue;
  if (i == 6) break;
  sum = sum + i;
}

console.log("sum =", sum); // 12


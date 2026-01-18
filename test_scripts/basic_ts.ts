// Type annotations and `as` should be stripped by TinyJS.
let n: number = 10;
let msg: string = ("Even numbers <= " + n) as string;
console.log(msg);

while (n > 0) {
  if (n % 2 === 0) {
    console.log("Even:", n);
  }
  n--;
}


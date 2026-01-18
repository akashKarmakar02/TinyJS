console.log("Number.isNaN(NaN) =", Number.isNaN(0 / 0));
console.log("Number.isNaN(\"x\") =", Number.isNaN("x"));

console.log("Number.isFinite(1) =", Number.isFinite(1));
console.log("Number.isFinite(NaN) =", Number.isFinite(0 / 0));
console.log("Number.isFinite(Infinity) =", Number.isFinite(1 / 0));

console.log("Number.parseInt(\"12.5\") =", Number.parseInt("12.5"));
console.log("Number.parseInt(\"ff\", 16) =", Number.parseInt("ff", 16));
console.log("Number.parseFloat(\" 12.5px \") =", Number.parseFloat(" 12.5px "));

console.log("Number(\" 12.5 \") =", Number(" 12.5 "));
console.log("Number.NaN =", Number.NaN);

import fs from "fs";

console.log("cwd =", fs.cwd());
console.log("exists TinyJS build file =", fs.existsSync("build.zig"));

let listing = fs.readdirSync(".");
console.log("dir =", listing);

// Writes a temp file inside test_scripts/
fs.writeFileSync("test_scripts/_tmp.txt", "hello from TinyJS\n");
console.log("read back =", fs.readFileSync("test_scripts/_tmp.txt"));


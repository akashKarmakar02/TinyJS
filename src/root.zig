//! TinyJS: a tiny, portable JavaScript/TypeScript runtime scaffold.
const std = @import("std");

pub const Runtime = @import("runtime.zig").Runtime;

pub fn installBuiltins(rt: *Runtime) !void {
    const builtins = @import("builtins/install.zig");
    try builtins.install(rt);
}

test "runs a tiny script" {
    const a = std.testing.allocator;

    var buf: [8192]u8 = undefined;
    var out_writer = std.io.Writer.fixed(buf[0..]);
    out_writer.end = 0;

    var runtime = try Runtime.init(a, &out_writer);
    defer runtime.deinit();
    try installBuiltins(&runtime);

    try runtime.runSource(
        \\let x = 1 + 2 * 3;
        \\let s = "hi " + "there";
        \\let o = { a: 1, b: 2 };
        \\let arr = [1, 2, 3];
        \\console.log("obj_toString =", o.toString());
        \\console.log("arr_toString =", arr.toString());
        \\let n = "n=" + 5;
        \\let t = !false;
        \\let z = !0;
        \\let c1 = 3 > 2;
        \\let c2 = 2 <= 1;
        \\let c3 = "a" < "b";
        \\let c4 = null == undefined;
        \\let c5 = 2 != 3;
        \\let c6 = -5 < 0;
        \\function add(a, b) { return a + b; }
        \\let sum = add(2, 3);
        \\function makeAdder(x) {
        \\  function addInner(y) { return x + y; }
        \\  return addInner;
        \\}
        \\let add2 = makeAdder(2);
        \\let add2res = add2(3);
        \\function getThis() { return this; }
        \\let th = getThis();
        \\let flag = 0;
        \\if (1 < 2) flag = 1; else flag = 2;
        \\let flag2 = 0;
        \\if (false) { flag2 = 1; } else { flag2 = 3; }
        \\let chain = 0;
        \\if (false) chain = 1; else if (true) chain = 2; else chain = 3;
        \\function retIf(x) { if (x) return 9; else return 10; }
        \\let rif1 = retIf(true);
        \\let rif2 = retIf(false);
        \\let i = 0;
        \\let sumw = 0;
        \\while (i < 10) {
        \\  i++;
        \\  if (i == 3) continue;
        \\  if (i == 6) break;
        \\  sumw = sumw + i;
        \\}
        \\let n2 = 10;
        \\n2--;
        \\let post = n2++;
        \\let pre = ++n2;
        \\let modv = 10 % 3;
        \\let strict1 = (2 === 2);
        \\let strict2 = (2 !== 3);
        \\let strict3 = (null === undefined);
        \\let strict4 = (null == undefined);
        \\let sumfor = 0;
        \\for (let j = 0; j < 10; j++) {
        \\  if (j == 3) continue;
        \\  if (j == 6) break;
        \\  sumfor += j;
        \\}
        \\let nums = [1, 2, 3, 4];
        \\let sumof = 0;
        \\for (const n of nums) {
        \\  if (n % 2 === 0) sumof = sumof + n;
        \\}
        \\let ca = 10;
        \\ca -= 3;
        \\ca *= 2;
        \\ca /= 7;
        \\let cm = 10;
        \\cm %= 3;
        \\let a_or = 0 || 5;
        \\let a_and = 0 && 5;
        \\let a_nullish = null ?? 7;
        \\let a_nullish2 = 0 ?? 7;
        \\let a_prec = 0 || 1 && 2;
        \\let obj2 = { a: 0 };
        \\obj2.a = 1;
        \\obj2["b"] = 2;
        \\let arr2 = [0];
        \\arr2[0] = 5;
        \\arr2[1] = 6;
        \\let arr2v = arr2[0] + arr2[1];
        \\let obj2v = obj2.a + obj2["b"];
        \\let obj3 = { x: 7 };
        \\function getX() { return this.x; }
        \\obj3.getX = getX;
        \\let this1 = obj3.getX();
        \\let this2 = obj3["getX"]();
        \\let fnexpr = function(a) { return a + 1; };
        \\let fnexprv = fnexpr(2);
        \\let fact = function fact(n) { if (n <= 1) return 1; return n * fact(n - 1); };
        \\let factv = fact(5);
        \\console.log("x =", x);
        \\console.log(s);
        \\console.log(o.a + o.b);
        \\console.log(arr);
        \\console.log(n);
        \\console.log("t =", t);
        \\console.log("z =", z);
        \\console.log("c1 =", c1);
        \\console.log("c2 =", c2);
        \\console.log("c3 =", c3);
        \\console.log("c4 =", c4);
        \\console.log("c5 =", c5);
        \\console.log("c6 =", c6);
        \\console.log("sum =", sum);
        \\console.log("add2res =", add2res);
        \\console.log("th_is_undef =", th == undefined);
        \\console.log("th_is_global =", th == globalThis);
        \\console.log("flag =", flag);
        \\console.log("flag2 =", flag2);
        \\console.log("chain =", chain);
        \\console.log("rif1 =", rif1);
        \\console.log("rif2 =", rif2);
        \\console.log("sumw =", sumw);
        \\console.log("n2 =", n2);
        \\console.log("post =", post);
        \\console.log("pre =", pre);
        \\console.log("modv =", modv);
        \\console.log("strict1 =", strict1);
        \\console.log("strict2 =", strict2);
        \\console.log("strict3 =", strict3);
        \\console.log("strict4 =", strict4);
        \\console.log("sumfor =", sumfor);
        \\console.log("sumof =", sumof);
        \\console.log("ca =", ca);
        \\console.log("cm =", cm);
        \\console.log("a_or =", a_or);
        \\console.log("a_and =", a_and);
        \\console.log("a_nullish =", a_nullish);
        \\console.log("a_nullish2 =", a_nullish2);
        \\console.log("a_prec =", a_prec);
        \\console.log("arr2v =", arr2v);
        \\console.log("obj2v =", obj2v);
        \\console.log("this1 =", this1);
        \\console.log("this2 =", this2);
        \\console.log("fnexprv =", fnexprv);
        \\console.log("factv =", factv);
        \\
    , "<test>");

    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "x = 7"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "hi there"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "\n3\n"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "[1, 2, 3]"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "obj_toString = [object Object]"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "arr_toString = [object Array]"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "n=5"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "t = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "z = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "c1 = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "c2 = false"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "c3 = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "c4 = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "c5 = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "c6 = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "sum = 5"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "add2res = 5"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "th_is_undef = false"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "th_is_global = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "flag = 1"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "flag2 = 3"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "chain = 2"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "rif1 = 9"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "rif2 = 10"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "sumw = 12"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "n2 = 11"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "post = 9"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "pre = 11"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "modv = 1"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "strict1 = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "strict2 = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "strict3 = false"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "strict4 = true"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "sumfor = 12"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "sumof = 6"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "ca = 2"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "cm = 1"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "a_or = 5"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "a_and = 0"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "a_nullish = 7"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "a_nullish2 = 0"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "a_prec = 2"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "arr2v = 11"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "obj2v = 3"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "this1 = 7"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "this2 = 7"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "fnexprv = 3"));
    try std.testing.expect(std.mem.containsAtLeast(u8, out_writer.buffered(), 1, "factv = 120"));
}

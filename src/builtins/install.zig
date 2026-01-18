const rtmod = @import("../runtime.zig");

const Runtime = rtmod.Runtime;
const Value = rtmod.Value;
const Object = rtmod.Object;

const console_b = @import("console.zig");
const fs_b = @import("fs.zig");
const array_b = @import("array.zig");
const function_b = @import("function.zig");
const math_b = @import("math.zig");
const number_b = @import("number.zig");

pub fn install(rt: *Runtime) !void {
    // Prototypes
    const object_proto = try rt.newObjectRaw(null);
    rt.object_prototype = object_proto;

    const function_proto = try rt.newObjectRaw(object_proto);
    rt.function_prototype = function_proto;
    try rt.putProp(function_proto, "toString", .{ .native_fn = .{ .func = rtmod.nativeFunctionToString } });
    try function_b.installPrototype(rt, function_proto);

    const array_proto = try rt.newObjectRaw(object_proto);
    rt.array_prototype = array_proto;
    try array_b.installPrototype(rt, array_proto);

    // Object / Function / Array "constructors" (not callable yet; used for prototype access).
    const object_ctor = try rt.newObjectRaw(function_proto);
    try rt.putProp(object_ctor, "prototype", .{ .object = object_proto });
    rt.builtin_object = .{ .object = object_ctor };
    try rt.define("Object", rt.builtin_object, true);

    const function_ctor = try rt.newObjectRaw(function_proto);
    try rt.putProp(function_ctor, "prototype", .{ .object = function_proto });
    rt.builtin_function = .{ .object = function_ctor };
    try rt.define("Function", rt.builtin_function, true);

    const array_ctor = try rt.newObjectRaw(function_proto);
    try rt.putProp(array_ctor, "prototype", .{ .object = array_proto });
    rt.builtin_array = .{ .object = array_ctor };
    try rt.define("Array", rt.builtin_array, true);

    // `Object.prototype.toString()`.
    try rt.putProp(object_proto, "toString", .{ .native_fn = .{ .func = rtmod.nativeObjectToString } });

    // `globalThis`
    const global_obj = try rt.newObjectRaw(object_proto);
    rt.global_this = .{ .object = global_obj };
    rt.current_this = rt.global_this;
    try rt.define("globalThis", rt.global_this, true);

    // `console`
    const console_obj = try rt.newObjectRaw(object_proto);
    try console_b.install(rt, console_obj);
    rt.builtin_console = .{ .object = console_obj };
    try rt.define("console", rt.builtin_console, true);

    // `fs`
    const fs_obj = try rt.newObjectRaw(object_proto);
    try fs_b.install(rt, fs_obj);
    rt.builtin_fs = .{ .object = fs_obj };
    try rt.define("fs", rt.builtin_fs, true);

    // `Math`
    const math_obj = try rt.newObjectRaw(object_proto);
    try math_b.install(rt, math_obj);
    rt.builtin_math = .{ .object = math_obj };
    try rt.define("Math", rt.builtin_math, true);

    // `Number`
    const number_fn = try rt.newNativeFunctionObject(number_b.nativeNumber, "Number");
    try number_b.install(rt, number_fn);
    rt.builtin_number = .{ .native_function = number_fn };
    try rt.define("Number", rt.builtin_number, true);

    // Attach globals onto `globalThis` for non-strict behavior.
    try rt.putProp(global_obj, "Object", rt.builtin_object);
    try rt.putProp(global_obj, "Function", rt.builtin_function);
    try rt.putProp(global_obj, "Array", rt.builtin_array);
    try rt.putProp(global_obj, "console", rt.builtin_console);
    try rt.putProp(global_obj, "fs", rt.builtin_fs);
    try rt.putProp(global_obj, "Math", rt.builtin_math);
    try rt.putProp(global_obj, "Number", rt.builtin_number);

    // Primitive-ish bindings (keep compatibility with current parser/tests).
    try rt.define("undefined", .undefined, true);
    try rt.define("null", .null, true);
    try rt.define("true", .{ .boolean = true }, true);
    try rt.define("false", .{ .boolean = false }, true);
}

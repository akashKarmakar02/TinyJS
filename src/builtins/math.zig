const std = @import("std");
const rtmod = @import("../runtime.zig");

const Runtime = rtmod.Runtime;
const Value = rtmod.Value;
const Object = rtmod.Object;

pub fn install(rt: *Runtime, math_obj: *Object) !void {
    try rt.putProp(math_obj, "PI", .{ .number = std.math.pi });
    try rt.putProp(math_obj, "abs", .{ .native_fn = .{ .func = nativeAbs } });
    try rt.putProp(math_obj, "floor", .{ .native_fn = .{ .func = nativeFloor } });
    try rt.putProp(math_obj, "ceil", .{ .native_fn = .{ .func = nativeCeil } });
    try rt.putProp(math_obj, "max", .{ .native_fn = .{ .func = nativeMax } });
    try rt.putProp(math_obj, "min", .{ .native_fn = .{ .func = nativeMin } });
}

fn expectNumber(args: []const Value, index: usize) !f64 {
    if (args.len <= index) return error.InvalidArguments;
    return switch (args[index]) {
        .number => |n| n,
        else => error.InvalidArguments,
    };
}

fn nativeAbs(_: *Runtime, _: Value, args: []const Value) !Value {
    const n = try expectNumber(args, 0);
    return .{ .number = @abs(n) };
}

fn nativeFloor(_: *Runtime, _: Value, args: []const Value) !Value {
    const n = try expectNumber(args, 0);
    return .{ .number = @floor(n) };
}

fn nativeCeil(_: *Runtime, _: Value, args: []const Value) !Value {
    const n = try expectNumber(args, 0);
    return .{ .number = @ceil(n) };
}

fn nativeMax(_: *Runtime, _: Value, args: []const Value) !Value {
    if (args.len == 0) return .{ .number = -std.math.inf(f64) };
    var best = try expectNumber(args, 0);
    for (args[1..]) |v| {
        const n = switch (v) { .number => |x| x, else => return error.InvalidArguments };
        if (n > best) best = n;
    }
    return .{ .number = best };
}

fn nativeMin(_: *Runtime, _: Value, args: []const Value) !Value {
    if (args.len == 0) return .{ .number = std.math.inf(f64) };
    var best = try expectNumber(args, 0);
    for (args[1..]) |v| {
        const n = switch (v) { .number => |x| x, else => return error.InvalidArguments };
        if (n < best) best = n;
    }
    return .{ .number = best };
}


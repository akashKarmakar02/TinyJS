const ast = @import("../ast.zig");
const rtmod = @import("../runtime.zig");

const Runtime = rtmod.Runtime;
const Value = rtmod.Value;
const Object = rtmod.Object;

pub fn installPrototype(rt: *Runtime, function_proto: *Object) !void {
    try rt.putProp(function_proto, "call", .{ .native_fn = .{ .func = nativeCall } });
}

fn nativeCall(rt: *Runtime, this_value: Value, args: []const Value) anyerror!Value {
    const target: Value = switch (this_value) {
        .function, .native_fn, .native_function => this_value,
        else => {
            rt.noteRuntimeErrorFmt(defaultSpan(), "TypeError: Function.prototype.call called on non-callable.", .{});
            return error.RuntimeError;
        },
    };

    const this_arg: Value = if (args.len >= 1) args[0] else .undefined;
    const call_args = if (args.len >= 2) args[1..] else &.{};
    return rt.callValueDirect(target, this_arg, call_args);
}

fn defaultSpan() ast.Span {
    return .{ .start = 0, .end = 0, .line = 1, .col = 1 };
}

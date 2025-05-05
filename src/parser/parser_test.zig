const std = @import("std");

const parser = @import("parser.zig");
const ast = @import("ast");
const lexer = @import("lexer");

test "let_one" {
    const test_allocator = std.testing.allocator;
    const input: []const u8 =
        \\ let x = 5;
        \\ let y = 10;
        \\ let foobar = 83838;
    ;

    const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, input);
    const prsr: *parser.Parser = parser.Parser.New(lxr);

    const program: *ast.Program = try prsr.ParseProgram(test_allocator);

    if (program.*.statements.len != 3) {
        std.debug.print("program doesn't contain 3 statements", .{});
        return;
    }

    const expectedIdentifier = [_][]const u8{ "x", "y", "foobar" };

    for (expectedIdentifier, 0..) |_, index| {
        _ = program.*.statements[index];
        // std.debug.print("{d} {s}", .{ identifier, stmt });
    }
}

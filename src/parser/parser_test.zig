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
    defer test_allocator.destroy(lxr);

    const prsr: *parser.Parser = try parser.Parser.New(test_allocator, lxr);
    defer test_allocator.destroy(prsr);

    const program: *ast.Program = try prsr.ParseProgram(test_allocator);
    defer program.deinit(ast.LetStatement, test_allocator);

    if (program.*.statements.len != 3) {
        std.debug.print("program doesn't contain 3 statements", .{});
        return;
    }

    const expectedIdentifier = [_][]const u8{ "x", "y", "foobar" };

    for (expectedIdentifier, 0..) |identifier, index| {
        const statement = program.*.statements[index];
        try testLetStatement(statement, identifier);
    }
}

fn testLetStatement(statement: ast.Statement, name: []const u8) !void {
    std.testing.expectEqualStrings(statement.tokenLiteral(), "LET") catch |err| {
        std.debug.print("The token literal is not LET", .{});
        return err;
    };

    const let_statement: *ast.LetStatement = @ptrCast(@alignCast(statement.ptr));

    std.testing.expectEqualStrings(let_statement.name.value, name) catch |err| {
        std.debug.print("the variable name is different, expected {s} got {s}", .{ name, let_statement.name.value });
        return err;
    };
}

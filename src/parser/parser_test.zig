const std = @import("std");

const parser = @import("parser.zig");
const ast = @import("ast");
const lexer = @import("lexer");

test "identifier_one" {
    const test_allocator = std.testing.allocator;
    const input: []const u8 =
        \\ foobar;
    ;
    const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, input);
    defer test_allocator.destroy(lxr);

    const prsr: *parser.Parser = try parser.Parser.New(test_allocator, lxr);
    defer prsr.deinit(test_allocator);

    const program: *ast.Program = try prsr.ParseProgram(test_allocator);
    defer program.deinit(test_allocator);

    try std.testing.expect(checkParserErrors(prsr));

    if (program.*.statements.len != 1) {
        std.debug.print("program doesn't contain 1 statements", .{});
        return;
    }

    for (program.*.statements) |stmt| {
        const expression_statement: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));
        const identifier: *ast.Identifier = @ptrCast(@alignCast(expression_statement.*.expression.?.ptr));
        std.testing.expectEqualStrings(identifier.value, "foobar") catch {
            std.debug.print("Identifier failed! expected {s} got {s}", .{ "foobar", identifier.value });
        };
    }
}

test "identifier_two" {
    const test_allocator = std.testing.allocator;
    const input: []const u8 =
        \\ 5;
    ;
    const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, input);
    defer test_allocator.destroy(lxr);

    const prsr: *parser.Parser = try parser.Parser.New(test_allocator, lxr);
    defer prsr.deinit(test_allocator);

    const program: *ast.Program = try prsr.ParseProgram(test_allocator);
    defer program.deinit(test_allocator);

    try std.testing.expect(checkParserErrors(prsr));

    if (program.*.statements.len != 1) {
        std.debug.print("program doesn't contain 1 statements", .{});
        return;
    }

    for (program.*.statements) |stmt| {
        const expression_statement: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));
        const integerLiteral: *ast.IntegerLiteral = @ptrCast(@alignCast(expression_statement.*.expression.?.ptr));
        std.testing.expectEqual(integerLiteral.value, @as(i64, 5)) catch {
            std.debug.print("Identifier failed! expected {d} got {d}", .{ 5, integerLiteral.value });
        };
    }
}

test "return_one" {
    const test_allocator = std.testing.allocator;
    const input: []const u8 =
        \\ return 10;
        \\ return 83838;
    ;

    const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, input);
    defer test_allocator.destroy(lxr);

    const prsr: *parser.Parser = try parser.Parser.New(test_allocator, lxr);
    defer prsr.deinit(test_allocator);

    const program: *ast.Program = try prsr.ParseProgram(test_allocator);
    defer program.deinit(test_allocator);

    try std.testing.expect(checkParserErrors(prsr));

    if (program.*.statements.len != 2) {
        std.debug.print("program doesn't contain 2 statements", .{});
        return;
    }

    for (program.*.statements) |stmt| {
        const return_statement: *ast.ReturnStatement = @ptrCast(@alignCast(stmt.ptr));
        std.testing.expectEqualStrings(return_statement.*.token.toString(), "RETURN") catch {
            std.debug.print("Expected {s} got {s}", .{ "RETURN", return_statement.*.token.toString() });
        };
    }
}

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
    defer prsr.deinit(test_allocator);

    const program: *ast.Program = try prsr.ParseProgram(test_allocator);
    defer program.deinit(test_allocator);

    try std.testing.expect(checkParserErrors(prsr));

    if (program.*.statements.len != 3) {
        std.debug.print("program doesn't contain 3 statements", .{});
        return;
    }

    const expectedIdentifier = [_][]const u8{ "x", "y", "foobar" };

    for (expectedIdentifier, 0..) |identifier, index| {
        const statement = program.*.statements[index];

        const statement_str = try statement.string(test_allocator);
        std.debug.print("{s}", .{statement_str});
        defer test_allocator.free(statement_str);

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

fn checkParserErrors(prsr: *parser.Parser) bool {
    const errors = prsr.*.errors;
    if (errors.items.len == 0) {
        return true;
    }

    std.debug.print("Parser has {d} errors.\n", .{errors.items.len});
    for (errors.items) |err| {
        std.debug.print("{s}\n", .{err});
    }

    return false;
}

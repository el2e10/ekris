const std = @import("std");

const parser = @import("parser.zig");
const ast = @import("ast");
const lexer = @import("lexer");

test "operator_precedence" {
    const OperatorPrecedenceTest = struct {
        input: []const u8,
        expected: []const u8,
    };

    const precedence_tests = [_]OperatorPrecedenceTest{
        .{
            .input = "true;",
            .expected = "true",
        },
        .{
            .input = "false;",
            .expected = "false",
        },
        .{
            .input = "3 > 5 == false;",
            .expected = "((3 > 5) == false)",
        },
        .{
            .input = "3 < 5 == true;",
            .expected = "((3 < 5) == true)",
        },
        .{
            .input = "1 + (2 + 3) + 4;",
            .expected = "((1 + (2 + 3)) + 4)",
        },
        .{
            .input = "2 / (5 + 5);",
            .expected = "(2 / (5 + 5))",
        },
        .{
            .input = "!(true == true);",
            .expected = "(! (true == true))",
        },
    };
    const test_allocator = std.testing.allocator;

    for (precedence_tests) |precedence_test| {
        const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, precedence_test.input);
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
            const expr_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.node.ptr));

            const expr_str = try stmt.string(test_allocator);
            defer test_allocator.free(expr_str);

            try std.testing.expectEqualStrings(precedence_test.expected, expr_str);

            if (expr_stmt.expression == null) {
                std.debug.print("The expression statement was null\n", .{});
            }
        }
    }
}

test "bool_expr" {
    const BooleanTest = struct {
        input: []const u8,
        expected: bool,
    };

    const bool_tests = [_]BooleanTest{
        .{
            .input = "true;",
            .expected = true,
        },
        .{
            .input = "false;",
            .expected = false,
        },
    };

    const test_allocator = std.testing.allocator;

    for (bool_tests) |bool_test| {
        const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, bool_test.input);
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
            const bool_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.node.ptr));

            const expr_str = try stmt.string(test_allocator);
            defer test_allocator.free(expr_str);

            if (bool_stmt.expression == null) {
                std.debug.print("The expression statement was null\n", .{});
            }
        }
    }
}
const ValueTypeTag = enum {
    int_val,
    bool_val,
    str_val,
};
const ValueType = union(ValueTypeTag) { int_val: i64, bool_val: bool, str_val: []const u8 };

test "infix_expr" {
    const InfixTest = struct {
        input: []const u8,
        operator: []const u8,
        left_value: ValueType,
        right_value: ValueType,
    };

    const infix_tests = [_]InfixTest{
        .{ .input = "15 * 38;", .operator = "*", .left_value = ValueType{ .int_val = 15 }, .right_value = ValueType{ .int_val = 38 } },
        .{ .input = "15 + 28;", .operator = "+", .left_value = ValueType{ .int_val = 15 }, .right_value = ValueType{ .int_val = 28 } },
        .{ .input = "false == true;", .operator = "==", .left_value = ValueType{ .bool_val = false }, .right_value = ValueType{ .bool_val = true } },
        .{ .input = "true == false;", .operator = "==", .left_value = ValueType{ .bool_val = true }, .right_value = ValueType{ .bool_val = false } },
    };

    const test_allocator = std.testing.allocator;

    for (infix_tests) |infix_test| {
        const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, infix_test.input);
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
            const expr_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.node.ptr));

            const expr_str = try stmt.string(test_allocator);
            defer test_allocator.free(expr_str);

            if (expr_stmt.expression == null) {
                std.debug.print("The expression statement was null\n", .{});
            }
            const expr: ast.Expression = expr_stmt.expression.?;
            try std.testing.expect(testInfixExpression(expr, infix_test.left_value, infix_test.operator, infix_test.right_value));
        }
    }
}

test "prefix_expr" {
    const PrefixTest = struct {
        input: []const u8,
        operator: []const u8,
        integer_value: i64,
    };

    const prefix_tests: [1]PrefixTest = [1]PrefixTest{.{ .input = "!5;", .operator = "!", .integer_value = 5 }};

    const test_allocator = std.testing.allocator;

    for (prefix_tests) |prefix_test| {
        const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, prefix_test.input);
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
            const expr_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.node.ptr));

            if (expr_stmt.expression == null) {
                std.debug.print("The expression statement was null\n", .{});
            }
            const expr: ast.Expression = expr_stmt.expression.?;

            const prefix_expression: *ast.PrefixExpression = @ptrCast(@alignCast(expr.ptr));
            try std.testing.expectEqualStrings(prefix_expression.operator, prefix_test.operator);
        }
    }
}

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
        \\ let foobar = true;
    ;

    const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, input);
    defer test_allocator.destroy(lxr);

    const prsr: *parser.Parser = try parser.Parser.New(test_allocator, lxr);
    defer prsr.deinit(test_allocator);

    const program: *ast.Program = try prsr.ParseProgram(test_allocator);
    defer program.deinit(test_allocator);

    try std.testing.expect(checkParserErrors(prsr));

    if (program.*.statements.len != 3) {
        std.debug.print("program doesn't contain {d} statements", .{3});
        return;
    }

    const expectedIdentifier = [_][]const u8{ "x", "y", "foobar" };

    for (expectedIdentifier, 0..) |identifier, index| {
        const statement = program.*.statements[index];

        const statement_str = try statement.string(test_allocator);
        defer test_allocator.free(statement_str);

        try testLetStatement(statement, identifier, test_allocator);
    }
}

fn testLetStatement(statement: ast.Statement, name: []const u8, allocator: std.mem.Allocator) !void {
    std.testing.expectEqualStrings(statement.tokenLiteral(), "LET") catch |err| {
        std.debug.print("The token literal is not LET", .{});
        return err;
    };

    const let_statement: *ast.LetStatement = @ptrCast(@alignCast(statement.ptr));

    std.testing.expectEqualStrings(let_statement.name.value, name) catch |err| {
        std.debug.print("the variable name is different, expected {s} got {s}", .{ name, let_statement.name.value });
        return err;
    };
    const str = try statement.string(allocator);
    defer allocator.free(str);
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

fn testIntegerLiteral(expression: ast.Expression, value: i64) bool {
    const int_expression: *ast.IntegerLiteral = @ptrCast(@alignCast(expression.ptr));

    std.testing.expectEqual(int_expression.value, value) catch |err| {
        std.debug.print("error occured {any}", .{err});
        return false;
    };
    return true;
}

fn testIdentifier(expression: ast.Expression, value: []const u8) bool {
    const identifier: *ast.Identifier = @ptrCast(@alignCast(expression.ptr));
    if (std.mem.eql(u8, identifier.value, value)) {
        return false;
    }
    return true;
}

fn testLiteralExpression(expression: ?ast.Expression, expected: ValueType) !bool {
    const expr: ast.Expression = expression orelse return error.NoExpressionError;
    return switch (expected) {
        ValueTypeTag.int_val => |val| testIntegerLiteral(expr, val),
        ValueTypeTag.str_val => |val| testIdentifier(expr, val),
        ValueTypeTag.bool_val => |val| testBooleanLiteral(expr, val),
    };
}

fn testInfixExpression(expression: ast.Expression, left: ValueType, operator: []const u8, right: ValueType) bool {
    const infix_expression: *ast.InfixExpression = @ptrCast(@alignCast(expression.ptr));

    var status: bool = testLiteralExpression(infix_expression.*.left_expression, left) catch |err| {
        std.debug.print("{any}, error occured", .{err});
        return false;
    };

    std.testing.expectEqualStrings(operator, infix_expression.operator) catch |err| {
        std.debug.print("{any}, {any} error occured", .{ infix_expression.operator, err });
        return false;
    };

    status = testLiteralExpression(infix_expression.*.right_expression, right) catch |err| {
        std.debug.print("{any}, error occured", .{err});
        return false;
    };
    return true;
}

fn testBooleanLiteral(expression: ast.Expression, value: bool) bool {
    const boolean_expression: *ast.BooleanLiteral = @ptrCast(@alignCast(expression.ptr));

    if (boolean_expression.value != value) {
        std.debug.print("got {any} expected {any}\n", .{ boolean_expression.value, value });
        return false;
    }

    // std.testing.expectEqualStrings(boolean_expression.tokenLiteral(), )

    return true;
}

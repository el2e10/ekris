const std = @import("std");
const token = @import("token");
const TokenType = token.TokenType;

pub const NodeVTable = struct {
    tokenLiteral: *const fn (ptr: *anyopaque) []const u8,
    string: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) anyerror![]const u8,
    deinit: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) void,
};

pub const Node = struct {
    ptr: *anyopaque,
    vtable: *const NodeVTable,

    pub fn tokenLiteral(self: Node) []const u8 {
        return self.vtable.tokenLiteral(self.ptr);
    }

    pub fn string(self: Node, allocator: std.mem.Allocator) ![]const u8 {
        return self.vtable.string(self.ptr, allocator);
    }

    pub fn deinit(self: Node, allocator: std.mem.Allocator) void {
        return self.vtable.deinit(self.ptr, allocator);
    }
};

pub const Statement = struct {
    ptr: *anyopaque,
    node: Node,

    pub fn tokenLiteral(self: Statement) []const u8 {
        return self.node.tokenLiteral();
    }

    pub fn string(self: Statement, allocator: std.mem.Allocator) ![]const u8 {
        return self.node.string(allocator);
    }

    pub fn deinit(self: Statement, allocator: std.mem.Allocator) void {
        self.node.deinit(allocator);
    }
};

pub const Program = struct {
    statements: []Statement,

    pub fn tokenLiteral(self: Program) !void {
        if (self.statements.len > 0) {
            return self.statements[0].node.tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        for (self.statements) |stmt| {
            stmt.deinit(allocator);
        }

        allocator.free(self.statements);
        allocator.destroy(self);
    }
};

pub const Expression = struct {
    ptr: *anyopaque,
    node: Node,

    pub fn tokenLiteral(self: Expression) []const u8 {
        return self.node.tokenLiteral();
    }

    pub fn string(self: Expression, allocator: std.mem.Allocator) ![]const u8 {
        return self.node.string(allocator);
    }

    pub fn deinit(self: Expression, allocator: std.mem.Allocator) void {
        self.node.deinit(allocator);
    }
};

pub const InfixExpression = struct {
    token: TokenType,
    operator: []const u8,
    left_expression: ?Expression,
    right_expression: ?Expression,

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *InfixExpression = @ptrCast(@alignCast(ptr));
        return self.*.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const self: *InfixExpression = @ptrCast(@alignCast(ptr));
        var left_expression: []const u8 = "";
        var right_expression: []const u8 = "";

        if (self.*.left_expression) |expr| {
            left_expression = try expr.string(allocator);
        }
        if (self.*.right_expression) |expr| {
            right_expression = try expr.string(allocator);
        }

        defer allocator.free(left_expression);
        defer allocator.free(right_expression);

        return try std.fmt.allocPrint(allocator, "({s} {s} {s})", .{ left_expression, self.*.operator, right_expression });
    }

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *InfixExpression = @ptrCast(@alignCast(ptr));
        if (self.*.right_expression) |expr| {
            expr.deinit(allocator);
        }
        if (self.*.left_expression) |expr| {
            expr.deinit(allocator);
        }
        allocator.free(self.*.operator);
        allocator.destroy(self);
    }

    fn createNode(self: *InfixExpression) Node {
        return Node{ .ptr = self, .vtable = &NodeVTable{
            .tokenLiteral = tokenLiteral,
            .string = string,
            .deinit = deinit,
        } };
    }

    pub fn createExpression(self: *InfixExpression) Expression {
        const node: Node = createNode(self);
        return Expression{
            .ptr = self,
            .node = node,
        };
    }
};

pub const PrefixExpression = struct {
    token: TokenType,
    operator: []const u8,
    right_expression: ?Expression,

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *PrefixExpression = @ptrCast(@alignCast(ptr));
        return self.*.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        var right_str: []const u8 = "";
        const self: *PrefixExpression = @ptrCast(@alignCast(ptr));
        if (self.*.right_expression) |right_expr| {
            right_str = try right_expr.string(allocator);
        }
        const str: []const u8 = try std.fmt.allocPrint(allocator, "({s} {s})", .{ self.*.token.toString(), right_str });
        defer allocator.free(right_str);
        return str;
    }

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *PrefixExpression = @ptrCast(@alignCast(ptr));
        if (self.*.right_expression) |expr| {
            allocator.free(self.*.operator);
            allocator.destroy(self);
            expr.deinit(allocator);
        }
    }

    fn createNode(self: *PrefixExpression) Node {
        return Node{ .ptr = self, .vtable = &NodeVTable{
            .tokenLiteral = tokenLiteral,
            .string = string,
            .deinit = deinit,
        } };
    }

    pub fn createExpression(self: *PrefixExpression) Expression {
        const node: Node = createNode(self);
        return Expression{
            .ptr = self,
            .node = node,
        };
    }
};

pub const Identifier = struct {
    token: TokenType,
    value: []const u8,

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *Identifier = @ptrCast(@alignCast(ptr));
        return self.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const self: *Identifier = @ptrCast(@alignCast(ptr));
        const identifier_str: []const u8 = try std.fmt.allocPrint(allocator, "{s}", .{self.value});
        return identifier_str;
    }
    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *Identifier = @ptrCast(@alignCast(ptr));
        allocator.free(self.*.value);
        allocator.destroy(self);
    }

    fn createNode(self: *Identifier) Node {
        return Node{ .ptr = self, .vtable = &NodeVTable{
            .tokenLiteral = tokenLiteral,
            .string = string,
            .deinit = deinit,
        } };
    }

    pub fn createExpression(self: *Identifier) Expression {
        const node: Node = createNode(self);
        return Expression{
            .ptr = self,
            .node = node,
        };
    }
};
pub const BooleanLiteral = struct {
    token: TokenType,
    value: bool,

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *BooleanLiteral = @ptrCast(@alignCast(ptr));
        std.debug.print("\n{}\n", .{self.value});
        return self.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const self: *BooleanLiteral = @ptrCast(@alignCast(ptr));
        const identifier_str: []const u8 = try std.fmt.allocPrint(allocator, "{}", .{self.value});
        return identifier_str;
    }

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *BooleanLiteral = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }

    fn createNode(self: *BooleanLiteral) Node {
        return Node{ .ptr = self, .vtable = &NodeVTable{
            .tokenLiteral = tokenLiteral,
            .string = string,
            .deinit = deinit,
        } };
    }

    pub fn createExpression(self: *BooleanLiteral) Expression {
        const node: Node = createNode(self);
        return Expression{
            .ptr = self,
            .node = node,
        };
    }
};

pub const IntegerLiteral = struct {
    token: TokenType,
    value: i64,

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *IntegerLiteral = @ptrCast(@alignCast(ptr));
        return self.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const self: *IntegerLiteral = @ptrCast(@alignCast(ptr));
        const identifier_str: []const u8 = try std.fmt.allocPrint(allocator, "{d}", .{self.value});
        return identifier_str;
    }

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *IntegerLiteral = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }

    fn createNode(self: *IntegerLiteral) Node {
        return Node{ .ptr = self, .vtable = &NodeVTable{
            .tokenLiteral = tokenLiteral,
            .string = string,
            .deinit = deinit,
        } };
    }

    pub fn createExpression(self: *IntegerLiteral) Expression {
        const node: Node = createNode(self);
        return Expression{
            .ptr = self,
            .node = node,
        };
    }
};

pub const LetStatement = struct {
    token: TokenType,
    name: Identifier,
    expression: ?Expression,

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *LetStatement = @ptrCast(@alignCast(ptr));
        return self.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const let_statement: *LetStatement = @ptrCast(@alignCast(ptr));
        var expression_str: []const u8 = "";
        if (let_statement.*.expression != null) {
            expression_str = try std.fmt.allocPrint(allocator, "{s}", .{try let_statement.*.expression.?.string(allocator)});
            std.debug.print("The expression value is {any}\n", .{expression_str});
        }
        defer allocator.free(expression_str);
        const statement_str: []const u8 = try std.fmt.allocPrint(allocator, "{s} {s} = {s};\n", .{ tokenLiteral(ptr), let_statement.name.value, expression_str });
        return statement_str;
    }

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *LetStatement = @ptrCast(@alignCast(ptr));
        allocator.free(self.*.name.value);
        allocator.destroy(self);
    }

    fn createNode(self: *LetStatement) Node {
        return Node{ .ptr = self, .vtable = &NodeVTable{
            .tokenLiteral = tokenLiteral,
            .string = string,
            .deinit = deinit,
        } };
    }

    pub fn createStatement(self: *LetStatement) Statement {
        const node = self.createNode();
        const statement = Statement{
            .ptr = self,
            .node = node,
        };
        return statement;
    }
};

pub const ReturnStatement = struct {
    token: TokenType,
    expression: ?Expression,

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *ReturnStatement = @ptrCast(@alignCast(ptr));
        return self.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const statement_str: []const u8 = try std.fmt.allocPrint(allocator, "{s} ;", .{tokenLiteral(ptr)});
        return statement_str;
    }

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *ReturnStatement = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }

    fn createNode(self: *ReturnStatement) Node {
        return Node{ .ptr = self, .vtable = &NodeVTable{
            .tokenLiteral = tokenLiteral,
            .string = string,
            .deinit = deinit,
        } };
    }

    pub fn createStatement(self: *ReturnStatement) Statement {
        const node = self.createNode();
        const statement = Statement{
            .ptr = self,
            .node = node,
        };
        return statement;
    }
};

// To handle expression like 4 + 3
pub const ExpressionStatement = struct {
    token: TokenType,
    expression: ?Expression,

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *ExpressionStatement = @ptrCast(@alignCast(ptr));
        if (self.expression) |exp| {
            exp.node.deinit(allocator);
        }
        allocator.destroy(self);
    }

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *ExpressionStatement = @ptrCast(@alignCast(ptr));
        return self.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const self: *ExpressionStatement = @ptrCast(@alignCast(ptr));
        std.debug.print("Expression statement is called\n", .{});
        var expression_str: []const u8 = "";
        if (self.*.expression) |expr| {
            expression_str = try expr.string(allocator);
        }
        defer allocator.free(expression_str);
        const statement_str: []const u8 = try std.fmt.allocPrint(allocator, "{s}", .{expression_str});
        return statement_str;
    }

    fn createNode(self: *ExpressionStatement) Node {
        return Node{
            .ptr = self,
            .vtable = &NodeVTable{
                .tokenLiteral = tokenLiteral,
                .string = string,
                .deinit = deinit,
            },
        };
    }

    pub fn createStatement(self: *ExpressionStatement) Statement {
        const node = self.createNode();
        const statement = Statement{
            .ptr = self,
            .node = node,
        };
        return statement;
    }
};

pub const IfExpression = struct {
    token: TokenType,
    condition: Expression,
    consequence: *BlockStatement,
    alternative: ?BlockStatement,

    pub fn deinit(ptr: *anyopaque, alloctor: std.mem.Allocator) void {
        const self: *IfExpression = @ptrCast(@alignCast(ptr));
        self.*.consequence.deinit(alloctor);
        if (self.*.alternative != null) {
            self.*.alternative.?.deinit(alloctor);
        }
        self.*.condition.deinit(alloctor);
        alloctor.destroy(self);
    }

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *IfExpression = @ptrCast(@alignCast(ptr));
        return self.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const self: *IfExpression = @ptrCast(@alignCast(ptr));
        var if_expr_str: []const u8 = "";

        const condition_str = try self.condition.string(allocator);
        defer allocator.free(condition_str);

        const consequence_str = try self.consequence.string(allocator);
        defer allocator.free(consequence_str);

        if_expr_str = try std.fmt.allocPrint(allocator, "if {s} {s}", .{ condition_str, consequence_str });

        if (self.alternative != null) {
            const alternative_str = try self.alternative.?.string(allocator);
            defer allocator.free(alternative_str);
            if_expr_str = try std.fmt.allocPrint(allocator, "{s} {s}", .{ if_expr_str, alternative_str });
        }
        return if_expr_str;
    }

    fn createNode(self: *IfExpression) Node {
        return Node{ .ptr = self, .vtable = &NodeVTable{
            .tokenLiteral = tokenLiteral,
            .string = string,
            .deinit = deinit,
        } };
    }

    pub fn createExpression(self: *IfExpression) Expression {
        const node = self.createNode();
        return Expression{
            .ptr = self,
            .node = node,
        };
    }
};

pub const BlockStatement = struct {
    token: TokenType,
    statements: []Statement,

    pub fn deinit(self: *BlockStatement, allocator: std.mem.Allocator) void {
        for (self.*.statements) |stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(self.statements);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *ExpressionStatement = @ptrCast(@alignCast(ptr));
        return self.token.toString();
    }

    pub fn string(self: *BlockStatement, allocator: std.mem.Allocator) ![]const u8 {
        var statement_str: []const u8 = "";
        for (self.*.statements) |stmt| {
            const tmp_str = try stmt.string(allocator);
            defer allocator.free(tmp_str);
            statement_str = try std.fmt.allocPrint(allocator, "{s}{s}", .{ statement_str, tmp_str });
        }
        return statement_str;
    }
};

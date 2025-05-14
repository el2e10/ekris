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

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const identifier: *Identifier = @ptrCast(@alignCast(ptr));
        allocator.destroy(identifier);
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
        const identifier_str: []const u8 = try std.fmt.allocPrint(allocator, "{s} {s}", .{ tokenLiteral(ptr), self.value });
        return identifier_str;
    }

    // fn createNode(self: *Identifier) Node {
    //     return Node{
    //         .ptr = self,
    //         .stringFn = string,
    //         .tokenLiteralFn = tokenLiteral,
    //     };
    // }
    //
    // pub fn createExpression(self: *Identifier) Expression {
    //     const node: Node = createNode(self);
    //     return Expression{
    //         .ptr = self,
    //         .node = node,
    //     };
    // }

    // pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
    //
    // }
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
        const statement_str: []const u8 = try std.fmt.allocPrint(allocator, "Let statement is {s} {s} = \n", .{ tokenLiteral(ptr), let_statement.name.value });
        return statement_str;
    }

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *LetStatement = @ptrCast(@alignCast(ptr));
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
        const statement_str: []const u8 = try std.fmt.allocPrint(allocator, "{s} ", .{tokenLiteral(ptr)});
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
            exp.deinit(ptr, allocator);
        }
    }

    pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
        const self: *ExpressionStatement = @ptrCast(@alignCast(ptr));
        return self.token.toString();
    }

    pub fn string(ptr: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const statement_str: []const u8 = try std.fmt.allocPrint(allocator, "{s} ", .{tokenLiteral(ptr)});
        return statement_str;
    }

    fn createNode(self: *ExpressionStatement) Node {
        return Node{
            .ptr = self,
            .tokenLiteralFn = tokenLiteral,
            .stringFn = string,
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

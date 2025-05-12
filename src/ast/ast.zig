const std = @import("std");
const token = @import("token");
const TokenType = token.TokenType;

pub const Node = struct {
    ptr: *anyopaque,
    tokenLiteralFn: *const fn (ptr: *anyopaque) []const u8,

    pub fn tokenLiteral(self: Node) []const u8 {
        return self.tokenLiteralFn(self.ptr);
    }
};

const Expression = struct {
    ptr: *anyopaque,
    node: Node,
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

    pub fn createNode(self: *Program) Node {
        return Node{
            .ptr = self,
            .tokenLiteralFn = tokenLiteral,
        };
    }

    pub fn deinit(self: *Program, comptime T: type, allocator: std.mem.Allocator) void {
        for (self.statements) |stmt| {
            const ptr: *T = @ptrCast(@alignCast(stmt.ptr));
            allocator.destroy(ptr);
        }

        allocator.free(self.statements);
        allocator.destroy(self);
    }
};

pub const Identifier = struct {
    token: TokenType,
    value: []const u8,

    pub fn tokenLiteral(self: Identifier) []const u8 {
        return self.token.toString();
    }
};

pub const Statement = struct {
    ptr: *anyopaque,
    node: Node,

    pub fn tokenLiteral(self: Statement) []const u8 {
        return self.node.tokenLiteral();
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

    fn createNode(self: *LetStatement) Node {
        return Node{
            .ptr = self,
            .tokenLiteralFn = tokenLiteral,
        };
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

    fn createNode(self: *ReturnStatement) Node {
        return Node{
            .ptr = self,
            .tokenLiteralFn = tokenLiteral,
        };
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

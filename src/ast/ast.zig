const std = @import("std");
const token = @import("token");

const Node = struct {
    ptr: *anyopaque,
    tokenLiteralFn: *const fn (ptr: *anyopaque) anyerror!void,

    fn tokenLiteral(self: Node) !void {
        return self.tokenLiteralFn();
    }
};

pub const Statement = struct {
    ptr: *anyopaque,
    node: Node,
    // statementNodeFn: *const fn (ptr: *anyopaque) anyerror!void,

    // fn statementNode(self: Statement) !void {
    //     return self.statementNodeFn();
    // }

    pub fn createNode(self: *Statement) Node {
        return Node{
            .ptr = self,
            .tokenLiteralFn = self.statementNodeFn,
        };
    }
};

const Expression = struct {
    ptr: *anyopaque,
    node: Node,
    // expressionNodeFn: *const fn (ptr: *anyopaque) anyerror!void,

    // fn expressionNode(self: Expression) !void {
    //     return self.expressionNodeFn();
    // }
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
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn tokenLiteral(self: Identifier) []const u8 {
        return self.token.Literal;
    }

    pub fn createNode(self: *Identifier) Node {
        return Node{
            .ptr = self,
            .tokenLiteralFn = tokenLiteral,
        };
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: Identifier,
    expression: Expression,

    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.Literal;
    }

    pub fn createNode(self: *LetStatement) Node {
        Node{
            .ptr = self,
            .tokenLiteralFn = tokenLiteral,
        };
    }
};

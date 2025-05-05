const std = @import("std");

const token = @import("token");
const ast = @import("ast");
const lexer = @import("lexer");

pub const Parser = struct {
    lexer: *lexer.Lexer,
    current_token: token.Token,
    peek_token: token.Token,

    pub fn New(lxr: *lexer.Lexer) *Parser {
        var parser = Parser{ .lexer = lxr, .current_token = lxr.NextToken(), .peek_token = lxr.NextToken() };
        return &parser;
    }

    fn nextToken(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.NextToken();
    }

    fn parseStatement(_: *Parser) ?ast.Statement {
        return null;
    }

    pub fn ParseProgram(self: *Parser, allocator: std.mem.Allocator) !*ast.Program {
        var program: ast.Program = undefined;
        var statementsArrayList = try std.ArrayList(ast.Statement).initCapacity(allocator, 100);
        defer statementsArrayList.deinit();

        while (!std.mem.eql(u8, self.current_token.Type, token.EOF)) {
            const parseStmt: ?ast.Statement = self.parseStatement();
            if (parseStmt) |stmt| {
                try statementsArrayList.append(stmt);
            }
            self.nextToken();
        }

        const statements = statementsArrayList.items;
        program = ast.Program{ .statements = statements };
        return &program;
    }
};

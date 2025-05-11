const std = @import("std");

const token = @import("token");
const ast = @import("ast");
const lexer = @import("lexer");

const TokenType = token.TokenType;

pub const Parser = struct {
    lexer: *lexer.Lexer,
    current_token: token.Token,
    peek_token: token.Token,

    pub fn New(allocator: std.mem.Allocator, lxr: *lexer.Lexer) !*Parser {
        const parser = try allocator.create(Parser);
        const current_token: token.Token = lxr.NextToken();
        const peek_token: token.Token = lxr.NextToken();
        parser.* = Parser{ .lexer = lxr, .current_token = current_token, .peek_token = peek_token };
        return parser;
    }

    fn nextToken(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.NextToken();
    }

    fn parseStatement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        return switch (self.current_token.Type) {
            TokenType.LET => try self.parseLetStatement(allocator),
            else => null,
        };
    }

    pub fn ParseProgram(self: *Parser, allocator: std.mem.Allocator) !*ast.Program {
        var statementsArrayList = std.ArrayList(ast.Statement).init(allocator);

        while (self.current_token.Type != TokenType.EOF) {
            const parseStmt: ?ast.Statement = try self.parseStatement(allocator);
            if (parseStmt) |stmt| {
                try statementsArrayList.append(stmt);
            } else {
                std.debug.print("The parsed token is null\n", .{});
            }
            self.nextToken();
        }

        const statements = try statementsArrayList.toOwnedSlice();

        // statementsArrayList.deinit();
        const program: *ast.Program = try allocator.create(ast.Program);
        program.* = ast.Program{ .statements = statements };
        return program;
    }

    fn parseLetStatement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        const let_token: TokenType = self.current_token.Type;

        if (!self.*.expectPeek(TokenType.IDENT)) {
            return null;
        }

        const variable_name: []const u8 = self.*.current_token.Literal;
        const identifier: ast.Identifier = ast.Identifier{ .token = TokenType.IDENT, .value = variable_name };

        if (!self.*.expectPeek(TokenType.ASSIGN)) {
            return null;
        }

        while (!self.*.currentTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        const let_statement: *ast.LetStatement = try allocator.create(ast.LetStatement);
        let_statement.* = ast.LetStatement{ .token = let_token, .name = identifier, .expression = null };
        return let_statement.createStatement();
    }

    fn currentTokenIs(self: Parser, token_type: TokenType) bool {
        return self.current_token.Type == token_type;
    }

    fn peekTokenIs(self: Parser, token_type: TokenType) bool {
        return self.peek_token.Type == token_type;
    }

    fn expectPeek(self: *Parser, token_type: TokenType) bool {
        if (self.*.peekTokenIs(token_type)) {
            self.*.nextToken();
            return true;
        } else {
            return false;
        }
    }
};

const std = @import("std");

const token = @import("token");
const ast = @import("ast");
const lexer = @import("lexer");

const TokenType = token.TokenType;

const PRECEDENCE = enum(u8) {
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
};

pub const Parser = struct {
    lexer: *lexer.Lexer,
    current_token: token.Token,
    peek_token: token.Token,
    errors: std.ArrayList([]const u8),
    prefix_parse_fns: std.AutoHashMap(token.TokenType, *const fn (ptr: *Parser, allocator: std.mem.Allocator) anyerror!ast.Expression),

    pub fn New(allocator: std.mem.Allocator, lxr: *lexer.Lexer) !*Parser {
        const parser = try allocator.create(Parser);
        const current_token: token.Token = lxr.NextToken();
        const peek_token: token.Token = lxr.NextToken();
        const errors = std.ArrayList([]const u8).init(allocator);

        var prefix_parse_fns = std.AutoHashMap(token.TokenType, *const fn (ptr: *Parser, allocator: std.mem.Allocator) anyerror!ast.Expression).init(allocator);
        try prefix_parse_fns.put(TokenType.IDENT, parseIdentifer);

        parser.* = Parser{ .lexer = lxr, .current_token = current_token, .peek_token = peek_token, .errors = errors, .prefix_parse_fns = prefix_parse_fns };
        return parser;
    }

    pub fn deinit(self: *Parser, allocator: std.mem.Allocator) void {
        for (self.*.errors.items) |err| {
            allocator.free(err);
        }
        self.*.errors.deinit();
        self.*.prefix_parse_fns.deinit();
        allocator.destroy(self);
    }

    fn nextToken(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.NextToken();
    }

    fn parseStatement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        return switch (self.current_token.Type) {
            TokenType.LET => try self.parseLetStatement(allocator),
            TokenType.RETURN => try self.parseReturnStatement(allocator),
            else => try self.parseExpressionStatement(allocator),
        };
    }

    pub fn ParseProgram(self: *Parser, allocator: std.mem.Allocator) !*ast.Program {
        var statementsArrayList = std.ArrayList(ast.Statement).init(allocator);

        while (self.current_token.Type != TokenType.EOF) {
            const parseStmt: ?ast.Statement = try self.parseStatement(allocator);
            if (parseStmt) |stmt| {
                try statementsArrayList.append(stmt);
            }
            self.nextToken();
        }

        const statements = try statementsArrayList.toOwnedSlice();

        const program: *ast.Program = try allocator.create(ast.Program);
        program.* = ast.Program{ .statements = statements };
        return program;
    }

    fn parseExpressionStatement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        const expression_token: TokenType = self.current_token.Type;
        const expression: ?ast.Expression = try self.parseExpression(PRECEDENCE.LOWEST, allocator);

        while (!self.*.currentTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        const expression_statement: *ast.ExpressionStatement = try allocator.create(ast.ExpressionStatement);
        expression_statement.* = ast.ExpressionStatement{ .token = expression_token, .expression = expression };

        return expression_statement.createStatement();
    }

    fn parseExpression(self: *Parser, _: PRECEDENCE, allocator: std.mem.Allocator) !?ast.Expression {
        const prefix_fn = self.prefix_parse_fns.get(self.*.current_token.Type);
        if (prefix_fn == null) {
            return null;
        }
        const leftExpression = prefix_fn.?(self, allocator);
        return try leftExpression;
    }

    fn parseIdentifer(self: *Parser, allocator: std.mem.Allocator) !ast.Expression {
        const identifier: *ast.Identifier = try allocator.create(ast.Identifier);
        identifier.* = ast.Identifier{ .token = self.current_token.Type, .value = self.current_token.Literal };

        return identifier.createExpression();
    }

    fn parseReturnStatement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        const return_token: TokenType = self.current_token.Type;

        self.nextToken();

        while (!self.*.currentTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        const return_statement: *ast.ReturnStatement = try allocator.create(ast.ReturnStatement);
        return_statement.* = ast.ReturnStatement{ .token = return_token, .expression = null };
        return return_statement.createStatement();
    }

    fn parseLetStatement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        const let_token: TokenType = self.current_token.Type;

        if (!self.*.expectPeek(TokenType.IDENT, allocator)) {
            return null;
        }

        const variable_name: []const u8 = self.*.current_token.Literal;
        const identifier: ast.Identifier = ast.Identifier{ .token = TokenType.IDENT, .value = variable_name };

        if (!self.*.expectPeek(TokenType.ASSIGN, allocator)) {
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

    pub fn peekErrors(self: *Parser, tkn: token.TokenType, allocator: std.mem.Allocator) !void {
        const err_msg = try std.fmt.allocPrint(allocator, "Expected next token to be {s} but got {s} instead", .{ tkn.toString(), self.peek_token.Type.toString() });
        // defer allocator.free(err_msg);

        try self.errors.append(err_msg);
    }

    fn expectPeek(self: *Parser, token_type: TokenType, allocator: std.mem.Allocator) bool {
        if (self.*.peekTokenIs(token_type)) {
            self.*.nextToken();
            return true;
        } else {
            self.peekErrors(token_type, allocator) catch {
                return false;
            };
            return false;
        }
    }
};

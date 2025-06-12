const std = @import("std");

const token = @import("token");
const ast = @import("ast");
const lexer = @import("lexer");

const TokenType = token.TokenType;

const Precedence = enum(u8) {
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
    prefix_parse_fns: std.AutoHashMap(token.TokenType, *const fn (ptr: *Parser, allocator: std.mem.Allocator) anyerror!?ast.Expression),
    infix_parse_fns: std.AutoHashMap(token.TokenType, *const fn (ptr: *Parser, left: ast.Expression, allocator: std.mem.Allocator) anyerror!?ast.Expression),
    precedences: std.AutoHashMap(TokenType, Precedence),

    pub fn New(allocator: std.mem.Allocator, lxr: *lexer.Lexer) !*Parser {
        const parser = try allocator.create(Parser);
        const current_token: token.Token = try lxr.NextToken(allocator);
        const peek_token: token.Token = try lxr.NextToken(allocator);
        const errors = std.ArrayList([]const u8).init(allocator);

        var prefix_parse_fns = std.AutoHashMap(token.TokenType, *const fn (ptr: *Parser, allocator: std.mem.Allocator) anyerror!?ast.Expression).init(allocator);
        try prefix_parse_fns.put(TokenType.IDENT, parseIdentifer);
        try prefix_parse_fns.put(TokenType.INT, parseIntegerLiteral);
        try prefix_parse_fns.put(TokenType.MINUS, parsePrefixExpression);
        try prefix_parse_fns.put(TokenType.BANG, parsePrefixExpression);
        try prefix_parse_fns.put(TokenType.TRUE, parseBooleanExpression);
        try prefix_parse_fns.put(TokenType.FALSE, parseBooleanExpression);
        try prefix_parse_fns.put(TokenType.LPAREN, parseGroupedExpression);
        try prefix_parse_fns.put(TokenType.IF, parseIfExpression);

        var infix_parse_fns = std.AutoHashMap(token.TokenType, *const fn (ptr: *Parser, left: ast.Expression, allocator: std.mem.Allocator) anyerror!?ast.Expression).init(allocator);
        try infix_parse_fns.put(TokenType.PLUS, parseInfixExpression);
        try infix_parse_fns.put(TokenType.MINUS, parseInfixExpression);
        try infix_parse_fns.put(TokenType.SLASH, parseInfixExpression);
        try infix_parse_fns.put(TokenType.ASTERISK, parseInfixExpression);
        try infix_parse_fns.put(TokenType.EQ, parseInfixExpression);
        try infix_parse_fns.put(TokenType.NOT_EQ, parseInfixExpression);
        try infix_parse_fns.put(TokenType.LT, parseInfixExpression);
        try infix_parse_fns.put(TokenType.GT, parseInfixExpression);

        var precedences = std.AutoHashMap(TokenType, Precedence).init(allocator);
        try precedences.put(TokenType.EQ, Precedence.EQUALS);
        try precedences.put(TokenType.NOT_EQ, Precedence.EQUALS);
        try precedences.put(TokenType.LT, Precedence.LESSGREATER);
        try precedences.put(TokenType.GT, Precedence.LESSGREATER);
        try precedences.put(TokenType.PLUS, Precedence.SUM);
        try precedences.put(TokenType.MINUS, Precedence.SUM);
        try precedences.put(TokenType.SLASH, Precedence.PRODUCT);
        try precedences.put(TokenType.ASTERISK, Precedence.PRODUCT);

        parser.* = Parser{ .lexer = lxr, .current_token = current_token, .peek_token = peek_token, .errors = errors, .prefix_parse_fns = prefix_parse_fns, .infix_parse_fns = infix_parse_fns, .precedences = precedences };
        return parser;
    }

    pub fn deinit(self: *Parser, allocator: std.mem.Allocator) void {
        for (self.*.errors.items) |err| {
            allocator.free(err);
        }
        self.*.errors.deinit();
        self.*.prefix_parse_fns.deinit();
        self.*.infix_parse_fns.deinit();
        self.*.precedences.deinit();
        allocator.destroy(self);
    }

    fn nextToken(self: *Parser, allocator: std.mem.Allocator) !void {
        allocator.free(self.*.current_token.Literal);
        self.current_token = self.peek_token;
        self.peek_token = try self.lexer.NextToken(allocator);
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
            try self.nextToken(allocator);
        }

        const statements = try statementsArrayList.toOwnedSlice();

        const program: *ast.Program = try allocator.create(ast.Program);
        program.* = ast.Program{ .statements = statements };
        return program;
    }

    fn parseExpressionStatement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        const expression_token: TokenType = self.current_token.Type;
        const expression: ?ast.Expression = try self.parseExpression(Precedence.LOWEST, allocator);

        while (!self.*.currentTokenIs(TokenType.SEMICOLON)) {
            try self.nextToken(allocator);
        }

        const expression_statement: *ast.ExpressionStatement = try allocator.create(ast.ExpressionStatement);
        expression_statement.* = ast.ExpressionStatement{ .token = expression_token, .expression = expression };

        return expression_statement.createStatement();
    }

    fn parseExpression(self: *Parser, precedence: Precedence, allocator: std.mem.Allocator) !?ast.Expression {
        const prefix_fn = self.prefix_parse_fns.get(self.*.current_token.Type);
        if (prefix_fn == null) {
            std.debug.print("Error happended {s}\n", .{self.current_token.Literal});
            try self.noPrefixParserFnError(self.*.current_token.Type, allocator);
            return null;
        }
        var leftExpression = try prefix_fn.?(self, allocator);
        if (leftExpression == null) {
            return null;
        }

        while (!self.*.peekTokenIs(TokenType.SEMICOLON) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
            const infix_fn = self.*.infix_parse_fns.get(self.*.peek_token.Type);
            if (infix_fn == null) {
                return leftExpression;
            }
            try self.nextToken(allocator);
            leftExpression = try infix_fn.?(self, leftExpression.?, allocator);
        }

        return leftExpression;
    }

    fn parseInfixExpression(self: *Parser, left: ast.Expression, allocator: std.mem.Allocator) !?ast.Expression {
        const infix_token: TokenType = self.*.current_token.Type;
        const operator: []const u8 = try allocator.dupe(u8, self.*.current_token.Literal);

        const precedence = self.currentPrecedence();
        try self.nextToken(allocator);
        const right: ?ast.Expression = try self.parseExpression(precedence, allocator);
        if (right) |expr| {
            const infix_expression: *ast.InfixExpression = try allocator.create(ast.InfixExpression);
            const expression: ast.InfixExpression = ast.InfixExpression{ .token = infix_token, .operator = operator, .left_expression = left, .right_expression = expr };
            infix_expression.* = expression;
            return infix_expression.createExpression();
        }

        return null;
    }

    fn parsePrefixExpression(self: *Parser, allocator: std.mem.Allocator) !?ast.Expression {
        const prefix_token: TokenType = self.*.current_token.Type;
        const prefix_operator: []const u8 = try allocator.dupe(u8, self.*.current_token.Literal);

        try self.nextToken(allocator);

        const expression: ?ast.Expression = try self.parseExpression(Precedence.PREFIX, allocator);
        if (expression) |expr| {
            const prefix_expression: *ast.PrefixExpression = try allocator.create(ast.PrefixExpression);
            prefix_expression.* = ast.PrefixExpression{ .token = prefix_token, .operator = prefix_operator, .right_expression = expr };
            return prefix_expression.createExpression();
        }
        return null;
    }

    fn parseIntegerLiteral(self: *Parser, allocator: std.mem.Allocator) !?ast.Expression {
        const intValue: i64 = std.fmt.parseInt(i64, self.*.current_token.Literal, 10) catch {
            std.debug.print("Cannot convert {s} to integer", .{self.*.current_token.Literal});
            return null;
        };
        const integerLiteral: *ast.IntegerLiteral = try allocator.create(ast.IntegerLiteral);
        integerLiteral.* = ast.IntegerLiteral{ .token = self.current_token.Type, .value = intValue };

        return integerLiteral.createExpression();
    }

    fn parseBooleanExpression(self: *Parser, allocator: std.mem.Allocator) !?ast.Expression {
        const booleanValue: bool = self.currentTokenIs(TokenType.TRUE);

        const booleanLiteral: *ast.BooleanLiteral = try allocator.create(ast.BooleanLiteral);
        booleanLiteral.* = ast.BooleanLiteral{ .token = self.current_token.Type, .value = booleanValue };

        return booleanLiteral.createExpression();
    }

    fn parseIdentifer(self: *Parser, allocator: std.mem.Allocator) !?ast.Expression {
        const identifier: *ast.Identifier = try allocator.create(ast.Identifier);
        const literal: []const u8 = try allocator.dupe(u8, self.*.current_token.Literal);
        identifier.* = ast.Identifier{ .token = self.current_token.Type, .value = literal };

        return identifier.createExpression();
    }

    fn parseGroupedExpression(self: *Parser, allocator: std.mem.Allocator) !?ast.Expression {
        try self.nextToken(allocator);

        const expr = try self.parseExpression(Precedence.LOWEST, allocator) orelse return null;

        if (!self.expectPeek(TokenType.RPAREN, allocator)) {
            return null;
        }

        return expr;
    }

    fn parseIfExpression(self: *Parser, allocator: std.mem.Allocator) !?ast.Expression {
        const if_expression: *ast.IfExpression = try allocator.create(ast.IfExpression);
        const current_token: TokenType = self.current_token.Type;

        if (!self.expectPeek(TokenType.LPAREN, allocator)) {
            return null;
        }

        try self.nextToken(allocator);
        const condition: ast.Expression = try self.parseExpression(Precedence.LOWEST, allocator) orelse return null;

        if (!self.expectPeek(TokenType.RPAREN, allocator)) {
            return null;
        }

        if (!self.expectPeek(TokenType.LBRACE, allocator)) {
            return null;
        }

        const consequence: *ast.BlockStatement = try self.parseBlockStatement(allocator);
        var alternative: ?*ast.BlockStatement = null;

        if (self.peekTokenIs(TokenType.ELSE)) {
            try self.nextToken(allocator);
            if (!self.expectPeek(TokenType.LBRACE, allocator)) {
                return null;
            }
            alternative = try self.parseBlockStatement(allocator);
        }

        if_expression.* = ast.IfExpression{ .token = current_token, .condition = condition, .consequence = consequence, .alternative = alternative };

        return if_expression.createExpression();
    }

    fn parseBlockStatement(self: *Parser, allocator: std.mem.Allocator) !*ast.BlockStatement {
        var statement_array_list = std.ArrayList(ast.Statement).init(allocator);
        const current_token: TokenType = self.current_token.Type;

        try self.nextToken(allocator);

        while (!self.currentTokenIs(TokenType.RBRACE) and !self.currentTokenIs(TokenType.EOF)) {
            const statement: ?ast.Statement = try self.parseStatement(allocator);
            if (statement != null) {
                try statement_array_list.append(statement.?);
            }
            try self.nextToken(allocator);
        }
        const statements = try statement_array_list.toOwnedSlice();

        const block_statement: *ast.BlockStatement = try allocator.create(ast.BlockStatement);
        block_statement.* = ast.BlockStatement{ .token = current_token, .statements = statements };
        return block_statement;
    }

    fn parseReturnStatement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        const return_token: TokenType = self.current_token.Type;

        try self.nextToken(allocator);

        while (!self.*.currentTokenIs(TokenType.SEMICOLON)) {
            try self.nextToken(allocator);
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

        const variable_name: []const u8 = try allocator.dupe(u8, self.*.current_token.Literal);
        const identifier: ast.Identifier = ast.Identifier{ .token = TokenType.IDENT, .value = variable_name };

        if (!self.*.expectPeek(TokenType.ASSIGN, allocator)) {
            return null;
        }

        while (!self.*.currentTokenIs(TokenType.SEMICOLON)) {
            try self.nextToken(allocator);
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

    fn peekPrecedence(self: *Parser) Precedence {
        return self.*.precedences.get(self.*.peek_token.Type) orelse Precedence.LOWEST;
    }

    fn currentPrecedence(self: *Parser) Precedence {
        return self.*.precedences.get(self.*.current_token.Type) orelse Precedence.LOWEST;
    }

    fn expectPeek(self: *Parser, token_type: TokenType, allocator: std.mem.Allocator) bool {
        if (self.*.peekTokenIs(token_type)) {
            self.*.nextToken(allocator) catch {
                return false;
            };
            return true;
        } else {
            self.peekErrors(token_type, allocator) catch {
                return false;
            };
            return false;
        }
    }

    fn noPrefixParserFnError(self: *Parser, t: TokenType, allocator: std.mem.Allocator) !void {
        const msg: []const u8 = try std.fmt.allocPrint(allocator, "no prefix parse function for {any} found", .{t});
        try self.errors.append(msg);
    }
};


#define KEYWORD(name)                                                                              \
    name##_keyword = str_intern(#name);                                                            \
    buf_push(keywords, name##_keyword)

const char *typedef_keyword;
const char *enum_keyword;
const char *struct_keyword;
const char *union_keyword;
const char *var_keyword;
const char *const_keyword;
const char *func_keyword;
const char *sizeof_keyword;
const char *break_keyword;
const char *continue_keyword;
const char *return_keyword;
const char *if_keyword;
const char *else_keyword;
const char *while_keyword;
const char *do_keyword;
const char *for_keyword;
const char *switch_keyword;
const char *case_keyword;
const char *default_keyword;

const char *first_keyword;
const char *last_keyword;
const char **keywords;

/* first_keyword and last_keyword is useful because we can simplify the process of checking whether
  a str is keyword or not.
*/

void init_keywords() {
    static bool inited;
    if (inited) {
        return;
    }

    char *arena_end = str_arena.end;
    KEYWORD(typedef);
    KEYWORD(enum);
    KEYWORD(struct);
    KEYWORD(union);
    KEYWORD(const);
    KEYWORD(var);
    KEYWORD(func);
    KEYWORD(sizeof);
    KEYWORD(break);
    KEYWORD(continue);
    KEYWORD(return);
    KEYWORD(if);
    KEYWORD(else);
    KEYWORD(while);
    KEYWORD(do);
    KEYWORD(for);
    KEYWORD(switch);
    KEYWORD(case);
    KEYWORD(default);

    assert(str_arena.end == arena_end);
    first_keyword = typedef_keyword;
    last_keyword = default_keyword;
    inited = true;
}

#undef KEYWORD

typedef enum TokenKind {
    TOKEN_EOF,
    TOKEN_COLON,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_QUESTION,
    TOKEN_SEMICOLON,
    TOKEN_KEYWORD,
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_STR,
    TOKEN_NAME,
    /* Multiplicative precedence */
    TOKEN_MUL,
    TOKEN_FIRST_MUL = TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_MOD,
    TOKEN_AND,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_LAST_MUL = TOKEN_RSHIFT,
    /* Additive precedence */
    TOKEN_ADD,
    TOKEN_FIRST_ADD = TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_XOR,
    TOKEN_OR,
    TOKEN_LAST_ADD = TOKEN_OR,
    /* Comparative precedence */
    TOKEN_EQ,
    TOKEN_FIRST_CMP = TOKEN_EQ,
    TOKEN_NOTEQ,
    TOKEN_LT,
    TOKEN_GT,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_LAST_CMP = TOKEN_GTEQ,
    TOKEN_AND_AND,
    TOKEN_OR_OR,
    /* Assignment operators */
    TOKEN_ASSIGN,
    TOKEN_FIRST_ASSIGN = TOKEN_ASSIGN,
    TOKEN_ADD_ASSIGN,
    TOKEN_SUB_ASSIGN,
    TOKEN_OR_ASSIGN,
    TOKEN_AND_ASSIGN,
    TOKEN_XOR_ASSIGN,
    TOKEN_LSHIFT_ASSIGN,
    TOKEN_RSHIFT_ASSIGN,
    TOKEN_MUL_ASSIGN,
    TOKEN_DIV_ASSIGN,
    TOKEN_MOD_ASSIGN,
    TOKEN_LAST_ASSIGN = TOKEN_MOD_ASSIGN,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_COLON_ASSIGN,
} TokenKind;

/*Token modifier is useful when we want to know what the input source way. Since we are converting
 * all the numerical value to either double or int64*/
typedef enum {
    TOKENMOD_NONE,
    TOKENMOD_HEX,
    TOKENMOD_OCT,
    TOKENMOD_BIN,
    TOKENMOD_CHAR,
} TokenModifier;

const char *token_kind_names[] = {
    [TOKEN_EOF] = "EOF",
    [TOKEN_COLON] = ":",
    [TOKEN_LPAREN] = "(",
    [TOKEN_RPAREN] = ")",
    [TOKEN_LBRACE] = "{",
    [TOKEN_RBRACE] = "}",
    [TOKEN_LBRACKET] = "[",
    [TOKEN_RBRACKET] = "]",
    [TOKEN_COMMA] = ",",
    [TOKEN_DOT] = ".",
    [TOKEN_QUESTION] = "?",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_KEYWORD] = "keyword",
    [TOKEN_INT] = "int",
    [TOKEN_FLOAT] = "float",
    [TOKEN_STR] = "string",
    [TOKEN_NAME] = "name",
    [TOKEN_MUL] = "*",
    [TOKEN_DIV] = "/",
    [TOKEN_MOD] = "%",
    [TOKEN_AND] = "&",
    [TOKEN_LSHIFT] = "<<",
    [TOKEN_RSHIFT] = ">>",
    [TOKEN_ADD] = "+",
    [TOKEN_SUB] = "-",
    [TOKEN_OR] = "|",
    [TOKEN_XOR] = "^",
    [TOKEN_EQ] = "==",
    [TOKEN_NOTEQ] = "!=",
    [TOKEN_LT] = "<",
    [TOKEN_GT] = ">",
    [TOKEN_LTEQ] = "<=",
    [TOKEN_GTEQ] = ">=",
    [TOKEN_AND_AND] = "&&",
    [TOKEN_OR_OR] = "||",
    [TOKEN_ASSIGN] = "=",
    [TOKEN_ADD_ASSIGN] = "+=",
    [TOKEN_SUB_ASSIGN] = "-=",
    [TOKEN_OR_ASSIGN] = "|=",
    [TOKEN_AND_ASSIGN] = "&=",
    [TOKEN_XOR_ASSIGN] = "^=",
    [TOKEN_MUL_ASSIGN] = "*=",
    [TOKEN_DIV_ASSIGN] = "/=",
    [TOKEN_MOD_ASSIGN] = "%=",
    [TOKEN_LSHIFT_ASSIGN] = "<<=",
    [TOKEN_RSHIFT_ASSIGN] = ">>=",
    [TOKEN_INC] = "++",
    [TOKEN_DEC] = "--",
    [TOKEN_COLON_ASSIGN] = ":=",
};

const char *token_kind_name(TokenKind kind) {
    if (kind <= (sizeof(token_kind_names) / sizeof(*token_kind_names))) {
        return token_kind_names[kind];
    } else {
        return "<unknown>";
    }
}

typedef struct {
    TokenKind kind;
    TokenModifier mod;
    char *start;
    char *end;
    union {
        uint64_t int_val; /* Used for storing integers */
        double float_val; /* Used for storing float */
        const char *str_val;
        const char *name; /* Used to store the variable names */
    };
} Token;

Token token;
char *stream;

char const *token_info() {
    if (token.kind == TOKEN_NAME || token.kind == TOKEN_KEYWORD) {
        return token.name;
    } else {
        return token_kind_name(token.kind);
    }
}

uint8_t char_to_digit[256] = {
    ['0'] = 0,  ['1'] = 1,  ['2'] = 2,  ['3'] = 3,  ['4'] = 4,  ['5'] = 5,  ['6'] = 6,  ['7'] = 7,
    ['8'] = 8,  ['9'] = 9,  ['a'] = 10, ['A'] = 10, ['b'] = 11, ['B'] = 11, ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13, ['e'] = 14, ['E'] = 14, ['f'] = 15, ['F'] = 15,
};

bool is_keyword_name(const char *str) { return (str >= first_keyword) && (str <= last_keyword); }

void scan_int() {
    int base = 10;
    uint64_t val = 0;
    uint64_t digit = 0;

    /*This is for supporting hexadecimals, octal and binary*/
    if (*stream == '0') {
        stream++;
        if (tolower(*stream) == 'x') {
            /* Hexadecimal */
            base = 16;
            stream++;
            token.mod = TOKENMOD_HEX;
        } else if (isdigit(*stream)) {
            /* Octal */
            base = 8;
            token.mod = TOKENMOD_OCT;
        } else if (tolower(*stream) == 'b') {
            base = 2;
            stream++;
            token.mod = TOKENMOD_BIN;
        }
    }

    for (;;) {
        digit = char_to_digit[*(unsigned char *)stream];
        if (digit == 0 && *stream != '0') {
            /*
            when we use an index which is not defined in the 1-f range it will return 0 so we have
            to differentiate it between a real 0. For eg: 'G' and '0' will both return 0.
            */
            break;
        }
        if (digit >= base) {
            syntax_error("Digit '%c' out of range for base %llu,", *stream, base);
            digit = 0;
            break;
        }
        if (val > (UINT64_MAX - digit) / base) {
            syntax_error("Int overflow occured");
            while (isdigit(*stream)) {
                stream++;
            }
            val = 0;
        }
        val = val * base + digit;
        stream++;
    }
    token.kind = TOKEN_INT;
    token.int_val = val;
}

void scan_float() {
    /* [0-9]*'.'[0-9]*([eE][-+]?[0-9]+)? */
    char *start = stream;
    while (isdigit(*stream)) {
        stream++;
    }

    if (*stream == '.') {
        stream++;
    }
    while (isdigit(*stream)) {
        stream++;
    }

    if (tolower(*stream) == 'e') {
        stream++;
        if (*stream == '+' || *stream == '-') {
            stream++;
        }
        if (!isdigit(*stream)) {
            syntax_error("Expected digit after float literal exponent, found %c", *stream);
        }
        while (isdigit(*stream)) {
            stream++;
        }
    }
    double val = strtod(start, NULL);
    if (val == HUGE_VAL || val == -HUGE_VAL) {
        syntax_error("Floating point value overflow");
    }

    token.kind = TOKEN_FLOAT;
    token.float_val = val;
}

char escape_to_char[256] = {
    ['n'] = '\n', ['r'] = '\r', ['t'] = '\t', ['v'] = '\v', ['b'] = '\b', ['a'] = '\a', ['0'] = '0',
};

void scan_char() {
    assert(*stream == '\'');
    stream++;

    if (*stream == '\'') {
        syntax_error("char value cannont be empty");
        stream++;
    } else if (*stream == '\n') {
        syntax_error("char value cannot have newline");
        stream++;
    } else if (*stream == '\\') {
        stream++;
        int val = escape_to_char[*(unsigned char *)stream];
        if (val == 0 && *stream != '0') {
            syntax_error("Invalid char literal escape '\\%c'", *stream);
        }
        token.int_val = val;
        stream++;
    } else {
        token.int_val = *stream;
        stream++;
    }

    if (*stream != '\'') {
        syntax_error("Expected ' to end char but found, %c", *stream);
    } else {
        stream++;
    }

    token.kind = TOKEN_INT;
    token.mod = TOKENMOD_CHAR;
}

void scan_string() {
    assert(*stream == '\"');
    stream++;
    char *str = NULL;

    while (*stream && *stream != '"') {
        char val = *stream;
        if (val == '\n') {
            syntax_error("string literals cannont contain new lines");
            break;
        } else if (val == '\\') {
            stream++;
            val = escape_to_char[*(unsigned char *)stream];
            if (val == 0 && *stream != '0') {
                syntax_error("Invalid string literal escape '\\%c'", val);
            }
        }
        buf_push(str, val);
        stream++;
    }

    if (*stream) {
        assert(*stream == '"');
        stream++;
    } else {
        syntax_error("Unexpected end of string literal");
    }

    buf_push(str, 0);
    token.kind = TOKEN_STR;
    token.str_val = str;
}

#define CASE1(c1, k1)                                                                              \
    case c1:                                                                                       \
        token.kind = k1;                                                                           \
        stream++;                                                                                  \
        break;

#define CASE2(c1, k1, c2, k2)                                                                      \
    case c1:                                                                                       \
        token.kind = k1;                                                                           \
        stream++;                                                                                  \
        if (*stream == c2) {                                                                       \
            token.kind = k2;                                                                       \
            stream++;                                                                              \
        }                                                                                          \
        break;

#define CASE3(c1, k1, c2, k2, c3, k3)                                                              \
    case c1:                                                                                       \
        token.kind = k1;                                                                           \
        stream++;                                                                                  \
        if (*stream == c2) {                                                                       \
            token.kind = k2;                                                                       \
            stream++;                                                                              \
        } else if (*stream == c3) {                                                                \
            token.kind = k3;                                                                       \
            stream++;                                                                              \
        }                                                                                          \
        break;

/* clang-format off */
void next_token() {
    token.start = stream;
    token.mod = TOKENMOD_NONE;
    switch (*stream) {
        case '\'': {
            scan_char();
            break;
        }
        case '\"': {
            scan_string();
            break;
        }
        case ' ': case '\n': case '\r': case '\t': case '\v': {
            while (isspace(*stream)) {
                stream++;
            }
            next_token();
            break;
			}
        case '.': {
			if(isdigit(stream[1])) {
				scan_float();
			} else {
				token.kind = TOKEN_DOT;
				stream++;
			}
            break;
        }
        case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9': {
            while (isdigit(*stream)) {
                stream++;
            }
            if (*stream == '.' || tolower(*stream) == 'e') {
                stream = token.start;
                scan_float();
            } else {
                stream = token.start;
                scan_int();
            }
            break;
        }
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i':
        case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
        case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z': case 'A':
        case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
        case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S':
        case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z': case '_': {
            while (isalnum(*stream) || *stream == '_') {
                stream++;
            }
            token.name = str_intern_range(token.start, stream);
            token.kind = is_keyword_name(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
            break;
        }
		case '>': {
			token.kind = TOKEN_GT;
			stream++;
			if(*stream == '>') {
				token.kind = TOKEN_RSHIFT;
				stream++;
				if(*stream == '='){
					token.kind = TOKEN_RSHIFT_ASSIGN;
					stream++;
				}
			} else if(*stream == '=') {
				token.kind = TOKEN_GTEQ;
				stream++;
			} 
			break;
		}
		case '<': {
			token.kind = TOKEN_LT;
			stream++;
			if(*stream == '<') {
				token.kind = TOKEN_LSHIFT;
				stream++;
				if(*stream == '='){
					token.kind = TOKEN_LSHIFT_ASSIGN;
					stream++;
				}
			} else if(*stream == '=') {
				token.kind = TOKEN_LTEQ;
				stream++;
			}
			break;
		}
		CASE1('\0', TOKEN_EOF)
		CASE1('(', TOKEN_LPAREN)
		CASE1(')', TOKEN_RPAREN)
		CASE1('{', TOKEN_LBRACE)
		CASE1('}', TOKEN_RBRACE)
		CASE1('[', TOKEN_LBRACKET)
		CASE1(']', TOKEN_RBRACKET)
		CASE1(',', TOKEN_COMMA)
		CASE1('?', TOKEN_QUESTION)
		CASE1(';', TOKEN_SEMICOLON)
		CASE2(':', TOKEN_COLON, '=', TOKEN_COLON_ASSIGN)
		CASE2('=', TOKEN_ASSIGN, '=', TOKEN_EQ)
		CASE2('^', TOKEN_XOR, '=', TOKEN_XOR_ASSIGN)
		CASE2('*', TOKEN_MUL, '=', TOKEN_MUL_ASSIGN)
		CASE2('/', TOKEN_DIV, '=', TOKEN_DIV_ASSIGN)
		CASE2('%', TOKEN_MOD, '=', TOKEN_MOD_ASSIGN)
		CASE3('+', TOKEN_ADD, '=', TOKEN_ADD_ASSIGN, '+', TOKEN_INC)
		CASE3('-', TOKEN_SUB, '=', TOKEN_SUB_ASSIGN, '-', TOKEN_DEC)
		CASE3('&', TOKEN_AND, '=', TOKEN_AND_ASSIGN, '&', TOKEN_AND_AND)
		CASE3('|', TOKEN_OR, '=', TOKEN_OR_ASSIGN, '|', TOKEN_OR_OR)
        default:
			syntax_error("Invalid '%c' token, skipping", *stream);
			stream++;
            next_token();
    }
    token.end = stream;
}
/* clang-format on */
#undef CASE1
#undef CASE2
#undef CASE3

void init_stream(char *str) {
    stream = str;
    next_token();
}

bool is_token(TokenKind kind) { return token.kind == kind; }

bool is_eof(TokenKind kind) { return token.kind == TOKEN_EOF; }

bool is_token_name(const char *name) { return token.kind == TOKEN_NAME && token.name == name; }

bool is_keyword(const char *name) { return token.kind == TOKEN_KEYWORD && token.name == name; }

bool match_keyword(const char *name) {
    if (is_keyword(name)) {
        next_token();
        return true;
    } else {
        return false;
    }
}

bool match_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        return false;
    }
}

bool expect_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        fatal("Expected token %s, got %s", token_kind_name(kind), token_kind_name(kind));
        return false;
    }
}

void print_token(Token token) {
    switch (token.kind) {
        case TOKEN_INT:
            printf("TOKEN INT: %llu", token.int_val);
            break;
        case TOKEN_FLOAT:
            printf("TOKEN FLOAT: %f", token.float_val);
            break;
        case TOKEN_NAME:
            printf("TOKEN NAME: %.*s", (int)(token.end - token.start), token.start);
            break;
        default:
            printf("TOKEN: %c", token.kind);
    }
    printf("\n");
}

void keyword_test() {
    printf("Testing keyword\n");
    init_keywords();
    assert(is_keyword_name(first_keyword));
    assert(is_keyword_name(last_keyword));
	const char **it;
    for (it = keywords; it != buf_end(keywords); it++) {
        assert(is_keyword_name(*it));
    }
    assert(!is_keyword_name(str_intern("foo")));
}

#define assert_token(x) assert(match_token(x))
#define assert_token_keyword(x) assert(token.name == str_intern(x) && match_token(TOKEN_KEYWORD))
#define assert_token_name(x) assert(token.name == str_intern(x) && match_token(TOKEN_NAME))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_string(x) assert((strcmp(token.str_val, (x)) == 0) && match_token(TOKEN_STR))
#define assert_token_eof() assert(is_token(0))

void test_lexer() {
	keyword_test();

    printf("Testing lexer\n");

    init_stream("if while");
    assert_token_keyword("if");
    assert_token_keyword("while");
    assert_token_eof();

    init_stream(": := + += ++ < <= << <<=");
    assert_token(TOKEN_COLON);
    assert_token(TOKEN_COLON_ASSIGN);
    assert_token(TOKEN_ADD);
    assert_token(TOKEN_ADD_ASSIGN);
    assert_token(TOKEN_INC);
    assert_token(TOKEN_LT);
    assert_token(TOKEN_LTEQ);
    assert_token(TOKEN_LSHIFT);
    assert_token(TOKEN_LSHIFT_ASSIGN);
    assert_token_eof();

    init_stream("\"hello\" \"a\\nb\"");
    assert_token_string("hello");
    assert_token_string("a\nb");

    init_stream("'a' 'c'");
    assert_token_int('a');
    assert_token_int('c');
    assert_token_eof();

    init_stream("2.33e-2 .33 33e1 33.1");
    assert_token_float(2.33e-2);
    assert_token_float(.33);
    assert_token_float(33e1);
    assert_token_float(33.1);
    assert_token_eof();

    init_stream("0");
    assert_token_int(0);
    assert_token_eof();

    init_stream("XY+(XY)_HELLO1,0x23a+994");
    assert_token_name("XY");
    assert_token(TOKEN_ADD);
    assert_token(TOKEN_LPAREN);
    assert_token_name("XY");
    assert_token(TOKEN_RPAREN);
    assert_token_name("_HELLO1");
    assert_token(TOKEN_COMMA);
    assert_token_int(0x23a);
    assert_token(TOKEN_ADD);
    assert_token_int(994);
    assert_token_eof();
    printf("Testing lexer - passed\n");
}

#undef assert_token_eof
#undef assert_token_int
#undef assert_token_name
#undef assert_token

void test_str_intern() {
    char x1[] = "hello";
    char y1[] = "hello";
    assert(x1 != y1);
    assert(str_intern(x1) == str_intern(y1));

    char y2[] = "hello9";
    assert(str_intern(x1) != str_intern(y2));
}

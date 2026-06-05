Token token;
char *stream;

uint8_t char_to_digit[256] = {
    ['0'] = 0,  ['1'] = 1,  ['2'] = 2,  ['3'] = 3,  ['4'] = 4,  ['5'] = 5,  ['6'] = 6,  ['7'] = 7,
    ['8'] = 8,  ['9'] = 9,  ['a'] = 10, ['A'] = 10, ['b'] = 11, ['B'] = 11, ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13, ['e'] = 14, ['E'] = 14, ['f'] = 15, ['F'] = 15,
};

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
        digit = char_to_digit[(int)*stream];
        if (digit == 0 && *stream != '0') {
            /*
            when we use an index which is not defined in the 1-f range it will return 0 so we have
            to differentiate it between a real 0. For eg: 'G' and '0' will both return 0.
            */
            break;
        }
        if (digit > base) {
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
        int val = escape_to_char[(int)*stream];
        if (val == 0 && val != '0') {
            syntax_error("");
        }
        token.int_val = val;
    } else {
        token.int_val = *stream;
        stream++;
    }

    if (*stream != '\'') {
        syntax_error("Expected ' to end char but found, %c", *stream);
    }

    stream++;
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
        } else if (val == '\\') {
            stream++;
            val = escape_to_char[(int)*stream];
            if (val == 0 && val != '0') {
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

    stream++;
    token.kind = TOKEN_STR;
    token.str_val = str;
}

#define CASE1(c, c1, k1)                                                                           \
    case c:                                                                                        \
        token.kind = *stream++;                                                                    \
        if (*stream == c1) {                                                                       \
            token.kind = k1;                                                                       \
            stream++;                                                                              \
        }                                                                                          \
        break;

#define CASE2(c, c1, k1, c2, k2)                                                                   \
    case c:                                                                                        \
        token.kind = *stream++;                                                                    \
        if (*stream == c1) {                                                                       \
            token.kind = k1;                                                                       \
            stream++;                                                                              \
        } else if (*stream == c2) {                                                                \
            token.kind = k2;                                                                       \
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
            scan_float();
            break;
        }
        case '0': case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9': {
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
            token.kind = TOKEN_NAME;
            token.name = str_intern_range(token.start, stream);
            break;
        }
		case '>': {
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
		CASE1(':', '=', TOKEN_COLON_ASSIGN)
		CASE1('^', '=', TOKEN_XOR_ASSIGN)
		CASE1('*', '=', TOKEN_MUL_ASSIGN)
		CASE1('/', '=', TOKEN_DIV_ASSIGN)
		CASE1('%', '=', TOKEN_MOD_ASSIGN)
		CASE2('+', '=', TOKEN_ADD_ASSIGN, '+', TOKEN_INC)
		CASE2('-', '=', TOKEN_SUB_ASSIGN, '-', TOKEN_DEC)
		CASE2('|', '=', TOKEN_OR_ASSIGN, '|', TOKEN_OR)
		CASE2('&', '=', TOKEN_AND_ASSIGN, '&', TOKEN_AND)
        default:
            token.kind = *stream++;
            break;
    }
    token.end = stream;
}
/* clang-format on */
#undef CASE1
#undef CASE2

bool is_token(TokenKind kind) { return token.kind == kind; }

bool is_token_name(const char *name) { return token.kind == TOKEN_NAME && token.name == name; }

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

void init_stream(char *str) {
    stream = str;
    next_token();
}

#define assert_token(x) assert(match_token(x))
#define assert_token_name(x) assert(token.name == str_intern(x) && match_token(TOKEN_NAME))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_string(x) assert((strcmp(token.str_val, (x)) == 0) && match_token(TOKEN_STR))
#define assert_token_eof() assert(is_token(0))

void test_lexer() {
    printf("Testing lexer\n");

    init_stream("++ -- >> >>=");
    assert_token(TOKEN_INC);
    assert_token(TOKEN_DEC);
    assert_token(TOKEN_RSHIFT);
    assert_token(TOKEN_RSHIFT_ASSIGN);
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
    assert_token('+');
    assert_token('(');
    assert_token_name("XY");
    assert_token(')');
    assert_token_name("_HELLO1");
    assert_token(',');
    assert_token_int(0x23a);
    assert_token('+');
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

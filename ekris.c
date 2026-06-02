#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ekris.h"

int parse_expr();
bool match_token(TokenKind);
void next_token();

#define MAX(a, b) (((a) > (b)) ? (a) : (b))

/*
 Sketch buffer
 The way the data will be organized is len,cap, buf[0], buf[1], buf[2]..
 continously The user will have the pointer to the first element of buf ie;
 buf[0] so access can be done like buf[0] and buf[1] not like BufHdr->buf[0].
*/
typedef struct {
    size_t len;
    size_t cap;
    char buf[0];
} BufHdr;

/*
offsetof() will return how deep the 'buf' value exist from the start of BufHdr.
Here it will be 16.
*/
#define buf__hdr(b) ((BufHdr *)((char *)b - offsetof(BufHdr, buf)))
#define buf__fits(b, n) (buf_len(b) + (n) <= buf_cap(b))
#define buf__fit(b, n)                                                                             \
    (buf__fits(b, n) ? 0 : ((b) = buf__grow((b), buf_len(b) + (n), sizeof(*(b)))))

#define buf_len(b) ((b) ? buf__hdr(b)->len : 0)
#define buf_cap(b) ((b) ? buf__hdr(b)->cap : 0)
#define buf_push(b, x) (buf__fit(b, 1), b[buf_len(b)] = (x), buf__hdr(b)->len++)
#define buf_free(b) ((b) ? (free(buf__hdr(b)), (b) = NULL) : 0)

void *buf__grow(const void *buf, size_t new_len, size_t elem_size) {
    size_t new_cap = MAX(1 + 2 * buf_cap(buf), new_len);
    assert(new_cap >= new_len);

    size_t new_size = offsetof(BufHdr, buf) + (new_cap * elem_size);
    BufHdr *new_hdr;
    if (buf) {
        new_hdr = realloc(buf__hdr(buf), new_size);
    } else {
        new_hdr = malloc(new_size);
        new_hdr->len = 0;
    }
    new_hdr->cap = new_cap;
    return new_hdr->buf;
}

void test_stretchy_buffer() {
    printf("Testing the stretchy buffer\n");
    int *buf = NULL;

    int i;
    enum { N = 1024 };
    for (i = 0; i < N; i++) {
        buf_push(buf, i);
    }
    assert(buf_len(buf) == N);

    for (i = 0; i < N; i++) {
        assert(buf[i] == i);
    }

    printf("Test passed\n\n");
    buf_free(buf);
}

typedef struct InternStr {
    size_t len;
    char *str;
} InternStr;

void fatal(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
    exit(1);
}

void syntax_error(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("Syntax Error: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
}

const char *token_kind_name(TokenKind kind) {
    static char buf[256];
    switch (kind) {
        case TOKEN_INT:
            sprintf(buf, "integer");
            break;
        case TOKEN_FLOAT:
            sprintf(buf, "float");
            break;
        case TOKEN_NAME:
            sprintf(buf, "name");
            break;
        default:
            if (kind < 128 && isprint(kind)) {
                sprintf(buf, "%c", kind);
            } else {
                sprintf(buf, "<ASCII %d>", kind);
            }
    }

    return buf;
}

Token token;
char *stream;

static InternStr *interns;

const char *str_intern_range(const char *start, const char *end) {
    size_t len = end - start;
    int i;
    for (i = 0; i < buf_len(interns); i++) {
        if (len == interns[i].len && strncmp(interns[i].str, start, len) == 0) {
            return interns[i].str;
        }
    }
    char *str = malloc(len + 1);
    memcpy(str, start, len);
    str[len] = '\0';
    InternStr intern_str = {};
    intern_str.len = len;
    intern_str.str = str;

    buf_push(interns, intern_str);
    return str;
}

const char *str_intern(const char *str) { return str_intern_range(str, str + strlen(str)); }

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
        default:
            token.kind = *stream++;
            break;
    }
    token.end = stream;
}
/* clang-format on */

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

int parse_3() {
    if (is_token(TOKEN_INT)) {
        int val = token.int_val;
        next_token();
        return val;
    } else if (is_token(TOKEN_FLOAT)) {
        double val = token.float_val;
        next_token();
        return val;
    } else if (match_token('+')) {
        int val = parse_expr();
        return val;
    } else if (match_token('(')) {
        int val = parse_expr();
        expect_token(')');
        return val;
    } else {
        fatal("Expected ( or an int but got, %c", token_kind_name(token.kind));
        return 0;
    }
}

int parse_2() {
    if (match_token('-')) {
        return -parse_3();
    } else if (match_token('+')) {
        return parse_2();
    } else {
        return parse_3();
    }
}

int parse_1() {
    int val = parse_2();
    while (is_token('*') || is_token('/')) {
        char op = token.kind;
        next_token();
        int rval = parse_2();
        if (op == '*') {
            val = val * rval;
        } else {
            assert(op == '/');
            assert(op != 0);
            val = val / rval;
        }
    }
    return val;
}

int parse_0() {
    int val = parse_1();
    while (is_token('+') || is_token('-')) {
        char op = token.kind;
        next_token();
        int rval = parse_1();
        if (op == '+') {
            val = val + rval;
        } else {
            assert(op == '-');
            val = val - rval;
        }
    }
    return val;
}

int parse_expr() { return parse_0(); }

int parse_expr_str(char *str) {
    stream = str;
    next_token();
    return parse_expr();
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

#if 0
    expr3 = INT | '(' expr ')' 
    expr2 = '-' expr2 | expr3
    expr1 = expr2 ([*/] expr2)*
    expr0 = expr1 ([+-] expr1)*
    expr = expr0
#endif

void test_str_intern() {
    char x1[] = "hello";
    char y1[] = "hello";
    assert(x1 != y1);
    assert(str_intern(x1) == str_intern(y1));

    char y2[] = "hello9";
    assert(str_intern(x1) != str_intern(y2));
}

void run_test() {
    test_stretchy_buffer();
    test_lexer();
    test_str_intern();
}

int main(int argc, char **argv) {
    run_test();
    return 0;
}

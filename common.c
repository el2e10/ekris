typedef enum {
    TOKEN_INT = 128,
    TOKEN_FLOAT,
    TOKEN_STR,
    TOKEN_NAME, /* Identifier or variable name */
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_EQ,
    TOKEN_NOTEQ,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_COLON_ASSIGN,
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
} TokenKind;

typedef enum {
    TOKENMOD_NONE,
    TOKENMOD_HEX,
    TOKENMOD_OCT,
    TOKENMOD_BIN,
    TOKENMOD_CHAR,
} TokenModifier;

typedef struct {
    TokenKind kind;
    TokenModifier mod;
    char *start;
    char *end;
    union {
        uint64_t int_val; /* Used for storing integers */
        double float_val; /* Used for storing float */
        char *str_val;
        const char *name; /* Used to store the variable names */
    };
} Token;

typedef struct InternStr {
    size_t len;
    char *str;
} InternStr;

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

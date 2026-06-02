#include <stdint.h>

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

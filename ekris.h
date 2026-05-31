#include <stdint.h>

typedef enum {
    TOKEN_INT = 128,
    TOKEN_NAME, /* Identifier or variable name */
    TOKEN_WS,   /*Whitespace*/
} TokenKind;

typedef struct {
    TokenKind kind;
    char *start;
    char *end;
    union {
        uint64_t int_val; /* Used for storing numbers */
        const char *name; /* Used to store the variable names */
    };
} Token;

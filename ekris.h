#include <stdint.h>

typedef enum {
    TOKEN_INT = 128,
	TOKEN_FLOAT,
    TOKEN_NAME, /* Identifier or variable name */
} TokenKind;

typedef struct {
    TokenKind kind;
    char *start;
    char *end;
    union {
        uint64_t int_val; /* Used for storing integers */
		double float_val; /* Used for storing float */
        const char *name; /* Used to store the variable names */
    };
} Token;

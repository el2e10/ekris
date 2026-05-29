#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>

#define MAX(a, b) (((a) > (b)) ? (a) : (b))

/*
 Sketch buffer
 The way the data will be organized is len,cap, buf[0], buf[1], buf[2].. continously
 The user will have the pointer to the first element of buf ie; buf[0] so access can be done like
   buf[0] and buf[1] not like BufHdr->buf[0].
*/
typedef struct {
	size_t len;
	size_t cap;
	char buf[1];
} BufHdr;

/*
offsetof() will return how deep the 'buf' value exist from the start of BufHdr. Here it will be 16.
*/
#define buf__hdr(b) ((BufHdr *)((char *)b - offsetof(BufHdr, buf)))
#define buf__fits(b, n) (buf_len(b) + (n) <= buf_cap(b))
#define buf__fit(b, n) (buf__fits(b, n) ? 0 : ((b) = buf__grow((b), buf_len(b) + (n),\
															   sizeof(*(b)))))

#define buf_len(b) ((b) ? buf__hdr(b)->len : 0)
#define buf_cap(b) ((b) ? buf__hdr(b)->cap : 0)
#define buf_push(b, x) (buf__fit(b, 1), b[buf_len(b)] = (x), buf__hdr(b)->len++)
#define buf_free(b) ((b) ? (free(buf__hdr(b)), (b) = NULL): 0)

void *buf__grow(const void *buf, size_t new_len, size_t elem_size){
	size_t new_cap = MAX(1 + 2 * buf_cap(buf), new_len);
	assert(new_cap >= new_len);

	size_t new_size = offsetof(BufHdr, buf) + (new_cap * elem_size);
	BufHdr *new_hdr;
	if(buf){
		new_hdr = realloc(buf__hdr(buf), new_size);
	} else {
		new_hdr =  malloc(new_size);
		new_hdr->len = 0;
	}
	new_hdr->cap = new_cap;
	return new_hdr->buf;
}

void test_stretchy_buffer(){
	printf("Testing the stretchy buffer\n");
	int *buf = NULL;

	int i;
	enum { N = 1024 };
	for(i = 0; i < N; i++){
		buf_push(buf, i);
	}
	assert(buf_len(buf) == N);

	for(i = 0; i < N; i++){
		assert(buf[i] == i);
	}

	printf("Test passed\n\n");
	buf_free(buf);
}

typedef enum {
	TOKEN_INT = 128,
	TOKEN_NAME, /* Identifier or variable name */
	TOKEN_OPERATOR, /* operators like + - >> << ^ */
	TOKEN_WS, /*Whitespace*/
} TokenKind;

typedef struct InternStr {
	size_t len;
	char *str;
} InternStr;

typedef struct {
	TokenKind kind;
	char *start;
	char *end;
	union {
		uint64_t val; /* Used for storing numbers */
		const char *name; /* Used to store the variable names */
		char operator; /* Used to store the operators in an expression */
	};
} Token;

Token token;
char *stream;

static InternStr *interns;

const char *str_intern_range(const char *start, const char *end) {
	size_t len = end - start;
	int i;
	for(i = 0; i < buf_len(interns); i++) {
		if(len == interns[i].len && strncmp(interns[i].str, start, len) == 0){
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

const char *str_intern(const char *str) {
	return str_intern_range(str, str + strlen(str));
}

void next_token() {
	token.start = stream;
	switch(*stream) {
		case ' ':
			token.kind = TOKEN_WS;
			stream++;
			break;
		case '+': case '-': case '*': case '/':
			{
				token.kind = TOKEN_OPERATOR;
				token.operator = *stream++;
				break;
			}
		case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8':
		case '9':
			{
				uint64_t val = 0;
				while(isdigit(*stream)){
					val *= 10;
					val += *stream++ - '0';
				}
				token.kind = TOKEN_INT;
				token.val = val;
				break;
			}
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i':
		case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
		case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z': case 'A':
		case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
		case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S':
		case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z': case '_':
			{
				while(isalnum(*stream) || *stream == '_') {
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

char peek_token() {
	return *stream;
}

void print_token(Token token){
	switch(token.kind) {
		case TOKEN_WS:
			return;
		case TOKEN_INT:
			printf("TOKEN INT: %llu", token.val);
			break;
		case TOKEN_NAME:
			printf("TOKEN NAME: %.*s", (int)(token.end - token.start), token.start);
			break;
		case TOKEN_OPERATOR:
			printf("TOKEN_OPERATOR: %c", token.operator);
			break;
		default:
			printf("TOKEN: %c", token.kind);
	}
	printf("\n");
}

void test_lexer() {
	printf("Testing the lexer\n");
	/*stream = "12*34 + 45/56 + ~25";*/
	stream = "2apple(2)apple";
	next_token();
	while(token.kind) {
		print_token(token);
		next_token();
	}
}

void test_str_intern() {
	char x1[] = "hello";
	char y1[] = "hello";
	assert(x1 != y1);
	assert(str_intern(x1) == str_intern(y1));

	char y2[] = "hello9";
	assert(str_intern(x1) != str_intern(y2));
}

int main(int argc, char **argv) {
	test_stretchy_buffer();
	test_lexer();
	test_str_intern();

	return 0;
}


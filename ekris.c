#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

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
#define buf_free(b) ((b) ? free(buf__hdr(b)): 0)

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

int main(int argc, char **argv) {
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

	buf_free(buf);
	return 0;
}


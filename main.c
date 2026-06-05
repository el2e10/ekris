#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "common.c"
#include "lexer.c"

void run_test() {
    test_stretchy_buffer();
    test_lexer();
    test_str_intern();
}

int main(int argc, char **argv) {
    run_test();
    return 0;
}

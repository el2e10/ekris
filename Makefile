default:
	clang --std=c89 -Wall -Werror ekris.c -o ekris
	./ekris

compile:
	clang --std=c89 -Wall -Werror -g -O0 ekris.c -o ekris

debug:
	clang --std=c89 -Wall -Werror -g -O0 ekris.c -o ekris
	lldb ./ekris


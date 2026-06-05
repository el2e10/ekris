default:
	clang --std=c89 -Wall -Werror main.c -o ekris
	./ekris

compile:
	clang --std=c89 -Wall -Werror -g -O0 main.c -o ekris

debug:
	clang --std=c89 -Wall -Werror -g -O0 main.c -o ekris
	lldb ./ekris


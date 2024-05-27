CC = gcc
C_FLAGS = -Wall -Wextra

build:
	yacc -d -v src/duna.y
	lex src/duna.l
	${CC} -o duna y.tab.c lex.yy.c -ll
run: build
	./duna sintatico.duna

clean:
	rm -f duna y.tab.c lex.yy.c

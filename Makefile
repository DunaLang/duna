CC = gcc
C_FLAGS = -Wall -Wextra

build: yacc
	$(CC) -o duna y.tab.c lex.yy.c -ll
run: build
	./duna sintatico.duna

yacc: lex
	yacc -d src/duna.y
lex:
	lex src/duna.l

clean:
	rm -f duna y.tab.c lex.yy.c

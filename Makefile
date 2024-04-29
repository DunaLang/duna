CC = clang
C_FLAGS = -Iinclude -Wall -Wextra

dune: lex
	$(CC) $(C_FLAGS) lex.yy.c 
lex:
	lex src/dune.lex

clean:
	rm -f a.out lex.yy.c

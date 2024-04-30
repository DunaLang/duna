CC = gcc
C_FLAGS = -Iinclude -Wall -Wextra

duna: lex
	$(CC) $(C_FLAGS) lex.yy.c -o duna
lex:
	lex src/duna.lex
run: duna
	./duna teste.duna 

clean:
	rm -f duna lex.yy.c

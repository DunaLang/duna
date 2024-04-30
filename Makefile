CC = gcc
C_FLAGS = -Wall -Wextra

duna: lex
	$(CC) $(C_FLAGS) lex.yy.c -o duna
lex:
	lex src/duna.l
run: duna
	./duna teste.duna 

clean:	
	rm -f duna lex.yy.c

CC = gcc
C_FLAGS = -Wall -Wextra
OUTPUT_DIR = ./out/compiler

build:
	mkdir -p out/compiler
	yacc -d src/duna.y -o ${OUTPUT_DIR}/y.tab.c
	lex -o ${OUTPUT_DIR}/lex.yy.c src/duna.l
	${CC} -o ${OUTPUT_DIR}/duna ${OUTPUT_DIR}/y.tab.c ${OUTPUT_DIR}/lex.yy.c lib/record.c lib/utils.c lib/symbol_table.c -ll
run: build
	${OUTPUT_DIR}/duna ./problems/happy-path/problem1.duna
	${CC} -o ./out/problem1 ./out/duna.c
	./out/problem1

clean:
	rm -rf out
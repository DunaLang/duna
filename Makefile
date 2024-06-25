CC = gcc
C_FLAGS = -Wall -Wextra
OUTPUT_DIR = ./out/compiler
N = 6

OBJS = $(shell find lib -name '*.c')

build:
	mkdir -p out/compiler
	yacc -d src/duna.y -o ${OUTPUT_DIR}/y.tab.c
	lex -o ${OUTPUT_DIR}/lex.yy.c src/duna.l
	${CC} -o ${OUTPUT_DIR}/duna ${OUTPUT_DIR}/y.tab.c ${OUTPUT_DIR}/lex.yy.c $(OBJS) -ll

runN: build
	${OUTPUT_DIR}/duna ./problems/happy-path/problem${N}.duna
	${CC} -g ${C_FLAGS} -o ./out/problem${N} ./out/duna.c
	./out/problem${N}

run: build
	bash executeHappyPath.sh
	
clean:
	rm -rf out
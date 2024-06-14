CC = gcc
C_FLAGS = -Wall -Wextra
OUTPUT_DIR = ./out/compiler

build:
	mkdir -p out/compiler
	yacc -d src/duna.y -o ${OUTPUT_DIR}/y.tab.c
	lex -o ${OUTPUT_DIR}/lex.yy.c src/duna.l
	${CC} -o ${OUTPUT_DIR}/duna ${OUTPUT_DIR}/y.tab.c ${OUTPUT_DIR}/lex.yy.c lib/record.c lib/utils.c lib/symbol_table.c lib/scope_stack.c lib/symbol_utils.c -ll
run: build
	${OUTPUT_DIR}/duna ./problems/happy-path/problem1.duna
	${CC} ${C_FLAGS} -o ./out/problem1 ./out/duna.c
	./out/problem1
test: build
	echo "----TESTING----"
	${OUTPUT_DIR}/duna ./problems/happy-path/problem1.duna
	echo "----HAPPY PATH----"
	${CC} ${C_FLAGS} -o ./out/problem1 ./out/duna.c
	./out/problem1

	${OUTPUT_DIR}/duna ./problems/should-fail/problem1-fail1.duna
	echo "----SHOULD FAIL - SEMICOLON MISSING----"
	${CC} ${C_FLAGS} -o ./out/fail ./out/duna.c

	${OUTPUT_DIR}/duna ./problems/should-fail/problem1-fail2.duna
	${CC} ${C_FLAGS} -o ./out/fail ./out/duna.c
	./out/fail

	${OUTPUT_DIR}/duna ./problems/should-fail/problem1-fail3.duna
	${CC} ${C_FLAGS} -o ./out/fail ./out/duna.c
	./out/fail


clean:
	rm -rf out
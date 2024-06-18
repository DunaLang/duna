CC = gcc
C_FLAGS = -Wall -Wextra
OUTPUT_DIR = ./out/compiler

build:
	mkdir -p out/compiler
	yacc -d src/duna.y -o ${OUTPUT_DIR}/y.tab.c
	lex -o ${OUTPUT_DIR}/lex.yy.c src/duna.l
	${CC} -o ${OUTPUT_DIR}/duna ${OUTPUT_DIR}/y.tab.c ${OUTPUT_DIR}/lex.yy.c lib/record.c lib/utils.c lib/symbol_table.c lib/scope_stack.c lib/symbol_utils.c lib/checks.c -ll

run: build
	${OUTPUT_DIR}/duna ./problems/happy-path/problem1.duna
	${CC} ${C_FLAGS} -o ./out/problem1 ./out/duna.c

runN: build
	${OUTPUT_DIR}/duna ./problems/happy-path/problem$(n).duna
	${CC} ${C_FLAGS} -o ./out/problem$(n) ./out/duna.c
	echo "------------------- RUNNING -------------------"
	echo
	./out/problem$(n)

test1: build
	echo "----TESTING----"
	${OUTPUT_DIR}/duna ./problems/happy-path/problem1.duna
	echo "----HAPPY PATH----"
	${CC} ${C_FLAGS} -o ./out/problem1 ./out/duna.c
	./out/problem1

	echo

	echo "------------------- SHOULD FAIL -------------------"
	echo
	echo "----SHOULD FAIL - SEMICOLON MISSING----"
	echo
	${OUTPUT_DIR}/duna ./problems/should-fail/problem1/problem1-fail1.duna
	${CC} ${C_FLAGS} -o ./out/fail ./out/duna.c > /dev/null > /dev/null

	echo
	echo "----SHOULD FAIL - INEXISTENT TYPE----"
	echo

	${OUTPUT_DIR}/duna ./problems/should-fail/problem1/problem1-fail2.duna
	${CC} ${C_FLAGS} -o ./out/fail ./out/duna.c

	echo
	echo "----SHOULD FAIL - VARIABLE X NOT DEFINED----"
	echo

	${OUTPUT_DIR}/duna ./problems/should-fail/problem1/problem1-fail3.duna
	${CC} ${C_FLAGS} -o ./out/fail ./out/duna.c

	echo
	echo "----SHOULD FAIL - IDENTIFIER ALREADY DEFINED ----"
	echo

	${OUTPUT_DIR}/duna ./problems/should-fail/problem1/problem1-fail8.duna
	${CC} ${C_FLAGS} -o ./out/fail ./out/duna.c

test3: build
	echo "----TESTING----"
	${OUTPUT_DIR}/duna ./problems/happy-path/problem3.duna
	echo "----HAPPY PATH----"
	${CC} ${C_FLAGS} -o ./out/problem3 ./out/duna.c
	./out/problem3

	echo

	echo "------------------- SHOULD FAIL -------------------"
	echo
	echo "----SHOULD FAIL - string_literal in array definition----"
	echo
	${OUTPUT_DIR}/duna ./problems/should-fail/problem3/problem3-fail1.duna
	${CC} ${C_FLAGS} -o ./out/fail ./out/duna.c

array: build
	${OUTPUT_DIR}/duna ./arrays.duna
	${CC} ${C_FLAGS} -o ./out/arrays ./out/duna.c
	./out/arrays

clean:
	rm -rf out
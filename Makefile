#Compile the compiler
all: result main

result: lex.yy.c simple.tab.c
	gcc simple.tab.c lex.yy.c -o result

main: main.c preprocessing.c
	gcc -o main main.c

lex.yy.c: scannerwithreturns.lex
	flex scannerwithreturns.lex

simple.tab.c: simple.y
	bison -d simple.y
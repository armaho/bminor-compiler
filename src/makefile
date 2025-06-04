bminor: main.o compiler.o parser.o scanner.o disassemble.o
	clang disassemble.o scanner.o parser.o compiler.o main.o -o bminor

main.o: main.c
	clang -c main.c -o main.o

compiler.o: compiler.c
	clang -c compiler.c -o compiler.o

parser.o: parser.c
	clang -c parser.c -o parser.o

scanner.o: scanner.c
	clang -c scanner.c -o scanner.o

disassemble.o: disassemble.c
	clang -c disassemble.c -o disassemble.o

clean:
	rm -rf bminor* *.o

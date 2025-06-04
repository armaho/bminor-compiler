bminor: main.o parser.o scanner.o
	clang scanner.o parser.o main.o -o bminor

main.o: main.c
	clang -c main.c -o main.o

parser.o: parser.c
	clang -c parser.c -o parser.o

scanner.o: scanner.c
	clang -c scanner.c -o scanner.o

clean:
	rm -f bminor *.o

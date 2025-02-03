.PHONY: all clean

CC = g++
CFLAGS = -std=c++17 -g -O0

all: clean
	flex scanner.lex
	bison -Wcounterexamples -d parser.y
	$(CC) $(CFLAGS) -o hw2 *.c *.cpp
clean:
	rm -f lex.yy.* parser.tab.* hw2

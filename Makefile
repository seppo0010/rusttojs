all: build/lex.yy.c build/parser-lalr.tab.c

.PHONY: all clean

clean:
	rm -rf build

build/lex.yy.c:
	mkdir -p build
	$(FLEX_PREFIX)flex -o build/lex.yy.c rust/src/grammar/lexer.l

build/parser-lalr.y:
	mkdir -p build
	cp rust/src/grammar/parser-lalr.y build/

build/parser-lalr.tab.c: build/parser-lalr.y
	$(BISON_PREFIX)bison build/parser-lalr.y --output=build/parser-lalr.tab.c --defines=build/parser-lalr.tab.h --name-prefix=rs -d

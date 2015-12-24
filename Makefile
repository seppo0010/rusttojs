all: build/rust-tokenizer build/rust-to-js

.PHONY: all clean

clean:
	cargo clean
	rm -rf build

build/lex.c:
	mkdir -p build
	$(FLEX_PREFIX)flex -o build/lex.yy.c rust/src/grammar/lexer.l
	echo '#include "parser-lalr.tab.h"' > build/lex.c
	cat build/lex.yy.c >> build/lex.c

build/parser-lalr.y:
	mkdir -p build
	cp rust/src/grammar/parser-lalr.y build/

build/parser-lalr.tab.c: build/parser-lalr.y
	$(BISON_PREFIX)bison build/parser-lalr.y --output=build/parser-lalr.tab.c --defines=build/parser-lalr.tab.h --name-prefix=rs -d

build/rust-tokenizer: build/lex.c build/parser-lalr.y build/parser-lalr.tab.c
	$(CC) -o build/rust-tokenizer grammar/rust-tokenizer.c build/lex.c build/parser-lalr.tab.c

build/rust-to-js:
	mkdir -p build
	cargo build
	cp ./target/debug/rusttojs build/rust-to-js

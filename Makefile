all: build/rust-tokenizer build/rust-to-js

.PHONY: all clean test

clean:
	cargo clean
	rm -rf build

test: all
	RUSTTOJS_PREFIX=./build/ ./runtests.sh
	@echo "SUCCESS"

build/lex.c:
	mkdir -p build
	wget "https://raw.githubusercontent.com/rust-lang/rust/4ce1dafd1d58852a88f38a0f63cb11236a7470cb/src/grammar/lexer.l" -O build/lexer.l
	$(FLEX_PREFIX)flex -o build/lex.yy.c build/lexer.l
	echo '#include "parser-lalr.tab.h"' > build/lex.c
	cat build/lex.yy.c >> build/lex.c

build/parser-lalr.y:
	mkdir -p build
	wget "https://raw.githubusercontent.com/rust-lang/rust/4ce1dafd1d58852a88f38a0f63cb11236a7470cb/src/grammar/parser-lalr.y" -O build/parser-lalr.y

build/parser-lalr.tab.c: build/parser-lalr.y
	$(BISON_PREFIX)bison build/parser-lalr.y --output=build/parser-lalr.tab.c --defines=build/parser-lalr.tab.h --name-prefix=rs -d

build/rust-tokenizer: build/lex.c build/parser-lalr.y build/parser-lalr.tab.c
	$(CC) -o build/rust-tokenizer grammar/rust-tokenizer.c build/lex.c build/parser-lalr.tab.c

build/rust-to-js:
	mkdir -p build
	cargo build
	cp ./target/debug/rusttojs build/rust-to-js

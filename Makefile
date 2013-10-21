LIBRARIES=-L ./lib
LINKER=

compile:
	rustc --lib $(LIBRARIES) $(LINKER) src/peg.rs --out-dir bin

test:
	rustc --test $(LIBRARIES) $(LINKER) src/peg.rs -o bin/pegtest && ./bin/pegtest

watch:
	fswatch ./src "make test && make compile"

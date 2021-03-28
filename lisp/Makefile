files := $(shell ls *.lisp)
names := $(files:.lisp=)

.PHONY: all clean $(names)

all: $(names)

$(names): %: bin/% man/man1/%.1

bin/%: %.lisp build_binary
	mkdir -p bin
	./build_binary $<
	mv $(@F) bin/

man/man1/%.1: %.lisp build_manual
	mkdir -p man/man1
	./build_manual $<
	mv $(@F) man/man1/

clean:
	rm -rf bin man

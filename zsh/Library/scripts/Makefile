SRC=src
BIN=bin
SOURCE_FILES = $(wildcard $(SRC)/*.hs)
EXE_FILES = $(SOURCE_FILES:$(SRC)/%.hs=$(BIN)/%)

build: $(EXE_FILES)

clean:
	rm -rf ./bin
	rm -rf ./src/*.{hi,o}

.PHONY: build clean

$(EXE_FILES): $(SOURCE_FILES)
	@mkdir -p $(@D)
	stack ghc --resolver lts-20.11 \
		--package turtle \
		--package process \
		-- -O2 -threaded -o $@ $<

format:
	find * -name '*.hs' | xargs -P0 fourmolu --no-cabal -i
	hlint .

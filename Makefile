
SRC=$(shell find src -name '*.hs')

CABAL=cabal

# FLAGS=--enable-tests --enable-library-profiling --enable-executable-profiling
# FLAGS=--enable-tests

RUN_FLAGS=

# CORPUS=corpora/gutenberg
# CORPUS=corpora/gutenberg/README
# CORPUS=corpora/gutenberg/carroll-alice.txt
# CORPUS=corpora/gutenberg/austen-persuasion.txt
CORPUS=corpora/gutenberg/melville-moby_dick.txt

all: test docs package

test:
	stack test

specs: build
	./dist/build/stat-nlp-specs/stat-nlp-specs

run:
	stack build
	stack exec -- stat-nlp $(CORPUS) $(RUN_FLAGS) > bigrams.txt

profile:
	stack build --library-profiling --executable-profiling
	stack exec -- stat-nlp $(CORPUS) $(RUN_FLAGS) > bigrams.txt

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

tags: ${SRC}
	codex update

hlint:
	hlint *.hs src specs

ghcid:
	ghcid "--command=stack ghci"

clean:
	stack clean
	-rm -rf *.hp *.prof *.ps *.aux
	codex cache clean

distclean: clean

deps:
	stack deps

stat-nlp.ps: stat-nlp.hp
	hp2ps -e8in -c $<

build:
	stack build

watch:
	ghcid

restart: distclean build

rebuild: clean build

.PHONY: all test run clean distclean deps build rebuild hlint watch tags ghcid profile

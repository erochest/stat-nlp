SRC=$(shell find src -name '*.hs')

CABAL=cabal

# FLAGS=--enable-tests --enable-library-profiling --enable-executable-profiling
FLAGS=--pedantic

RUN_FLAGS=

# CORPUS=corpora/gutenberg
# CORPUS=corpora/gutenberg/README
# CORPUS=corpora/gutenberg/carroll-alice.txt
# CORPUS=corpora/gutenberg/austen-persuasion.txt
# CORPUS=corpora/gutenberg/melville-moby_dick.txt
# CORPUS=corpora/TOBACCO
# CORPUS=TEST
CORPUS=corpora/lingspam_public/bare

all: test docs package

test:
	stack test stat-nlp:test:stat-nlp-specs $(FLAGS)

run: build
	stack exec -- stat-nlp -s corpora/stopwords/english -c $(CORPUS) $(RUN_FLAGS)

profile:
	stack build --library-profiling --executable-profiling
	stack exec -- stat-nlp $(CORPUS) $(RUN_FLAGS) > bigrams.txt

benchmark:
	stack bench $(FLAGS)

sgt.out: sgt
	./sgt < data/austen-cntcnt.txt > sgt.out

sgt: SGT.c
	gcc -o sgt SGT.c

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
	stack build $(FLAGS)

watch: build
	ghcid "--command=stack ghci --main-is stat-nlp:exe:stat-nlp"

restart: distclean build

rebuild: clean build

.PHONY: all test run clean distclean deps build rebuild hlint watch tags ghcid profile


SRC=$(shell find src -name '*.hs')

CABAL=cabal
FLAGS=--enable-tests --enable-library-profiling --enable-executable-profiling

all: init test docs package

init:
	${CABAL} sandbox init
	${CABAL} sandbox add-source ~/p/taygeta
	make deps

test: build
	${CABAL} test --test-option=--color

specs: build
	./dist/build/stat-nlp-specs/stat-nlp-specs

run:
	${CABAL} run corpora/gutenberg/


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
	hasktags --ctags *.hs src

hlint:
	hlint *.hs src specs

clean:
	${CABAL} clean

distclean: clean
	${CABAL} sandbox delete

configure: clean
	${CABAL} configure ${FLAGS}

deps: clean
	${CABAL} install --only-dependencies --allow-newer ${FLAGS}
	make configure

stat-nlp.ps: stat-nlp.hp
	hs2ps -e8in -c $<

build:
	${CABAL} build

watch:
	arion . src specs

restart: distclean init build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild hlint watch

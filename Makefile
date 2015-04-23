
build: HelloWorld

HelloWorld: cabal.sandbox.config HelloWorld.lhs
	cabal exec -- ghc --make HelloWorld.lhs

init: cabal.sandbox.config deps

cabal.sandbox.config:
	stackage sandbox init

deps: cabal.sandbox.config
	cabal install text

upgrade:
	stackage sandbox upgrade

clean:
	-rm -f *.o *.hi *.html

distclean: clean
	-rm -f HelloWorld
	stackage sandbox delete

%.html: *.lhs
	pandoc --from markdown+lhs --to html5 --smart --standalone --output=$@ $<

.PHONY: init deps build clean distclean upgrade

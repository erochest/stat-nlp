
build: HelloWorld

HelloWorld: cabal.sandbox.config HelloWorld.lhs
	cabal exec -- ghc --make HelloWorld.lhs

init: cabal.sandbox.config

cabal.sandbox.config:
	cabal sandbox init

clean:
	rm -f *.o *.hi *.html

distclean: clean
	rm -f HelloWorld
	cabal sandbox delete

%.html: *.lhs
	pandoc --from markdown+lhs --to html5 --smart --standalone --output=$@ $<

.PHONY: init build clean distclean

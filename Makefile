
build: HelloWorld

HelloWorld: cabal.sandbox.config HelloWorld.lhs
	cabal exec -- ghc --make HelloWorld.lhs

init: cabal.sandbox.config

cabal.sandbox.config:
	cabal sandbox init

clean:
	rm -f *.o *.hi

distclean: clean
	rm -f HelloWorld
	cabal sandbox delete

.PHONY: init build clean distclean

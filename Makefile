
chapters=Chapter01

build: $(chapters)

$(chapters) : % : %.hs cabal.sandbox.config
	cabal exec -- ghc --make $<

init: cabal.sandbox.config deps

cabal.sandbox.config:
	cabal sandbox init
	cabal sandbox add-source ~/p/taygeta

deps: cabal.sandbox.config
	cabal install taygeta \
		text-format \
		system-filepath system-fileio \
		conduit conduit-combinators conduit-extra \
		unordered-containers

clean:
	-rm -f *.o *.hi *.html

distclean: clean
	-rm -f $(chapters)
	cabal sandbox delete

reboot: distclean init

%.html: *.lhs
	pandoc --from markdown+lhs --to html5 --smart --standalone --output=$@ $<

.PHONY: init deps build clean distclean reboot

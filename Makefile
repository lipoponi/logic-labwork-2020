all: Main.hs
	$(MAKE) -C A all
	ghc Main.hs -outputdir ./out

run: Main
	./Main

clean:
	$(MAKE) -C A clean
	rm -rf ./out Main
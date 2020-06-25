all: Main.hs
	$(MAKE) -C A all
	$(MAKE) -C B all
	ghc -O2 -XBangPatterns Main.hs -outputdir ./out

run: Main
	./Main

clean:
	$(MAKE) -C A clean
	$(MAKE) -C B clean
	rm -rf ./out Main archive.zip

zip: clean
	zip -r archive.zip Makefile Main.hs ./A ./B ./Data
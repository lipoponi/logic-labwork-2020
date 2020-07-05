all: Main.hs
	$(MAKE) -C A all
	$(MAKE) -C B all
	$(MAKE) -C C all
	$(MAKE) -C D all
	ghc -O2 -XBangPatterns Main.hs -outputdir ./out

run: Main
	./Main

clean:
	$(MAKE) -C A clean
	$(MAKE) -C B clean
	$(MAKE) -C C clean
	$(MAKE) -C D clean
	rm -rf ./out Main archive.zip

zip: clean
	zip -r archive.zip Makefile Main.hs ./A ./B ./C ./D ./Data
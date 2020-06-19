all: Main.hs Parser.y Lexer.x
	alex Lexer.x
	happy Parser.y
	ghc Main.hs -outputdir ./out

run: Main
	./Main

clean:
	rm -rf ./out
	rm Lexer.hs Parser.hs Main
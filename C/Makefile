all: Lexer.x Parser.y
	alex Lexer.x --ghc
	happy Parser.y --ghc

clean:
	rm -f Lexer.hs Parser.hs
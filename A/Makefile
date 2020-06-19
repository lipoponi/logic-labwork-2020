all: Lexer.x Parser.y
	alex Lexer.x
	happy Parser.y

clean:
	rm -f Lexer.hs Parser.hs
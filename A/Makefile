all: Lexer.x Parser.y
	alex Lexer.x
	happy Parser.y

clean:
	rm Lexer.hs Parser.hs
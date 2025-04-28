Parser.hs: Parser.y
	#For legibility and info file:
	#happy --ghc -i -o Parser.hs Lang.y

	#For performance:
	happy --ghc -a -g -c -o Parser.hs Parser.y

ghci: Parser.hs
	ghci Parser.hs

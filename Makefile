CC=ghc
LX=alex

default : clean MinisculusParser

MinisculusParser: MinisculusLexer.hs AST.hs ParseTree.hs MinisculusParser.hs
	$(CC) MinisculusParser.hs

MinisculusLexer.hs: MinisculusLexer.x
	$(LX) MinisculusLexer.x

clean: 
	-rm MinisculusParser MinisculusLexer.hs *.o *.hi

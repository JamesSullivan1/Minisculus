CC=ghc
LX=alex

default : clean MinisculusParser 

MinisculusParser: MinisculusLexer.hs MinisculusAST.hs MinisculusParser.hs MinisculusError.hs Main.hs 
	$(CC) Main.hs -o MinisculusParser

MinisculusLexer.hs: MinisculusLexer.x
	$(LX) MinisculusLexer.x

clean: 
	-rm MinisculusParser MinisculusLexer.hs *.o *.hi

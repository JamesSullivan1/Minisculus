CC=ghc
LX=alex

default : clean MinisculusCodeGenerator 

MinisculusCodeGenerator: MinisculusLexer.hs MinisculusParser.hs MinisculusAST.hs MinisculusCodeGenerator.hs MinisculusError.hs Main.hs
	$(CC) Main.hs -o MinisculusCodeGenerator

MinisculusLexer.hs: MinisculusLexer.x
	$(LX) MinisculusLexer.x

clean: 
	-rm MinisculusCodeGenerator MinisculusLexer.hs *.o *.hi

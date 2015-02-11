CC=ghc
LX=alex

default : clean mcc 

mcc: MinisculusLexer.hs MinisculusParser.hs MinisculusAST.hs MinisculusCodeGenerator.hs MinisculusError.hs Main.hs
	$(CC) Main.hs -o mcc 
	-rm *.o *.hi 

MinisculusLexer.hs: MinisculusLexer.x
	$(LX) MinisculusLexer.x

clean: 
	-rm mcc MinisculusLexer.hs *.o *.hi

default: src 

.PHONY: src
src:
	$(MAKE) -C src/
	mv src/mcc .

clean:
	$(MAKE) -C src/ clean
	-rm mcc 


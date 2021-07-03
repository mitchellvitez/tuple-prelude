run: clean
	ghc Main -ddump-splices
	./Main

clean:
	rm -rf Main *.o *.dyn_o *.hi *.dyn_hi

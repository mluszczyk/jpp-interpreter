all:
	stack build --ghc-options="-Wall -Werror"


clean:
	stack clean


bnfc:
	bash runbnfc.sh

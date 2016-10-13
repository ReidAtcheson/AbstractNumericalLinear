
all : floatarrays polynomials

floatarrays : 
	ocamlbuild  'floatarrays.native'

polynomials : 
	ocamlbuild  'polynomials.native'







.PHONY : clean floatarrays polynomials



clean :	
	rm -rf ./polynomials.native
	rm -rf ./floatarrays.native
	rm -rf ./_build

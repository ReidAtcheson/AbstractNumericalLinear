
all : floatarrays polynomials

floatarrays : 
	ocamlbuild -tag annot 'floatarrays.native'

polynomials : 
	ocamlbuild  -tag annot 'polynomials.native'







.PHONY : clean floatarrays polynomials




clean :	
	ocamlbuild -clean

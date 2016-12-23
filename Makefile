
all : floatarrays polynomials infinite

infinite : 
	ocamlbuild  'infinite.native'

floatarrays : 
	ocamlbuild   'floatarrays.native'

polynomials : 
	ocamlbuild   'polynomials.native'







.PHONY : clean floatarrays polynomials




clean :	
	ocamlbuild -clean

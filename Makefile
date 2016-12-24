all : floatarrays polynomials infinite bench test

bench : 
	ocamlbuild  -pkg unix 'bench.native'
test : 
	ocamlbuild 'test.native'

infinite : 
	ocamlbuild  'infinite.native'

floatarrays : 
	ocamlbuild   'floatarrays.native'

polynomials : 
	ocamlbuild   'polynomials.native'







.PHONY : clean 




clean :	
	ocamlbuild -clean

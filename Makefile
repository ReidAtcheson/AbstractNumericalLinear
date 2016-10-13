main : 
	ocamlbuild  'floatarrays.native'





.PHONY : clean



clean :	
	rm -rf ./floatarrays.native
	rm -rf ./_build

main : 
	ocamlbuild  'main.native'





.PHONY : clean



clean :	
	rm -rf ./main.native
	rm -rf ./_build

# Numerical Linear Algebra in the Abstract

Not an Ocaml expert, but playing with its
facilities for abstraction to try and
define important numerical linear algebra
computations in the abstract, ideally 
without direct reference to matrix or 
vector entries, or even the
underlying representation of 
numbers.


These will not be efficient, nor probably
interesting to anyone else.




# What is implemented thus far?

* Gram-schmidt orthogonalization
* Operator adjoint
  * Computed via Riesz representation theorem argument
* Operator norm
  * Computed as sqrt of spectral radius of A'*A where A' is adjoint of A
  * Uses power iteration
  * (So may not converge if spectral radius has multiplicity>1)




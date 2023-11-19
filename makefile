all: 
	dune exec ray --profile=release
b: 
	dune build --profile=release
c:
	dune clean
w:
	dune build -w

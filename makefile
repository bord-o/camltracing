run: 
	dune exec ray --profile=release
build: 
	dune build --profile=release
clean:
	dune clean
watch:
	dune build -w
promote:
	dune promote

runtest:
	dune runtest

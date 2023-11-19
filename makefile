run: 
	dune exec ray --profile=release
build: 
	dune build --profile=release
clean:
	dune clean
watch:
	dune build -w
promote:
	dune promote  --profile=release

runtest:
	dune runtest  --profile=release

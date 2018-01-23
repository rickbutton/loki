compile: prereq compiler.scm
	gsi compiler.scm
	wat2wasm bin/a.wat -o bin/a.wasm --debug-names

webserver:
	http-server

prereq:
	if not exist bin mkdir -p bin

clean:
	rm bin/*
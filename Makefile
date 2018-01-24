DIR := ${CURDIR}

compile: prereq compiler.scm
	gsi compiler.scm
	jrepl1.0 "\^\^-" "(; " /f .\bin\a.wat /o -
	jrepl1.0 "-\^\^" " ;)" /f .\bin\a.wat /o -
	wat2wasm bin/a.wat -o bin/a.wasm --debug-names

webserver:
	http-server

prereq:
	if not exist bin mkdir -p bin

clean:
	rm bin/*
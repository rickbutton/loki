compile: prereq
	scheme --script src/compiler.scm test.scm bin/a.wat
	.\util\jrepl1.0 "\^\^-" "(; " /f .\bin\a.wat /o -
	.\util\jrepl1.0 "-\^\^" " ;)" /f .\bin\a.wat /o -
	wat2wasm bin/a.wat -o bin/a.wasm --debug-names

run: compile
	node --expose-wasm bootstrap.js bin/a.wasm
	
debug: compile
	node --inspect --debug-brk --expose-wasm bootstrap.js bin/a.wasm

webserver:
	http-server

prereq:
	if not exist bin mkdir -p bin

clean:
	rm bin/*
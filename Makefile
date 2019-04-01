compile: prereq
	guile -L src/ src/schwasm.scm  test2.scm bin/a.wat
	wat2wasm.exe --debug-names bin/a.wat -o bin/a.wasm

run: compile
	node --expose-wasm bootstrap.js bin/a.wasm
	
debug: compile
	node --inspect-brk --expose-wasm bootstrap.js bin/a.wasm

http:
	http-server

prereq:
	mkdir -p bin

clean:
	rm bin/*
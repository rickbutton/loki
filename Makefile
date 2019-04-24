compile: prereq
	chibi-scheme -I compiler compiler/schwasm.scm test2.scm bin/a.wat bin/a.funcs
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
NODE_FLAGS= --expose-wasm --experimental-modules \
		    --experimental-wasm-return_call \
		    --experimental-wasm-anyref

example: examples/test.wasm examples/test.wat
	node $(NODE_FLAGS) host/src/node.mjs examples/test.wasm

example-debug: examples/test.wasm examples/test.wat
	node --inspect-brk $(NODE_FLAGS) host/src/node.mjs examples/test.wasm

test:
	chibi-scheme -I compiler/src -I compiler/tests compiler/tests/tests.scm

%.wat: %.scm compiler/src/**
	chibi-scheme -I compiler/src compiler/src/loki.scm $< $@

%.wasm: %.wat
	wat2wasm --enable-reference-types --enable-tail-call --debug-names $< -o $@

http:
	http-server

clean: 
	rm -f examples/*.wasm examples/*.wat

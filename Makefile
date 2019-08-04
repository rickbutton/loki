V8_FLAGS= --experimental-wasm-return_call \
		  --experimental-wasm-anyref

CHROME_FLAGS=--js-flags="$(V8_FLAGS)"
NODE_FLAGS=--expose-wasm --experimental-modules $(V8_FLAGS)

WABT_FLAGS=--enable-reference-types --enable-tail-call
		   
example: examples/test.wasm examples/test.wat
	node $(NODE_FLAGS) host/src/node.mjs examples/test.wasm

example-debug: examples/test.wasm examples/test.wat
	node --inspect-brk $(NODE_FLAGS) host/src/node.mjs examples/test.wasm

test:
	chibi-scheme -I compiler/src -I compiler/tests compiler/tests/tests.scm

%.wat: %.scm compiler/src/**
	chibi-scheme -I compiler/src compiler/src/loki.scm $< $@

%.wasm: %.wat
	wat2wasm $(WABT_FLAGS) --debug-names $< -o $@

chrome:
	chromium-browser $(CHROME_FLAGS)

http:
	npx http-server

clean: 
	rm -f examples/*.wasm examples/*.wat

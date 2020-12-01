V8_FLAGS= --experimental-wasm-return_call \
		  --experimental-wasm-anyref

CHROME_FLAGS=--js-flags="$(V8_FLAGS)"
NODE_FLAGS=--expose-wasm --experimental-modules $(V8_FLAGS)

WABT_FLAGS=--enable-reference-types --enable-tail-call

SCM=gosh
SCM_FLAGS=-A src
COMPILE_SCM=$(SCM) $(SCM_FLAGS) src/loki.scm --
		   
example: examples/test.wasm examples/test.wat
	node $(NODE_FLAGS) host/src/node.mjs examples/test.wasm

example-debug: examples/test.wasm examples/test.wat
	node --inspect-brk $(NODE_FLAGS) host/src/node.mjs examples/test.wasm

test:
	$(COMPILE_SCM) src/tests.scm

repl:
	$(COMPILE_SCM) src/repl.scm

compiler:
	$(COMPILE_SCM) src/yggdrasil.scm

%.wasm: %.wat
	wat2wasm $(WABT_FLAGS) --debug-names $< -o $@

http:
	npx http-server

clean: 
	find . -name "*.so" -type f -delete
	rm -f examples/*.wasm examples/*.wat

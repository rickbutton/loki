V8_FLAGS= --experimental-wasm-return_call \
		  --experimental-wasm-anyref

CHROME_FLAGS=--js-flags="$(V8_FLAGS)"
NODE_FLAGS=--expose-wasm --experimental-modules $(V8_FLAGS)

WABT_FLAGS=--enable-reference-types --enable-tail-call

SCM=gosh
SCM_FLAGS=-A src
COMPILE_SCM=$(SCM) $(SCM_FLAGS) src/loki.scm --
		   
test:
	$(COMPILE_SCM) src/tests.scm

repl:
	$(COMPILE_SCM) src/repl.scm

example:
	#$(COMPILE_SCM) examples/yggdrasil.scm
	$(COMPILE_SCM) src/loki/compiler/expander.sld

%.wasm: %.wat
	wat2wasm $(WABT_FLAGS) --debug-names $< -o $@

http:
	npx http-server

clean: 
	find . -name "*.so" -type f -delete
	rm -f examples/*.wasm examples/*.wat

format-all:
	./scripts/format.sh

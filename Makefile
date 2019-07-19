RUST_TARGET = wasm32-unknown-unknown
RUST_ARGS = --release --target $(RUST_TARGET)
RUST_TOML = runtime/Cargo.toml

RUST_OUT_DIR = runtime/target/wasm32-unknown-unknown/release
RUST_RUNTIME_WASM = runtime/target/wasm32-unknown-unknown/release/runtime.wasm

example: examples/runtime.wat examples/test.wasm examples/test.wat
	node --expose-wasm --experimental-modules host/src/node.mjs examples/runtime.wasm examples/test.wasm

test:
	chibi-scheme -I compiler/src -I compiler/tests compiler/tests/tests.scm

%.wat: %.scm compiler/src/**
	chibi-scheme -I compiler/src compiler/src/schwasm.scm $< $@

%.wasm: %.wat
	wat2wasm --debug-names $< -o $@

$(RUST_RUNTIME_WASM): runtime/src/**
	cargo +nightly build $(RUST_ARGS) --manifest-path $(RUST_TOML)

examples/runtime.wasm:$(RUST_RUNTIME_WASM)
	cp $< $@

examples/runtime.wat: examples/runtime.wasm
	wasm2wat $< -o $@

http:
	http-server

clean:
	rm -f examples/*.wasm examples/*.wat
	rm -rf runtime/target

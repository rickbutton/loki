# loki

### A Scheme to WebAssembly Compiler

<a href="https://github.com/rickbutton/loki/actions?query=workflow%3Atests"><img alt="tests" src="https://github.com/rickbutton/loki/workflows/tests/badge.svg"></a>

This supports almost zero percent of any scheme standard or spec. I'm hoping to eventually make this R7RS small compliant at least. No promises.

Take caution, because this probably doesn't work on your machine without tweaks.

#### Why is it called loki?

In Norse mythology, Loki is a mischievous trickster. Attemting to run scheme in the browser is the epitome of mischief.

### How to run

You will need [Gauche Scheme](https://github.com/shirok/Gauche) on your path.

```
make compiler
```

### Things that work?

Right now, nothing. `loki` is currently going through a major rewrite. `loki` currently compiles `R7RS` down to a very small subset of Scheme ran with `eval`. The next step is to implement a new CPS-bytecode, scheme-to-CPS-bytecode-transform , and CPS-bytecode-interpreter. Once done, the next step will be to glue that bytecode to the existing WASM backend.

### Credits

Thanks to André van Tonder, who wrote the [SRFI-72 reference implementation](https://srfi.schemers.org/srfi-72/srfi-72.html) that I ported to R7RS.

Thanks to Göran Weinholt, who wrote [laesare](https://github.com/weinholt/laesare), which I forked for loki.

Thanks to Alex Shinn, who wrote [chibi scheme](https://github.com/ashinn/chibi-scheme), the source of many of loki's standard libraries.

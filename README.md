# loki

### A Scheme to WebAssembly Compiler

This supports almost zero percent of any scheme standard or spec. I'm hoping to eventually make this R7RS small compliant at least. No promises.

Take caution, because this probably doesn't work on your machine without tweaks.

#### Why is it called loki?

In Norse mythology, Loki is a mischievous trickster. Attemting to run scheme in the browser is the epitome of mischief.

### How to run

You will need [chibi scheme](https://github.com/ashinn/chibi-scheme/), [node](https://nodejs.org), and [wabt](https://github.com/WebAssembly/wabt) on your path.

```
make example
```

### Things that work?

Right now, nothing. `loki` is currently going through a major rewrite. The front-end has a macro expander and module system, and I am currently working on implementing the rest of the standard library. Once done, the next step will be to glue it back onto the backend.

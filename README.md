# loki

### A R7RS Scheme for the Web

<a href="https://github.com/rickbutton/loki/actions?query=workflow%3Atests"><img alt="tests" src="https://github.com/rickbutton/loki/workflows/tests/badge.svg"></a>

loki is a (almost!) R7RS-compliant Scheme implementation. loki currently runs on top of Gauche Scheme, but in the future will run on top of a custom virtual machine written in Rust.

#### Why is it called loki?

In Norse mythology, Loki is a mischievous trickster. Attemting to run scheme in the browser is the epitome of mischief.

#### Isn't implementing a Scheme on top of another Scheme cheating?

Only to a point. loki compiles down to a small subset of Scheme barely larger than the untyped lambda calculus, so while the "backend" is a Gauche Scheme "eval", it will be simple to port to another implementation. Starting loki's implementation on top of another Scheme is an intentional part of the bootstrap process!

### How to run

You will need [Gauche Scheme](https://github.com/shirok/Gauche) on your path.

```
make test
```

### Credits

Thanks to André van Tonder, who wrote the [SRFI-72 reference implementation](https://srfi.schemers.org/srfi-72/srfi-72.html) that I ported to R7RS.

Thanks to Göran Weinholt, who wrote [laesare](https://github.com/weinholt/laesare), which I forked for loki.

Thanks to Alex Shinn, who wrote [chibi scheme](https://github.com/ashinn/chibi-scheme), the source of many of loki's standard libraries.

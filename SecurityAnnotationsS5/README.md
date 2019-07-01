S5 with Security Annotations
=====================================

This repository contains a reference implementation of Security Annotations within S5.

Acknowledgments
---------------
This work builds upon the [original reference implementation](https://github.com/brownplt/LambdaS5) of S5.

Installation
-----------------
Approximate system requirements - these should be the same as the original implementation of S5, although we used:
* macOS High Sierra (though any standard-ish Linux distribution should work)
* Ocaml 4.02
* Python 2.7
* `sh`, `make`, `autoconf`, etc.

To build S5, you need to build the ocamlgraph library and copy some files to
the right spot:

1. `cd src/ocamlgraph-1.8.1`
2. `autoconf && ./configure`
3. `make`
4. `cp graph.cmi graph.cma ../../lib/`
5. `cd ..`
6. `make`

Before you can run anything, you need to make sure that `bin/js` is linked to an appropriate executable for your OS.  The default is `js-1.8.5-Darwin-i386`, which should work on macOS systems.

Usage
-----------------
`scripts/` provides 3 helper scripts for running S5 programs.

* `ls5.sh` takes a JavaScript program as input, desugars it into a S5 program and evaluates that S5 program.
* `ls5-desugar.sh` takes a JavaScript program as input and desugars it, producing an S5 program in a file with the `.ls5` suffix.
* `ls5-eval.sh` takes an S5 program and evaluates it.

In `examples/` we provide a series of example programs which focus on Security Annotations.

Since JavaScript programs do not carry annotations, the JavaScript programs included within `examples/js/` serve as a base for the desugared programs without Security Annotations.

The S5 programs themselves are examples which deal with Security Annotations. The output of each program is contained in an accompanying `.output` file in `examples/output/`.
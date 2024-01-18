# smlbody

Proof of concept of combining
[sml-tigr](https://github.com/diku-dk/sml-tigr) and
[Futhark](https://futhark-lang.org) through
[smlfut](https://github.com/diku-dk/smlfut).

This is an N-body simulation where the physics computations are done
by Futhark, and the GUI and overall control flow is implemented in
Standard ML.

Currently the Makefile is set up to use Futhark's `multicore` backend,
but there is nothing in the implementation that prevents the GPU
backends from working.

## Dependencies

* [`futhark`](https://futhark-lang.org)

* [`mlkit`](https://elsman.com/mlkit/)

* [`smlpkg`](https://github.com/diku-dk/smlpkg)

* [`smlfut`](https://github.com/diku-dk/smlfut)

* Whatever X11/OpenGL libraries are needed by [TIGR](https://github.com/erkkah/tigr).

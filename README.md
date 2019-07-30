HoCL 
====

**HoCL** (Higher Order dataflow Coordination Language) is a language for describing dataflow networks
and generating tool-specific descriptions.

**HoCL** comes with :

- a higher-order functional language for describing programs built from dataflow actors 
- automatic _type inference_ and type-checking
- the possibility to define _higher order wiring functions_ for describing and encapsulating _graph
  patterns_
- a [DOT][graphviz] backend for visualisation of the generated networks
- a [PREESM][preesm] backend for implementing the described dataflow applications on many/multi-core
  embedded platforms
- a [SystemC][systemc] backend for code simulation

**HoCL** is a joint project between the [Dream][dream] and [Vaader][vaader] research groups.

[graphviz]: http://www.graphviz.org
[preesm]: https://preesm.github.io
[systemc]: https://www.accellera.org/downloads/standards/systemc
[dream]: https://dream.ispr-ip.fr
[vaader]: https://www.ietr.fr/spip.php?article1604

DOCUMENTATION
------------

Here's a short [tutorial](https://github.com/jserot/hocl/blob/master/doc/tutorial.pdf) on the language.

INSTALLATION
------------

Pre-requisites :

* [Ocaml](http://ocaml.org/docs/install.html) version >= 4.06.0 with the following package(s)
    - [menhir](https://opam.ocaml.org/packages/menhir)
    - [dune](https://opam.ocaml.org/packages/dune)

Download the source tree (`git clone https://github.com/jserot/hocl`).

From the root of the source tree :

1. `./configure [options]`  (`./configure --help` for the list of options)
2. `make`
3. `make install`

To try examples :

1. go the directory containing the example (*e.g.* `cd examples/working/simple/basic`)
2. type `make dot` to generate the `.dot` representation (`make` will also display it)
3. type `make preesm` to invoke the PREESM backend (code will be generated in
   sub-directory `preesm`)
4. type `make systemc` to invoke the SystemC backend (code will be generated in
   sub-directory `systemc`); to test the generated code: `cd systemc; make`

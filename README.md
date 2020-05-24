HoCL 
====

**HoCL** (Higher Order dataflow Coordination Language) is a language for describing dataflow process
networks and generating tool-specific descriptions.

**HoCL** 

- can describe _hierarchical_ and/or _parameterized_ graphs
- support two styles of description : _structural_ and _functional_
- use _polymorphic type inference_ to check graphs
- supports the notion of _higher order wiring functions_ for describing and encapsulating _graph
  patterns_
- supports several dataflow semantics (SDF, PSDF, ..)  by means of annotations

The generated code is independent of the target implementation platform (software, hardware, mixed,
..). Targeting is done using dedicated backends. The current version
comes is equipped with five backends :

- a [DOT][graphviz] backend for visualisation of the generated networks
- a [SystemC][systemc] backend for simulation
- a [PREESM][preesm] backend for implementing the described dataflow applications on many/multi-core
  embedded platforms
- a [DIF][dif] backend for interfacing to various dataflow analysis tools
- an XDF backend for interfacing to CAL-based design flows

**HoCL** is a joint project between the [Dream][dream] and [Vaader][vaader] research groups.

[graphviz]: http://www.graphviz.org
[preesm]: https://preesm.github.io
[systemc]: https://www.accellera.org/downloads/standards/systemc
[dream]: https://dream.ispr-ip.fr
[vaader]: https://www.ietr.fr/spip.php?article1604
[dif]: https://www.researchgate.net/publication/220714226_DIF_An_Interchange_Format_for_Dataflow-Based_Design_Tools

DOCUMENTATION
------------

A short [tutorial](https://github.com/jserot/hocl/blob/master/doc/tutorial.pdf) on the language.

A minimal [user manual](https://github.com/jserot/hocl/blob/master/doc/using.pdf) describing how to
invoke the compiler.

A [gentle introduction](https://github.com/jserot/hocl/blob/master/doc/fgd.pdf) to the concepts of
_functional graph description_.

A BNF description of the syntax, in [pdf](https://github.com/jserot/hocl/blob/master/doc/syntax.pdf)
and [html](https://github.com/jserot/hocl/blob/master/doc/syntax.html)

The [formal semantics](https://github.com/jserot/hocl/blob/master/doc/semantics.pdf) of the
language.

INSTALLATION
------------

Pre-requisites :

* [Ocaml](http://ocaml.org/docs/install.html) version >= 4.08.0 with the following package(s)
    - [menhir](https://opam.ocaml.org/packages/menhir)
    - [dune](https://opam.ocaml.org/packages/dune)
    - [ezxmlm](https://opam.ocaml.org/packages/ezxmlm)

Download the source tree (`git clone https://github.com/jserot/hocl`).

From the root of the source tree :

1. `./configure [options]`  (`./configure --help` for the list of options)
2. `make`
3. `make install`

To try examples :

1. go the directory containing the example (*e.g.* `cd examples/working/simple/basic`)
2. type `make dot` to generate the `.dot` representation (`make` will also display it)
3. type `make systemc` to invoke the SystemC backend (code will be generated in
   sub-directory `systemc`); to test the generated code: `cd systemc; make`
4. type `make preesm` to invoke the PREESM backend (code will be generated in
   sub-directory `preesm`)

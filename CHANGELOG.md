# Changes

# 1.2 (May 18, 2020)

* type-based handling of parameters (allow parameters also on outputs)
* Revised formal semantics and updated implementation accordingly

# 1.1 (May 3, 2020)

* Complete source code recrafting, now phased with the provided formal semantics
* Added documentation (BNF syntax, formal semantics, compiler usage, tutorial)
* Got rid of ad-hoc notation for parameter passing. Parameters are now passed
  "normally" as curried arguments
* Allow definitions of polymorphic actors (such as `delay`, `switch` and `merge`), to
  be interpreted specifically by backend. 

# 1.0 (Mar 7, 2020)

* Fully revised syntax, with `node` and `graph` declarations, and a new way of
  declaring and passing parameters 
* The tutorial has been completely rewritten to reflect these changes
* The so-called `prelude` file is now called the _standard library_ (`lib/hocl/stdlib.hcl`). This
  file is now automatically parsed by the compiler, unless the `-no_stdlib` argument is given
* Increased the number of SystemC examples 
* `switch` and `merge` dedicated actors for expressing DDF-style conditional (see 
   `examples/working/misc/cond_ddf`)
* Added an (minimal) DIF backend

# 0.3 (Nov 10, 2019)
* Updated implementation of _broadcast_ nodes. Insertion of _broadcast_ node is now automatic when
  using the SystemC backend (both for parameter and data flows). When using the Preesm backend,
  insertion of parameter-less (aka "implicit") broadcast nodes on the data flows is also automatic.
  Definition and usage of parameterized, explicit, broadcasting actors is also possible.
  See `examples/working/bcast[1-3]`. 
* Support for _delay_ actors (see for ex. `examples/working/apps/idiff[1-2]`).
* Dedicated syntax for annotating IO ports with SDF production-consumption rate
  Ex: `in i: int[size+2]`  (where `size` is a parameter)
  See `examples/working/sdf/sdf[1-2]` and `examples/working/apps/{sobel1,idiff1,idiff2}` for ex.
  Extra annotations can still be passed with the `{ann}` syntax.
* The SystemC backend can now be used to simulate applications reading and displaying video streams 
  (using the [SDL2](https://www.libsdl.org) library).
  See for ex. `examples/working/apps/{video,sobel1,idiff1,idiff2}`.
* Added several functions and tools for converting YUV-encoded video stream files to PGM and PPM
  format (see `tools/yuvutils`)
* Revised implementation of the SystemC backend. Actors now implemented as `SC_THREAD`s 
* Reorganized the `examples` directory

# 0.2 (Jul 30, 2019)
* Support for hierarchical graphs (see `examples/preesm/hier[1-4]`)
* Support for broadcast actors (see `examples/preesm/bcast`)
* Added target `preesm.proj` to automatically build a Preesm ready-to-import directory
* Added tool `pi2hcl` to convert `.pi` files to `.hcl` (see `tools/pi2hcl/examples`)
* Build process now handled by `dune`
* Several bug fixes

# 0.1 (May 18, 2019)
* First public version
* Preliminary backends for : DOT, Preesm and SystemC
* No dynamic parameters, no hierarchy
* Higher-order wiring functions

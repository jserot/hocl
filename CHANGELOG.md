# Changes

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

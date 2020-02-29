// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 1.0a)
// from file(s) main.hcl, on 2020-02-26 at 15:08:46, by jserot
// -------------------------------------------------------------------------------

#include <systemc.h>
#include <iostream>
#include "bcast.h"
#include "inParam.h"
#include "top_gph.h"

int sc_main(int argc, char* argv[]) {
  sc_clock clk("clk", 10, SC_NS, 0.5);

  // Toplevel parameters
  sc_signal<int> k;
  InParam<int> p1("p1",1);
  p1.clk(clk);
  p1.o(k);
  
  // Toplevel graphs
  Top top("top");
  top.clk(clk);
  top.k(k);

  sc_start(100, SC_NS);
  cout << "Simulation stopped at t=" << sc_time_stamp() << "\n";
  return EXIT_SUCCESS;
}

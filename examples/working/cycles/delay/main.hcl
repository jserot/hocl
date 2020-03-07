-- A simple graph with a delayed feedback
--
-- When [foo(x,z)=x+z,x], given an input sequence [x1,x2,...] on input [i],
-- this graph produces the sequence [x1,x2+x1,x3+x2,...] on output [o]
--
-- The [delay] actor is defined in the standard library and interpreted
-- specifically by the dedicated backends


node foo in (i1: int, i2: int) out (o1: int, o2: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end; 

graph top in (i: int) out (o: int)
fun
  val o = let rec (y,z) = foo (i, delay<0> z) in y
end;
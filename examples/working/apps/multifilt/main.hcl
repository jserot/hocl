-- An imaginary DSP application in which all possible sequences of three
-- filters are executed in parallel and the "best" result finally selected

type fix16;

node src in () out (o: fix16)
actor
  systemc(loop_fn="src_read", init_fn="src_init", incl_file="include/io.h", src_file="src/io.c")
  preesm(loop_fn="src_read", init_fn="src_init", incl_file="include/io.h", src_file="src/io.c")
end;

node snk in (o: fix16) out ()
actor
  systemc(loop_fn="snk_read", incl_file="include/io.h", snk_file="src/io.c")
  preesm(loop_fn="snk_read", incl_file="include/io.h", snk_file="src/io.c")
end;

node f1 in (p: int param, i: fix16) out (o: fix16)
actor
  systemc(loop_fn="f1", incl_file="include/compute.h", snk_file="src/compute.c")
  preesm(loop_fn="f1", incl_file="include/compute.h", snk_file="src/compute.c")
end;

node f2 in (p: int param, i: fix16) out (o: fix16)
actor
  systemc(loop_fn="f2", incl_file="include/compute.h", snk_file="src/compute.c")
  preesm(loop_fn="f2", incl_file="include/compute.h", snk_file="src/compute.c")
end;

node f3 in (p: int param, i: fix16) out (o: fix16)
actor
  systemc(loop_fn="f3", incl_file="include/compute.h", snk_file="src/compute.c")
  preesm(loop_fn="f3", incl_file="include/compute.h", snk_file="src/compute.c")
end;

node select in (thr: int param, i1: fix16, i2: fix16, i3: fix16) out (o: fix16)
actor
  systemc(loop_fn="select", incl_file="include/compute.h", snk_file="src/compute.c")
  preesm(loop_fn="select", incl_file="include/compute.h", snk_file="src/compute.c")
end;

graph top in (p: int param=2, thr: int param=128) out ()
fun
  val fs = [ f1 p; f2 p; f3 p ]
  val chain s x = x |> pipe (shuffle s fs)
  val sel c1 c2 c3 x = select thr (c1 x) (c2 x) (c3 x)
  val o = src |-> sel (chain [0;1;2]) (chain [1;2;0]) (chain [2;0;1]) 
end;

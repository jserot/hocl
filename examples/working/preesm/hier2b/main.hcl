-- A simple program showing a hierarchical graph description
-- The [sub] node included in the [top] toplevel graph  is itself a graph, here described functionally
-- The [sub] node uses a local parameter

node foo param (k: int) in (i: int) out (o: int)
actor
  preesm(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

node bar in (i: int) out (o: int)
actor
  preesm(loop_fn="bar", incl_file="./include/bar.h", src_file="./src/bar.cpp")
end;

node inp in () out (o: int)
actor
  preesm(init_fn="inputInit", loop_fn="input", incl_file="./include/input.h", src_file="./src/input.cpp")
end;

node outp in (i: int) out ()
actor
  preesm(loop_fn="output", incl_file="./include/output.h", src_file="./src/output.cpp")
end;

node sub param (k: int) in (i: int) out (o: int)
fun
  val o = i |> foo k |> bar
end;

graph top in () out ()
fun
  val _ = inp |-> sub 2 |> outp
end;

-- A simple program showing a hierarchical graph description
-- The [sub] node included in the [top] toplevel graph  is itself a graph, here described functionally
-- The [sub] node uses a local parameter

node foo param (k: int) in (i: int) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

node bar in (i: int) out (o: int)
actor
  systemc(loop_fn="bar", incl_file="./include/bar.h", src_file="./src/bar.cpp")
end;

node sub in (i: int) out (o: int)
fun
  val o = i |> foo 2 |> bar
end;

graph top in (i: int) out (o: int)
fun
  val o = sub i
end;

-- A simple graph with an implicit broadcast node on the data flow
-- Insertion of bcast nodes is automatic when using the SystemC and the Preesm backends
-- Otherwise, it is triggered by the [-insert_bcast] option

node foo in (i: int) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

graph top in (i: int) out (o1: int, o2: int)
fun
  val (o1, o2) = foo i, foo i
end;

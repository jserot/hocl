-- Parameters are implicitely broadcasted (IOW, the output port of a parameter box 
-- can be directly wired to several distinct input ports)
-- This interpretation is naturally supported by the Preesm backend
-- When using the SystemC backend, explicit bcast nodes are automatically inserted 
-- Inserted bcast nodes wan be viewed with the Dot backend by passing the [-insert_bcasts] option

node foo in (k: int param, i: int) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

graph top in (p: int param=1, i: int) out (o: int)
fun
  val o  = foo (p, foo (p, i))
end;

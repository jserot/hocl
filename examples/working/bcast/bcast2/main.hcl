-- Parameters are implicitely broadcasted (IOW, the output port of a parameter box 
-- can be directly wired to several distinct input ports)
-- This interpretation is naturally supported by the Preesm backend
-- When using the SystemC backend, explicit bcast nodes are automatically inserted 
-- Inserted bcast nodes wan be viewed with the Dot backend by passing the [-insert_bcasts] option

node foo param (k: int) in (i: int) out (o: int)
actor
  systemc(loop_fn="foo", incl_file="./include/foo.h", src_file="./src/foo.cpp")
end;

graph top param (p: int=1) in (i: int) out (o: int)
fun
  val o  = i |> foo<p> |> foo<p>
end;

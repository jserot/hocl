node inp in () out(o: int)
actor
  preesm(loop_fn="inp", init_fn="inpInit", incl_file="input.h", src_file="input.cpp")
end;

node foo param (k: int, p: int) in (i: int) out (o: int)
actor
  preesm(loop_fn="foo", incl_file="foo.h", src_file="foo.cpp")
end;
node outp param (p: int) in (i: int) out ()
actor
  preesm(loop_fn="outp", init_fn="outpInit", outcl_file="output.h", src_file="output.cpp")
end;

graph top
  param (k:int=2, p:int=2)
  in () out ()
fun
  val _ = inp |-> foo<k,p> |> outp<p>
end;

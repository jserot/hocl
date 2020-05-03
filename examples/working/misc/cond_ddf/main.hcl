-- DDF formulation of "if ... then ..else" using user-defined [switch] and [merge] actors
-- The interpretation of these actors is backend-dependent.
-- The SystemC library in the current distribution provides default ones.
--
-- In this example, even input tokens are incremented and odd ones doubled

node switch in (sel: bool, i: $t) out (o1: $t, o2: $t);

node merge in (sel: bool, i1: $t, i2: $t) out (o: $t);

node is_even in (i: int) out (o: bool)
actor
  systemc(loop_fn="is_even", incl_file="./include/cond.h", src_file="./src/cond.cpp")
end;

node incr in (i: int) out (o: int)
actor
  systemc(loop_fn="incr", incl_file="./include/cond.h", src_file="./src/cond.cpp")
end;

node double in (i: int) out (o: int)
actor
  systemc(loop_fn="mult2", incl_file="./include/cond.h", src_file="./src/cond.cpp")
end;

graph top in (i: int) out (o: int)
fun
  val cond p f g x = 
    let b = p x in
    let (x1,x2) = switch (b,x) in
    merge (b, f x1, g x2)
  val o = cond is_even incr double i
end;

-- DDF formulation of "if ... then ..else" using [switch] and [merge] actors
-- using the [switch] and [merge] actors defined in the standard library
-- In this example, even input tokens are incremented and odd ones doubled

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
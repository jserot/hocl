-- Reformulation of [../param1a] using partial application
-- Note the distinction in the signature of node [mult], signaling
-- that this node can be partially applied

node mult in (k: int param -> i: int data) out (o: int data)
actor
  systemc(loop_fn="mult", incl_file="../include/mult.h", src_file="../src/mult.cpp")
end;

graph top in (i: int data) out (o: int data)
fun
  val double = mult '2'
  val o = double i
end;

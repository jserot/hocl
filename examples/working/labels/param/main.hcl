-- This example shows how to use labels in conjunction with partial application

node foo in (k: int param, x: int) out (o:  bool);

node bar in (x: int, k: int param) out (o:  bool);
-- same as [foo] but this the [k] parameter as _second_ input

graph top in (i1: int, i2: int) out (o11: bool, o12:bool, o21: bool, o22:bool)
fun
  val o11 = foo '1' i1       -- full application of [foo]
  val o12 = i1 |> foo '1'    -- partial application of [foo], allowing use of [|>] 
  val o21 = bar i2 '1'       -- full application of [bar]
  val o22 = i2 |> bar k:'1'  -- partial appliction of [bar]. This would not be possible without label-based port binding !
end;


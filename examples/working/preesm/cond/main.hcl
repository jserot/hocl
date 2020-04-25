-- Transcription of https://github.com/preesm/preesm-apps/tree/master/org.ietr.preesm.switch-example

node dataProvider param (dataSize: int) in () out (data: int);
node forwardDatabis param (dataSize: int, select: bool) in (input: int) out (out_1: int, out_2: int);
node branchOne param (dataSize: int, select: bool) in (data: int) out ();
node branchZero param (dataSize: int, select: bool) in (data: int) out ();

  
-- Ad-hoc formulation

graph top1 param (dataSize: int=128, selector: bool=true) in () out ()
fun
  val o1,o2 = dataProvider<dataSize> |-> forwardDatabis<dataSize,selector> 
  val _ = o1 |> branchZero<dataSize,selector> 
  val _ = o2 |> branchOne<dataSize,selector> 
end;

-- Generic formulation using the [cond] HOWF

val cond frk b0 b1 x = 
  let (o1,o2) = x |> frk in
  o1 |> b0,
  o2 |> b1
;

graph top param (dataSize: int=128, selector: bool=true) in () out ()
fun
  val _ =
    dataProvider<dataSize> |-> 
    cond
      forwardDatabis<dataSize,selector>
      branchZero<dataSize,selector>
      branchOne<dataSize,selector>
end;


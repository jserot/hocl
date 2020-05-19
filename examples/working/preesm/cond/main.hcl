-- Transcription of [org.ietr.preesm.switch-example]
-- Two formulations : first, direct, low-order transcription, second using
-- the (here called) [ifthenelse] higher-order wiring function

type t;

node dataProvider in (dataSize: int param) out (o: t);

node selector in () out (value: int param);

node forwardDatabis
   in (dataSize: int param, select: int param, input: t) 
  out (out0:  t[size*(1-select)], out1: t[size*select]);

node branchZero in (select: int param, dataSize: int param, data: t) out ();
node branchOne in (select: int param, dataSize: int param, data: t) out ();

graph top1 in (dataSize: int param = 4) out () -- First formulation
fun
  val i = dataProvider dataSize
  val select = selector ()
  val o0, o1 = forwardDatabis dataSize select i
  val _ = branchZero select dataSize o0
  val _ = branchOne select dataSize o1
end;

-- Second formulation 

val ifthenelse sel fork f0 f1 x =
  let x0, x1 = fork sel x in
  f0 sel x0, f1 sel x1
;
  
graph top2 in (dataSize: int param = 4) out ()
fun
  val _ = 
       dataProvider dataSize        -- source
    |> ifthenelse
         (selector ())              -- condition
         (forwardDatabis ~dataSize) -- fork
         (branchZero ~dataSize)     -- 'false' subgraph
         (branchOne ~dataSize)      -- 'true' subgraph
end;

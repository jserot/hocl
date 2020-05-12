-- Transcription of [org.ietr.preesm.switch-example]

type t;

node dataProvider in (dataSize: int param) out (o: t);

node selector in () out (value: int param);

node forwardDatabis
   in (select: int param, size: int param, input: t) 
  out (out0:  t[size*(1-select)], out1: t[size*select]);

node branchZero in (select: int param, dataSize: int param, data: t) out ();
node branchOne in (select: int param, dataSize: int param, data: t) out ();

val cond sel fork f0 f1 x =
  let x0, x1 = fork sel x in
  f0 sel x0, f1 sel x1
;
  
graph top in (dataSize: int param = 4) out ()
fun
  val i = dataProvider(dataSize)
  val select = selector ()
  val fork sel x = forwardDatabis (sel,dataSize, x)
  val f0 sel x = branchZero (sel, dataSize, x)
  val f1 sel x = branchOne (sel, dataSize, x)
  val _ = cond end;

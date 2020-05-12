-- Transcription of [org.ietr.preesm.switch-example]

type t;

node dataProvider in (dataSize: int param) out (o: t);

node selector in () out (value: int param);

node forwardDatabis
   in (select: int param, size: int param, input: t) 
  out (out0:  t[size*(1-select)], out1: t[size*select]);

node branchZero in (select: int param, dataSize: int param, data: t) out ();
node branchOne in (select: int param, dataSize: int param, data: t) out ();

graph top in (dataSize: int param = 4) out ()
fun
  val i = dataProvider(dataSize)
  val select = selector ()
  val o0, o1 = forwardDatabis (select,dataSize, i)
  val _ = branchZero (select, dataSize, o0)
  val _ = branchOne (select, dataSize, o1)
end;

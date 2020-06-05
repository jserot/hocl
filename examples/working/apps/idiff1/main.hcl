type uchar;

node ReadYUV
  in (width: int param, height: int param)
  out (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
actor
  systemc(loop_fn="yuvRead", init_fn="yuvReadInit", incl_file="include/yuvRead.h", src_file="src/yuvRead.c")
  preesm(loop_fn="yuvRead", init_fn="yuvReadInit", incl_file="include/yuvRead.h", src_file="src/yuvRead.c")
end
;

node DisplayYUV
  in (id: int param, width: int param, height: int param,
      y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
  out ()
actor
  systemc(loop_fn="yuvDisplay", init_fn="yuvDisplayInit", incl_file="include/yuvDisplay.h", src_file="src/yuvDisplay.c")
  preesm(loop_fn="yuvDisplay", init_fn="yuvDisplayInit", incl_file="include/yuvDisplay.h", src_file="src/yuvDisplay.c")
end
;

node ImDiff
  in (width: int param, height: int param,
      inp: uchar[height*width], previous: uchar[height*width])
  out (outp: uchar[height*width], result: uchar[height*width])
actor
  systemc(loop_fn="im_diff", incl_file="include/im_diff.h", src_file="src/im_diff.c")
  preesm(loop_fn="im_diff", incl_file="include/im_diff.h", src_file="src/im_diff.c")
end;

node ImDelay
  in (width: int param, height: int param, ival: int param,
      inp: uchar[height*width])
  out (outp: uchar[height*width])
actor
  systemc(loop_fn="im_delay", init_fn="im_delay_init", incl_file="include/im_delay.h", src_file="src/im_delay.c", is_delay)
  preesm(loop_fn="im_delay", init_fn="im_delay_init", incl_file="include/im_delay.h", src_file="src/im_delay.c", is_delay)
end;

graph top
 in (width: int param = 352, height: int param = 288, ival: int param = 0, index: int param = 0)
 out () 
fun
  val (yi,u,v) = ReadYUV width height
  val yo =
    let rec (outp,result) = ImDiff width height yi (ImDelay width height ival outp) in
    result
  val _ = DisplayYUV index width height yo u v
end;

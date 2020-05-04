type uchar;

node ReadYUV
  in (width: int param, height: int param)
  out (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
actor
  systemc(loop_fn="yuvRead", init_fn="yuvReadInit", incl_file="include/yuvRead.h", src_file="src/yuvRead.c")
  preesm(loop_fn="yuvRead", init_fn="yuvReadInit", incl_file="include/yuvRead.h", src_file="src/yuvRead.c")
end;

node DisplayYUV
  in (id: int param, width: int param, height: int param,
      y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
  out ()
actor
  systemc(loop_fn="yuvDisplay", init_fn="yuvDisplayInit", incl_file="include/yuvDisplay.h", src_file="src/yuvDisplay.c")
  preesm(loop_fn="yuvDisplay", init_fn="yuvDisplayInit", incl_file="include/yuvDisplay.h", src_file="src/yuvDisplay.c")
end;

node Sobel
  in (width: int param, height: int param,
      input: uchar[height*width])
  out (output: uchar[height*width])
actor
  systemc(loop_fn="sobel", incl_file="include/sobel.h", src_file="src/sobel.c")
  preesm(loop_fn="sobel", incl_file="include/sobel.h", src_file="src/sobel.c")
end;

graph main
  in (width: int param = 352, height: int param = 288, index: int param = 0)
  out () 
fun
  val (yi,u,v) = ReadYUV (width,height)
  val yo = Sobel (width,height,yi) 
  val _ = DisplayYUV (index,width,height,yo,u,v)
end;

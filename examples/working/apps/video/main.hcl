type uchar;

node ReadYUV
  param (width: int, height: int)
  in ()
  out (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
actor
  systemc(loop_fn="yuvRead", init_fn="yuvReadInit", incl_file="include/yuvRead.h", src_file="src/yuvRead.c")
end
;

node DisplayYUV
  param (id: int, width: int, height: int)
  in (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
  out ()
actor
  systemc(loop_fn="yuvDisplay", init_fn="yuvDisplayInit", incl_file="include/yuvDisplay.h", src_file="src/yuvDisplay.c")
end
;

graph top
  param (width: int=352, height: int=288, index: int=0)
  in ()
  out () 
fun
  -- val (yi,u,v) = ReadYUV<width,height>()
  -- val () = DisplayYUV<index,width,height>(yi,u,v)
  val _ = ReadYUV<width,height> |-> DisplayYUV<index,width,height>
end;

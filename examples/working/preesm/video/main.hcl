type uchar;

-- parameter width: int = 352;
-- parameter height: int = 288;
-- parameter index: int = 0;

actor Read_YUV
  param (width: int, height: int)
  in ()
  out (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
;

actor Display_YUV
  param (id: int, width: int, height: int)
  in (y: uchar[height*width], u: uchar[height/2*width/2], v: uchar[height/2*width/2])
  out ()
;

graph main
  -- param (width: int = 352, height: int = 288, index: int = 0)
  param (width: int, height: int, index: int)
  in ()
  out () 
fun
  val (yi,u,v) = Read_YUV<width,height>()
  val () = Display_YUV<index,width,height>(yi,u,v)
end;

graph top in () out ()
fun
  val _ = main<352,288,0>()
end;

-- #pragma code("Read_YUV", "include/yuvRead.h", "readYUV", "initReadYUV")
-- #pragma code("Display_YUV", "include/yuvDisplay.h", "yuvDisplay", "yuvDisplayInit")


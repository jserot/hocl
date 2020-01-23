type uchar;

-- parameter width: nat = 352;
-- parameter height: nat = 288;
-- parameter ival: nat = 0;
-- parameter index: nat = 0;

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

actor Diff
  param (width: int, height: int)
  in (input: uchar[height*width], previous: uchar[height*width])
  out (output: uchar[height*width], result: uchar[height*width])
;

actor Delay
  param (width: int, height: int, ival: int)
  in (input: uchar[height*width])
  out (output: uchar[height*width])
;

graph main
  -- param (width: int = 352, height: int = 288, ival: int = 0, index: int = 0)
  param (width: int, height: int, ival: int, index: int)
  in ()
  out () 
fun
  val (yi,u,v) = Read_YUV<width,height>()
  val yo =
    let rec (output,result) = Diff<width,height>(yi, Delay<width,height,ival> output) in
    result
  val _ = Display_YUV<index,width,height>(yo,u,v)
end;

graph top in () out ()
fun
  val _ = main<352,288,0,0>()
end;

-- #pragma code("Read_YUV", "include/yuvRead.h", "readYUV", "initReadYUV")
-- #pragma code("Diff", "include/difference.h", "difference")
-- #pragma code("Display_YUV", "include/yuvDisplay.h", "yuvDisplay", "yuvDisplayInit")


type uchar;

-- parameter width: int = 352;
-- parameter height: int = 288;
-- parameter ival: int = 0;
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

actor Diff
  param (width: int, height: int)
  in (input: uchar[height*width], previous: uchar[height*width])
  out (result: uchar[height*width])
;

actor Delay
  param (width: int, height: int, ival: int)
  in (input: uchar[height*width])
  out (output: uchar[height*width])
;

bcast bc
  param (width: int, height: int)
  in (i: uchar[height*width])
  out (o1: uchar[height*width], o2:uchar[height*width])
;

graph main
  -- param (width: int = 352, height: int = 288, ival: int = 0, index: int = 0)
  param (width: int, height: int, ival: int, index: int)
  in ()
  out () 
fun
  val (y,u,v) = () |> Read_YUV<width,height>
  val (y1,y2) = y |> bc<width,height>
  val yp = y1 |> Delay<width,height,ival>
  val yo = (y2,yp) |> Diff<width,height>
  val () = (yo,u,v) |> Display_YUV<index,width,height>
end;

graph top in () out ()
fun
  val _ = main<352,288,0,0>()
end;

-- #pragma code("Read_YUV", "include/yuvRead.h", "readYUV", "initReadYUV")
-- #pragma code("Diff", "include/difference.h", "difference")
-- #pragma code("Display_YUV", "include/yuvDisplay.h", "yuvDisplay", "yuvDisplayInit")
-- #pragma code("Bc1", "include/broadcast.h", "bcast")


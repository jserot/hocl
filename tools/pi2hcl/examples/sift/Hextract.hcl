-- Generated by pi2hcl from file Hextract.pi

type nat;
type int;
type SiftKpt;
type char;
type unsigned char;

parameter nKeypointsMax: nat = parallelismLevel*nLocalKptMax;
parameter nLocalKptMax: nat = max(1, floor(nKeypointsMaxUser/parallelismLevel));
parameter parallelismLevel: nat = 4;
parameter descrHistBins: nat = 8;
parameter tot_image_size: nat = image_width*image_height;
parameter nHistBins: nat = (descrWidth + 2) * (descrWidth + 2) * (descrHistBins + 2);
parameter nBins: nat = descrWidth*descrWidth*descrHistBins;
parameter descrWidth: nat = 4;
parameter DumpDescriptor: nat = 0;
parameter image_height: nat = 640;
parameter image_width: nat = 800;
parameter nKeypointsMaxUser: nat = 1400;
parameter FilePathLength: nat = 512;

#pragma code("filename1", "Code/include/ezsift-preesm.h", "filename1", "");
#pragma code("read_pgm", "Code/include/ezsift-preesm.h", "read_pgm", "");
#pragma code("SIFT", "Htop_sift.hcl");
#pragma code("export_keypoints_to_key_file", "Code/include/ezsift-preesm.h", "export_keypoints_to_key_file", "");
#pragma code("draw_keypoints_to_ppm_file", "Code/include/ezsift-preesm.h", "draw_keypoints_to_ppm_file", "");

actor filename1
  param (FilePathLength: int)
  in ()
  out (filename: char "FilePathLength");
bcast BdFilename
  param (FilePathLength: nat)
  in (filename: char "FilePathLength")
  out (forRead: char "FilePathLength", forDraw: char "FilePathLength", forExport: char "FilePathLength");
actor read_pgm
  param (FilePathLength: int, image_width: int, image_height: int, tot_image_size: nat)
  in (filename: char "FilePathLength")
  out (img: unsigned char "tot_image_size");
bcast BdOriginalImage
  param (tot_image_size: nat)
  in (originalImage: unsigned char "tot_image_size")
  out (forSift: unsigned char "tot_image_size", forDump: unsigned char "tot_image_size");
actor SIFT
  param (nKeypointsMax: nat, nLocalKptMax: nat, parallelismLevel: nat, image_width: nat, image_height: nat, nBins: nat, nHistBins: nat, descrWidth: nat, descrHistBins: nat, tot_image_size: nat)
  in (image: unsigned char "tot_image_size")
  out (keypoints: SiftKpt "nKeypointsMax", nbKeypoints: int "1");
bcast BdNbKeypoints
  in (nbKpts: int "1")
  out (forDraw: int "1", forDump: int "1");
bcast BdKeypoints
  param (nKeypointsMax: nat)
  in (kpts: SiftKpt "nKeypointsMax")
  out (forDraw: SiftKpt "nKeypointsMax", forDump: SiftKpt "nKeypointsMax");
actor export_keypoints_to_key_file
  param (FilePathLength: int, nKeypointsMax: int, DumpDescriptor: int, nBins: int)
  in (filename: char "FilePathLength", keypoints: SiftKpt "nKeypointsMax", nbKeypoints: int "1")
  out ();
actor draw_keypoints_to_ppm_file
  param (FilePathLength: int, image_height: int, image_width: int, nKeypointsMax: int, tot_image_size: int)
  in (keypoints: struct SiftKeypoint "nKeypointsMax", nbKeypoints: int "1", image: unsigned char "tot_image_size", filename: char "FilePathLength")
  out ();

let filename1_filename = filename1 FilePathLength ();
let (BdFilename_forRead,BdFilename_forDraw,BdFilename_forExport) = BdFilename FilePathLength filename1_filename;
let read_pgm_img = read_pgm (FilePathLength,image_width,image_height,tot_image_size) BdFilename_forRead;
let (BdOriginalImage_forSift,BdOriginalImage_forDump) = BdOriginalImage tot_image_size read_pgm_img;
let (SIFT_keypoints,SIFT_nbKeypoints) = SIFT (nKeypointsMax,nLocalKptMax,parallelismLevel,image_width,image_height,nBins,nHistBins,descrWidth,descrHistBins,tot_image_size) BdOriginalImage_forSift;
let (BdNbKeypoints_forDraw,BdNbKeypoints_forDump) = BdNbKeypoints SIFT_nbKeypoints;
let (BdKeypoints_forDraw,BdKeypoints_forDump) = BdKeypoints nKeypointsMax SIFT_keypoints;
let () = export_keypoints_to_key_file (FilePathLength,nKeypointsMax,DumpDescriptor,nBins) (BdFilename_forExport,BdKeypoints_forDump,BdNbKeypoints_forDump);
let () = draw_keypoints_to_ppm_file (FilePathLength,image_height,image_width,nKeypointsMax,tot_image_size) (BdKeypoints_forDraw,BdNbKeypoints_forDraw,BdOriginalImage_forDump,BdFilename_forDraw);

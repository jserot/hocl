-- Generated by pi2hcl from file Hblur.pi

type nat;
type int;
type float;

parameter tot_image_size: nat;
parameter nGpyrLayers: nat;
parameter parallelismLevel: nat;
parameter gWmax: nat;
parameter image_height: nat;
parameter image_width: nat;

#pragma code("row_filter_transpose_1", "Code/include/ezsift-preesm.h", "row_filter_transpose_1", "");
#pragma code("BarrierTranspose_1", "Code/include/ezsift-preesm.h", "BarrierTranspose_1", "");
#pragma code("row_filter_transpose_2", "Code/include/ezsift-preesm.h", "row_filter_transpose_2", "");
#pragma code("BarrierTranspose_2", "Code/include/ezsift-preesm.h", "BarrierTranspose_2", "");

source col_sizes2 : int;
source gauss_coefs2 : float;
source iter_nb2 : int;
source iter_nb1 : int;
source iterPrev : float;
source col_sizes1 : int;
source imgOri : float;
source gauss_coefs1 : float;
actor row_filter_transpose_1
  param (image_height: int, image_width: int, nGpyrLayers: int, parallelismLevel: int, gWmax: int, tot_image_size: nat)
  in (gaussian_coefs: float "nGpyrLayers*gWmax", img: float "tot_image_size/parallelismLevel", column_sizes: int "nGpyrLayers", imgIterPrev: float "tot_image_size/parallelismLevel", iter: int "1")
  out (imgGT: float "tot_image_size/parallelismLevel");
actor BarrierTranspose_1
  param (image_width: int, image_height: int, parallelismLevel: int, tot_image_size: int)
  in (img_in: float "tot_image_size")
  out (img_out: float "tot_image_size");
actor row_filter_transpose_2
  param (image_height: int, image_width: int, nGpyrLayers: int, parallelismLevel: int, gWmax: int, tot_image_size: nat)
  in (gaussian_coefs: float "nGpyrLayers*gWmax", img: float "tot_image_size/parallelismLevel", column_sizes: int "nGpyrLayers", iter: int "1")
  out (imgGT: float "tot_image_size/parallelismLevel");
actor BarrierTranspose_2
  param (image_width: int, image_height: int, parallelismLevel: int, tot_image_size: int)
  in (img_in: float "tot_image_size")
  out (img_out: float "tot_image_size");
sink imgBlurred : float;

let col_sizes2_col_sizes2 = col_sizes2 ();
let gauss_coefs2_gauss_coefs2 = gauss_coefs2 ();
let iter_nb2_iter_nb2 = iter_nb2 ();
let iter_nb1_iter_nb1 = iter_nb1 ();
let iterPrev_iterPrev = iterPrev ();
let col_sizes1_col_sizes1 = col_sizes1 ();
let imgOri_imgOri = imgOri ();
let gauss_coefs1_gauss_coefs1 = gauss_coefs1 ();
let row_filter_transpose_1_imgGT = row_filter_transpose_1 (image_height,image_width,nGpyrLayers,parallelismLevel,gWmax,tot_image_size) (gauss_coefs1_gauss_coefs1,imgOri_imgOri,col_sizes1_col_sizes1,iterPrev_iterPrev,iter_nb1_iter_nb1);
let BarrierTranspose_1_img_out = BarrierTranspose_1 (image_width,image_height,parallelismLevel,tot_image_size) row_filter_transpose_1_imgGT;
let row_filter_transpose_2_imgGT = row_filter_transpose_2 (image_height,image_width,nGpyrLayers,parallelismLevel,gWmax,tot_image_size) (gauss_coefs2_gauss_coefs2,BarrierTranspose_1_img_out,col_sizes2_col_sizes2,iter_nb2_iter_nb2);
let BarrierTranspose_2_img_out = BarrierTranspose_2 (image_width,image_height,parallelismLevel,tot_image_size) row_filter_transpose_2_imgGT;
let () = imgBlurred BarrierTranspose_2_img_out;

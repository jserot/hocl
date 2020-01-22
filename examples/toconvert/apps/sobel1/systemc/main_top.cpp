// -------------------------------------------------------------------------------
// This file has been automatically generated by the HoCL compiler (version 0.2)
// from file(s) main.hcl, on 2019-11-05 at 15:07:51, by jserot
// -------------------------------------------------------------------------------

#include <systemc.h>
#include <iostream>
#include "bcast.h"
#include "Read_YUV_act.h"
#include "Display_YUV_act.h"
#include "Sobel_act.h"
#include "index_param.h"
#include "height_param.h"
#include "width_param.h"

int sc_main(int argc, char* argv[]) {
  sc_fifo<int > w13("w13", 4);
  sc_fifo<int > w12("w12", 4);
  sc_fifo<int > w1("w1", 4);
  sc_fifo<int > w2("w2", 4);
  sc_fifo<int > w3("w3", 4);
  sc_fifo<int > w4("w4", 4);
  sc_fifo<uchar > w5("w5", 102000);
  sc_fifo<int > w6("w6", 4);
  sc_fifo<int > w7("w7", 4);
  sc_fifo<int > w8("w8", 4);
  sc_fifo<uchar > w11("w11", 102000);
  sc_fifo<uchar > w10("w10", 102000);
  sc_fifo<uchar > w9("w9", 102000);

  sc_clock clk("clk", 10, SC_NS, 0.5);

  bcast3<int > bcast_8("bcast_8");
  bcast_8.i(w13);
  bcast_8.o_1(w1);
  bcast_8.o_2(w3);
  bcast_8.o_3(w7);
  bcast3<int > bcast_7("bcast_7");
  bcast_7.i(w12);
  bcast_7.o_1(w2);
  bcast_7.o_2(w4);
  bcast_7.o_3(w8);
  Read_YUV_act Read_YUV("Read_YUV", false);
  Read_YUV.clk(clk);
  Read_YUV.width(w1);
  Read_YUV.height(w2);
  Read_YUV.y(w5);
  Read_YUV.u(w10);
  Read_YUV.v(w9);
  Sobel_act Sobel("Sobel", false);
  Sobel.clk(clk);
  Sobel.width(w3);
  Sobel.height(w4);
  Sobel.input(w5);
  Sobel.output(w11);
  Display_YUV_act Display_YUV("Display_YUV", false);
  Display_YUV.clk(clk);
  Display_YUV.id(w6);
  Display_YUV.width(w7);
  Display_YUV.height(w8);
  Display_YUV.y(w11);
  Display_YUV.u(w10);
  Display_YUV.v(w9);
  index_param index("index");
  index.clk(clk);
  index.o(w6);
  height_param height("height");
  height.clk(clk);
  height.o(w12);
  width_param width("width");
  width.clk(clk);
  width.o(w13);

  sc_start();
  return EXIT_SUCCESS;
}
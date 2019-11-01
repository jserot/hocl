PROJ=sobel
SRCS=main.hcl
MAIN=main
DOT_OPTS=#-dot_no_io_rates #-dump_static
#PREESM_OPTS=-preesm_top_name top_sobel -target_dir ./preesm
PREESM_OPTS=-target_dir ./preesm
SYSTEMC_OPTS=-target_dir ./systemc -sc_default_fifo_capacity 128000 # Warning ! fifo cap must be big enough to hold one YUV frame or a seq of sliced frames

include ../../../../lib/etc/Makefile.app

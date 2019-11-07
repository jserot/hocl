PROJ=idiff2
SRCS=main.hcl
MAIN=main
DOT_OPTS=-insert_bcasts # -dot_no_io_rates
PREESM_OPTS=-insert_bcasts -target_dir ./preesm
SYSTEMC_OPTS=-target_dir ./systemc -sc_default_fifo_capacity 102000 # Warning ! fifo cap must be big enough to hold one YUV frame

include ../../../../lib/etc/Makefile.app

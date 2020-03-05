SRCS=main.hcl
GEN_OPTS=-prelude ../../../../lib/hocl/prelude.hcl
DOT_OPTS=-target_dir ./dot -dot_slotted_boxes
SYSTEMC_OPTS=-target_dir ./systemc -sc_default_fifo_capacity 102000 # Warning ! fifo cap must be big enough to hold one YUV frame
PREESM_OPTS=-target_dir ./preesm

include ../../../../lib/etc/Makefile.app

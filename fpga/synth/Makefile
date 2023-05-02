SRCS = ../Reduceron/Reduceron.vhd
IMPL = post_route.dcp

all: $(IMPL)

$(IMPL): $(SRCS)
	vivado -mode batch -source synth.tcl

clean:
	rm -rf prj* ip
	rm -f *.rpt *.dcp *.jou *.log hd_visual vivado_*.str

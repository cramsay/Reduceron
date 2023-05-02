create_project prj . -force -part xcu280-fsvh2892-2L-e

# Project properties
set_property board_part xilinx.com:au280:part0:1.1 [current_project]
set_property target_language VHDL [current_project]
source set_clock.tcl

# Add sources
add_files { ../Reduceron/ }
set_property top Reduceron [current_fileset]
set_property file_type {VHDL 2008} [get_files  ../Reduceron/Reduceron.vhd]
update_compile_order -fileset sources_1
add_files -fileset constrs_1 ./constrs.sdc
add_files -fileset utils_1 ./set_clock.tcl
set_property STEPS.SYNTH_DESIGN.TCL.PRE [ get_files ./set_clock.tcl -of [get_fileset utils_1] ] [get_runs synth_1]
set_property STEPS.INIT_DESIGN.TCL.PRE  [ get_files ./set_clock.tcl -of [get_fileset utils_1] ] [get_runs impl_1]


# Source RAM IP scripts
set ramscripts [glob -directory "../Reduceron" -- "*.tcl"]
foreach f $ramscripts {source $f}

# Synthesise
launch_runs impl_1 -jobs 6
wait_on_run impl_1

# Report
open_run impl_1
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose -max_paths 10 -input_pins -routable_nets -name timing_1 -file ./post_route_timing.rpt
report_utilization -file ./post_route_util.rpt
write_checkpoint ./post_route.dcp -force

close_project

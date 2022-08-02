set script_path [ file dirname [ file normalize [ info script ] ] ]
set prj_dir [get_property DIRECTORY [current_project]]
set prj_name [get_property NAME [current_project]]
set xci_file $prj_dir/$prj_name.srcs/sources_1/ip/ram_c1750/ram_c1750.xci
create_ip -name blk_mem_gen -vendor xilinx.com -library ip -version 8.4 -module_name "ram_c1750"
set_property -dict [list \
    CONFIG.Memory_Type {Single_Port_RAM}\
    CONFIG.Algorithm {Fixed_Primitives}\
    CONFIG.Primitive {1kx18}\
    CONFIG.Write_Width_A {18}\
    CONFIG.Write_Depth_A {128}\
    CONFIG.Read_Width_A {18}\
    CONFIG.Write_Width_B {18}\
    CONFIG.Read_Width_B {18}\
    CONFIG.Load_Init_File {false}\
    CONFIG.Coe_File "$script_path/no_coe_file_loaded"\
    CONFIG.PRIM_type_to_Implement {BRAM}\
    CONFIG.Enable_32bit_Address {false}\
    CONFIG.Use_Byte_Write_Enable {false}\
    CONFIG.Byte_Size {9}\
    CONFIG.Assume_Synchronous_Clk {true}\
    CONFIG.Operating_Mode_A {NO_CHANGE}\
    CONFIG.Enable_A {Always_Enabled}\
    CONFIG.Operating_Mode_B {NO_CHANGE}\
    CONFIG.Enable_B {Always_Enabled}\
    CONFIG.Register_PortA_Output_of_Memory_Primitives {false}\
    CONFIG.Register_PortB_Output_of_Memory_Primitives {false}\
    CONFIG.Use_RSTA_Pin {false}\
    CONFIG.Use_RSTB_Pin {false}\
    CONFIG.Port_A_Write_Rate {100}\
    CONFIG.Port_B_Clock {100}\
    CONFIG.Port_B_Write_Rate {100}\
    CONFIG.Port_B_Enable_Rate {100}\
    CONFIG.use_bram_block {Stand_Alone}\
    CONFIG.EN_SAFETY_CKT {false}] [get_ips ram_c1750]
generate_target all [get_ips ram_c1750]
update_compile_order -fileset sources_1

module crun_directory
    implicit none
    integer, parameter :: VOA_PATH_LEN = 128
    character(len=VOA_PATH_LEN) :: root_directory
    character(len=VOA_PATH_LEN) :: run_directory
    character(len=VOA_PATH_LEN) :: area_directory
    character(len=VOA_PATH_LEN) :: area_inv_directory
end module crun_directory

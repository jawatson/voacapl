      module voacapl_defs
#ifdef _WIN32
        character(len=1), parameter :: PATH_SEPARATOR ='\'
        integer, parameter :: MAX_AREA_MONTHS = 9
#else
        character(len=1), parameter :: PATH_SEPARATOR ='/'
        integer, parameter :: MAX_AREA_MONTHS = 25
#endif
        character(len=*), parameter :: VOACAPL_VERSION = '0.7'
      end module voacapl_defs

      module voacapl_defs
#ifdef _WIN32
        character(len=1), parameter :: PATH_SEPARATOR ='\'
        integer, parameter :: MAX_AREA_MONTHS = 9
#else
        character(len=1), parameter :: PATH_SEPARATOR ='/'
        integer, parameter :: MAX_AREA_MONTHS = 25
#endif

#if defined(VERSION)
        character(len=*), parameter :: VOACAPL_VERSION = VERSION
#else
        character(len=*), parameter :: VOACAPL_VERSION = "Test"
#endif
      end module voacapl_defs

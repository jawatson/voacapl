module Cant99
    implicit none
    integer :: luaa, nfreq, itype
    integer :: ifreq1, ifreq2
    real, dimension(20) :: parms
    real, dimension(100) :: frequency,dbi,eff
    real, dimension(91, 360) :: gain1, gain2
    character(len=80) :: filenam,title
end module


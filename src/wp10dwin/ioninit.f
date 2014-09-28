      subroutine ioninit(index,parm,asig,aeps,and,anl,anh,aex)
c            initialize IONCAP antennas
      dimension parm(20),aex(4)

      asig=parm(4)                         !  conductivity
      aeps=parm(3)                         !  dielectric
      do 111 i=1,4
111   aex(i)=0.
      go to (1,2,3,3,5,6,7,8,9,10),index
c                                          !  Terminated Horizontal Rhombic
1     and=parm(6)              !  tilt angle
      anl=parm(7)              !  leg length
      anh=parm(8)              !  height
      return
c                                          !  Vertical Monopole
2     anl=parm(6)              !  height
      anh=parm(7)              !  gain above dipole
      and=0.
      return
c                                          !  Horizontal Dipole & Yagi
3     anl=parm(6)              !  antenna length
      anh=parm(7)              !  antenna height
      and=0.
      aex(1)=parm(8)           !  gain above 1/2 wavelength horizontal dipole
      return
c                                          !  Vertical Lop-Periodic
5     anl=-.25                 !  antenna height (MUST be 1/4 wavelength)
      and=0.
      anh=0.
      aex(1)=parm(6)           !  gain above 1/4 wavelength vertical
      return
c                                          !  Curtain
6     and=parm(6)              !  # of bays
      anl=parm(7)              !  antenna element length
      anh=parm(8)              !  antenna height to 1st element
      aex(1)=parm(9)           !  # of elements per bay
      aex(2)=parm(10)          !  distance between element centers
      aex(3)=parm(11)          !  vertical spacing of elements
      aex(4)=parm(12)          !  distance from screen
      return
c                                          !  Sloping Vee
7     and=parm(6)              !  1/2 apex angle in plane of wires
      anl=parm(7)              !  antenna leg length
      anh=parm(8)              !  antenna height
      aex(1)=parm(9)           !  terminated height
      return
c                                          !  Inverted L
8     anl=parm(6)              !  antenna length
      anh=parm(7)              !  antenna height
      and=0.
      return
c                                          !  Sloping Rhombic
9     and=parm(6)              !  1/2 large interior angle in plane of wires
      anl=parm(7)              !  antenna leg length
      anh=parm(8)              !  antenna height
      aex(1)=parm(9)           !  terminated height
      return
c                                          !  Interlaced Rhombic
10    and=parm(6)              !  1/2 large interior angle in plane of wires
      anl=parm(7)              !  antenna leg length
      anh=parm(8)              !  lower antenna height
      aex(1)=parm(9)           !  vertical displacement
      aex(2)=parm(10)          !  horizontal feed point displacement
      return
      end
c------------------------------------------------------------------

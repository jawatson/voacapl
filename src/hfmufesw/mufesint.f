      subroutine mufesint(index,parm,asig,aeps,and,anl,anh,aex)
c            initialize HFMUFES antennas
      dimension parm(20),aex(4)

      asig=parm(4)                         !  conductivity
      aeps=parm(3)                         !  dielectric
      do 111 i=1,4
111   aex(i)=0.
      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),index
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
c                                          !  Horizontal Half-Wave Dipole
3     anl=-.5                  !  antenna length(Must be -.5)
      anh=parm(7)              !  antenna height
      and=parm(8)              !  gain above 1/2 wavelength horizontal dipole
      return
c                                          !  Horizontal Yagi
4     anh=parm(6)              !  antenna height
      anl=parm(7)              !  length of driven element
      and=parm(8)              !  length of reflector element
      aex(1)=parm(9)           !  director length
      aex(2)=parm(12)          !  number of elements
      aex(3)=parm(10)          !  director spacing
      aex(4)=parm(11)          !  reflector spacing
      return
c                                          !  Vertical Dipole
5     anl=parm(6)              !  antenna length
      anh=parm(7)              !  feed    height
      and=parm(8)              !  additional gain
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
c                                          !  Terminated Sloping Vee
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
c                                          !  Terminated Sloping Rhombic
9     and=parm(6)              !  1/2 large interior angle in plane of wires
      anl=parm(7)              !  antenna leg length
      anh=parm(8)              !  antenna height
      aex(1)=parm(9)           !  terminated height
      return
c                                          !  Not used
10    return
c                                          !  Sloping Long Wire
11    and=parm(6)              !  Slope from Horizontal
      anl=parm(7)              !  Wire length
      anh=parm(8)              !  Feed height
      return
c                                          !  Not used (constant gain)
12    return
c                                          !  Horizontal Log Periodic
13    anh=parm(6)              !  Feed height
      anl=parm(7)              !  rear element length
      and=parm(8)              !  array slope mesaured from vertical
      aex(1)=parm(9)           !  unloaded transmission line impedance (Ohm)
      aex(2)=parm(10)          !  angle between array axis & element tips
      aex(3)=parm(11)          !  geometric ratio of element length
      aex(4)=parm(12)          !  number of elements
      return
c                                          !  Arbitrary Tilted Dipole
14    and=parm(6)              !  Tilt of element from horizontal
      anl=parm(7)              !  length
      anh=parm(8)              !  Feed height
      return
c                                          !  Side-Loaded Vertical Half Rhombic
15    and=parm(6)              !  angle between leg and ground
      anl=parm(7)              !  leg length
      anh=0.
      return
c                                          !  Sloping Double Rhomboid
16    anh=parm(6)              !  Feed height
      anl=parm(7)              !  Short leg length
      and=parm(8)              !  Angle bet antenna axis & major rhomboid axis
      aex(1)=parm(9)           !  Angle bet major rhomboid axis & short leg
      aex(2)=parm(10)          !  Angle bet major rhomboid axis & long  leg
      aex(3)=parm(11)          !  Long leg length
      aex(4)=parm(12)          !  Termination height
      return
c                                          !  Vertical w/ Radial Conductor
17    anl=parm(6)              !  antenna length
      and=parm(7)              !  length of radials
      anh=0.
      aex(1)=parm(8)           !  radius of radials
      aex(2)=parm(9)           !  number of radials
      return
      end
c------------------------------------------------------------------

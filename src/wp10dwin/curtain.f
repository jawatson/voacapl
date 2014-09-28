C--------------------------------
      SUBROUTINE Curtain(parm,AZIMD,ELEVD,XMGN,GNORM)
c          AZIMD= Degrees from Boresight
c          ELEVD= Elevation angle Degrees
c***************************************************************
c          parm( 1)= gain above isotrope (not used)
c              ( 2)= antenna type [should be 12]
c              ( 3)= Dielectric
c              ( 4)= Conductivity
c              ( 5)= Operating freq
c              ( 6)= # of bays
c              ( 7)= # elements per bay (# of stacks)
c              ( 8)= Design Frequency
c              ( 9)= Antenna element length (>0=meters  <0=wavelengths)
c              (10)= Height above ground (>0  <0)
c              (11)= Horiz Dist between dipole centers (>0  <0)
c              (12)= Vertical displacement (>0  <0)
c              (13)= Distance from Screen (>0  <0)
c              (14)= Vertical Excitation (mode number)
c              (15)= Horizontal Slew Angle
c***************************************************************
      dimension parm(15)
      COMMON/ANTDAT/nostak,STKSPM,NUMBAY,BAYSPM,DIPLNM,RRSPM,STKHTM,
     +STKRAT(8),bayphs(14),DFMHZ,OFMHZ
C*******************************************************
      CHARACTER ICHR*1,IVMA(15)*4
      INTEGER*4 IPHASE(14,8)
      DATA IPHASE/0,  0,  0,  0, 0,  0,  0,  0,  0, 0,  0,  0,  0,  0,
     2            0,  0, 31, 31, 0, 77, 77,109,109, 0,155,155,186,186,
     3            0, 47, 56,103, 0,139,185,195,242, 0,278,324,334,381,
     4            0, 47, 81,128, 0,200,246,281,327, 0,399,446,480,527,
     5            0, 47,105,152, 0,260,306,365,411, 0,519,566,624,671,
     6            0, 47,129,176, 0,318,365,447,494, 0,636,683,765,812,
     7            0, 90,152,242, 0,375,465,527,617, 0,750,840,903,993,
     8            0, 90,180,270, 0,444,534,624,714, 0,888,978,1068,1158/
      data IVMA/'+000','0+00','00+0','++00','+0+0','0++0','+-00',
     +          '+0-0','0+-0','+++0','++-0','+-+0','+--0','++++','++--'/
c......
      data VofL/299.79246/     !  speed of light
      DATA PI/3.1415926/
      DATA PIO2/1.570796326/
      DATA D2R/.01745329251/

      DFMHZ=parm(8)            !  design frequency
      wave=VofL/DFMHZ          !  wavelength in meters
      nostak=nint(parm(7))
      STKSPM=parm(12)
      if(STKSPM.lt.0.) STKSPM=-STKSPM*wave    !  convert to meters
      NUMBAY=nint(parm(6))
      BAYSPM=parm(11)
      if(BAYSPM.lt.0.) BAYSPM=-BAYSPM*wave    !  convert to meters
      DIPLNM=parm(9)
      if(DIPLNM.lt.0.) diplnm=-diplnm*wave    !  convert to meters
      RRSPM=parm(13)
      if(RRSPM.lt.0.) RRSPM=-RRSPM*wave       !  convert to meters
      STKHTM=parm(10)
      if(STKHTM.lt.0.) STKHTM=-STKHTM*wave    !  convert to meters
      mode=nint(parm(14))      !  vertical excitation mode
      DO 100 I=1,4
      ICHR=IVMA(mode)(I:I)
      K=2*I-1
      IF(ICHR.EQ.'+')THEN
        STKRAT(K)=1.
        STKRAT(K+1)=1.
      ELSE IF(ICHR.EQ.'-')THEN
        STKRAT(K)=-1.
        STKRAT(K+1)=-1.
      ELSE
        STKRAT(K)=0.
        STKRAT(K+1)=0.
      ENDIF
  100 CONTINUE
      IF(nostak.LT.8)THEN
        DO 101 I=nostak+1,8
  101   STKRAT(I)=0.
      ENDIF
      ISLEW=nint(parm(15))      !  Horizontal Slew angle
      KSLEW = (ABS(ISLEW)/4)+1
      DO 200 I=1,14
      IF(ISLEW.LT.0)THEN
        bayphs(I) = IPHASE(I,KSLEW)
      ELSE
        bayphs(I) = -IPHASE(I,KSLEW)
      ENDIF
  200 CONTINUE
      OFMHZ=parm(5)        !  operating freq
      IAZ=0
      AZIM=AZIMD*D2R
      IF(ABS(AZIM).GT.PIO2 .and. ABS(AZIM).lt.3.*PIO2) THEN
        AZIM=AZIM-SIGN(PI,AZIM)
        IAZ=1
      ENDIF

      CALL PATTRN0(AZIM,ELEVD*D2R,XMGN,GNORM)
      IF(IAZ.EQ.1) XMGN=XMGN-20.     !  backward radiation
      RETURN
      END
C------------------------

      subroutine getantyp(iant,antyp)   !  get antenna type generic description
      character antyp*48,antyps(0:48)*48
c============================================================================
      data (antyps(I),i=0,10)/
     +            'ITS addition   ISOTROPE',
     1            'ITU-R Rec705   MULTIBAND APER.REF. ARRAY',
     2            'ITU-R Rec705   DUAL-BAND CNT.FED TUN.REF. AR.',
     3            'ITU-R Rec705   DUAL-BAND END.FED TUN.REF. AR.',
     4            'ITU-R Rec705   DUAL-BAND CNT.FED TROPIC ARR.',
     5            'ITU-R Rec705   HORIZONTAL LOG-PERIODIC ARRAY',
     6            'ITU-R Rec705   VERTICAL LOG-PERIODIC ARRAY',
     7            'ITU-R Rec705   HORIZONTAL RHOMBIC',
     8            'ITU-R Rec705   QUADRANT ANTENNA',
     9            'ITU-R Rec705   CROSSED-DIPOLE ANTENNA',
     +            'ITU-R Rec705   VERTICAL MONOPOLE'/
      data (antyps(I),i=11,20)/
     1            'ITS addition   GAIN TABLE vs ELEVATION ANGLE',
     2            'ITS addition   CURTAIN ARRAY NTIA Rep 87-215',
     3            'ITS addition   GAIN TABLE vs AZIMUTH & ELEVATION',
     4            'ITS addition   Point-to-Point GAIN TABLE',
     +          6*'Unknown antenna type'/
      data (antyps(I),i=21,30)/
     1            'IONCAP ITSA-1  Terminated Horizontal Rhombic',
     2            'IONCAP ITSA-1  Vertical Monopole',
     3            'IONCAP ITSA-1  Horizontal Dipole',
     4            'IONCAP ITSA-1  Horizontal Yagi',
     5            'IONCAP ITSA-1  Vertical Log Periodic',
     6            'IONCAP ITSA-1  Curtain',
     7            'IONCAP ITSA-1  Sloping Vee',
     8            'IONCAP ITSA-1  Inverted L',
     9            'IONCAP ITSA-1  Sloping Rhombic',
     +            'IONCAP ITSA-1  Interlaced Rhombic'/
      data (antyps(I),i=31,40)/
     1            'HFMUFES ITS-78 Terminated Horizontal Rhombic',
     2            'HFMUFES ITS-78 Vertical Monopole',
     3            'HFMUFES ITS-78 Horizontal Half-Wave Dipole',
     4            'HFMUFES ITS-78 Horizontal Yagi',
     5            'HFMUFES ITS-78 Vertical Dipole',
     6            'HFMUFES ITS-78 Curtain',
     7            'HFMUFES ITS-78 Terminated Sloping Vee',
     8            'HFMUFES ITS-78 Inverted L',
     9            'HFMUFES ITS-78 Terminated Sloping Rhombic',
     +            'HFMUFES ITS-78 #10 - Not Used'/
      data (antyps(I),i=41,48)/
     1            'HFMUFES ITS-78 Sloping Long Wire',
     2            'HFMUFES ITS-78 #12 - Not Used',
     3            'HFMUFES ITS-78 Horiz Xed-Dipole Log-Periodic',
     4            'HFMUFES ITS-78 Arbitrary Tilted Dipole',
     5            'HFMUFES ITS-78 Side-Loaded Vert Half-Rhombic',
     6            'HFMUFES ITS-78 Sloping Double Rhomboid',
     7            'HFMUFES ITS-78 Vertical Monopole w/Ground Scrn',
     8            'NOSC-95        Inverted Cone Antenna'/
c=========================================================================
      if(iant.le.48) then
         antyp=antyps(iant)
      else if(iant.ge.90 .and. iant.le.99) then
         write(antyp,1) iant
1        format('External Antenna Type #',i2.2)
      else
         antyp='Unknown antenna type'
      end if
      return
      end

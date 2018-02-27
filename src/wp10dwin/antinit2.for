      subroutine antinit2
c*******************************************************************
c       PURPOSE:
c          initialize CCIR antenna calculations in propagation programs
c       INPUT  PARAMETERS:
c          in COMMON /CCIRPARM/
c       OUTPUT PARAMETERS:
c*******************************************************************
c       Written by Greg Hand NTIA/ITS Boulder, Colorado   Aug 1991
c       Please call (303)497-3375 with any questions.
c*******************************************************************
c       Calls following routines:
c          trigfun
c          logparm
c          parmprec
c          gainorm
c*******************************************************************
      common /ccirparm/ parm(20),z6,umax,vmax,giso,
     +                  modegain,gainmax(3,2)
c                                  antenna floor value  [30dBi]
      common /floorc/ floor
      common/general/d1,e1,f1,p1,q1,s1,beta,rlambda
      common/cros/cpfr,cp11,fr1,h1,pfr,p11,p11s,q2,r2,r3,sl
      common/surf/ro,costh,sinth,rnumb,gamma,epq
      common/ccparm/uust,nat,ah1,bh1
      common/trig/a(0:361),b(0:361)
c
      save ionce
      data ionce/0/
c
      iant=nint(parm(2))                    !  CCIR antenna type [0,10]
      call setmaxgain(parm(5))              !  parm(5)=operating freq
      if(ionce.eq.0) then
         ionce=1
c                                      antenna floor value [30dbi]
         floor=30.
         p1 = 3.1415927
         q1 = p1 / 180
         uust = 1.0
         vvst = 1.0
         call trigfun(a, b, uust)
      end if
c=========================================================================
      if(iant.eq.0) go to 10
      if(iant.eq.11) return       !  gain table at elevation angles
      if(iant.eq.12) return       !  NTIA Report 87-215 Curtain Arrays
      e1 = parm(3)
      s1 = parm(4)
      f1 = parm(5)
      d1 = 18000 * s1 / f1
      rlambda = 299.8 / f1
      beta = 2 * p1 / rlambda
c=========================================================================
      if(iant.lt.0 .or. iant.gt.10) then
         write(*,'('' iant='',i5)') iant
         stop 'In antinit2: iant out of range'
      end if
      go to (10,100,200,200,400,500,500,700,800,800,1000),iant+1
c*****************************************************************
c          iant = 0
c          Isotrope
c*****************************************************************
10    umax=0
      vmax=0
      z6=1.
      ifs=0
      go to 2010
c*****************************************************************
c          iant = 1
c          Multi-band Center-Fed Half-Wave Dipole Array
c*****************************************************************
100     r2 = parm(6)
        r3 = parm(7)
        fd = parm(8)
        h1 = parm(9)
        ifs = parm(10)
        sd = parm(11)
        ws = parm(12)
        wd = parm(13)
        umax = parm(14)
        vmax = parm(15)
        z6 = parm(17)
        fr = f1 / fd
        pfr = p1 * fr
        p11 = pfr / 2.0
        p11s = 8 * p11 * sd
        cpfr = cos(pfr)
        cp11 = cos(p11)
        sl = sin(ifs * q1)
        fdx = ws / (wd * p1 * .001)
        fdx1 = alog(fdx)
        fdx2 = .048 * ws
        fr1 = fdx1 * fdx2 * fr
        go to 2000
c****************************************************************
c          iant = 2
c          Dual-Band Center-Fed Half-Wave Dipole Array
c       &  iant = 3
c          Dual-Band End-Fed Half-Wave Dipole Array
c****************************************************************
200     r2 = parm(6)
        r3 = parm(7)
        fd = parm(8)
        h1 = parm(9)
        ifs = parm(10)
        q2 = parm(11)
        umax = parm(12)
        vmax = parm(13)
        fr = f1 / fd
        pfr = p1 * fr
        p11 = pfr / 2.0
        cpfr = cos(pfr)
        cp11 = cos(p11)
        sl = sin(ifs * q1)
        go to 2000
c****************************************************************
c          iant = 4
c          Tropical Array
c****************************************************************
400     r2 = parm(6)
        r3 = parm(7)
        fd = parm(8)
        h1 = parm(9)
        ifs = parm(10)
        umax = parm(11)
        vmax = parm(12)
        fr = f1 / fd
        pfr = p1 * fr
        p11 = pfr / 2.0
        cpfr = cos(pfr)
        cp11 = cos(p11)
        sl = sin(ifs * q1)
        go to 2000
c*****************************************************************
c          iant = 5
c          Horizontal Log-Periodic Array
c       &  iant = 6
c          Vertical Log-Periodic Array
c*****************************************************************
500     nel = parm(6)
        rl1 = parm(7) / 2.0
        rlnel = parm(8) / 2.0        
        dc = parm(9)
        h1 = parm(10)
        hnel = parm(11)
        z0 = parm(12)
c          iant=6=vertical
        call logparm(iant,nel, rlnel, rl1, hnel, h1, dc, z0)
        ifs = 0
        go to 2000
c******************************************************************
c          iant = 7
c          Horizontal Rhombic
c******************************************************************
700     sl = parm(6) * beta
        h1 = parm(7) * beta * 2
        gamma = parm(8) * q1
        sinth = sin(gamma)
        costh = cos(gamma)
        ifs = 0
        go to 2000
c*******************************************************************
c          iant = 8
c          Quadrant Antenna
c       &  iant = 9
c          Crossed-Dipole Antenna
c*******************************************************************
800     fd = parm(6)
        h1 = parm(7)
        umax = parm(11)
        vmax = parm(12)
        fr = f1 / fd
        pfr = p1 * fr
        p11 = pfr / 2.0
        cp11 = cos(p11)
c          modified per 18/8/92 : ifs=0 : added
        ifs=0
        go to 2000
c***************************************************************
c          iant = 10
c          Vertical Monopole
c***************************************************************
1000    h1 = parm(6) * beta
        ro = parm(7) * beta
        if (ro.gt.0) then
          diam = parm(8)
          rnumb = parm(9)
        else
          diam = 0
          rnumb = 0
        endif
        ah1 = sin(h1)
        bh1 = cos(h1)
        gamma = beta * rnumb * diam * .001
        phi = atan(d1 / e1) - .5 * atan(d1 / (e1 - 1))
        epsr = (((e1 - 1)**2 + d1**2)**.25) / sqrt(e1**2 + d1**2)
        epq = epsr**2
        sinth = epsr * sin(phi)
        costh = epsr * cos(phi)
        ifs = 0
c***************************************************************
2000    iperf = 0
        if(iant.eq.10) return      !  vertical monopole is a table
        call parmprec(iant, iperf)
2010    call gainorm( z6, umax, vmax, iperf,iant,ifs)
c***************************************************************
      return
      end
c----------------------------------------------------------------------

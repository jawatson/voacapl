c###mpath.for
      SUBROUTINE MPATH(IF)
C-------------------------------
C
C     THIS ROUTINE PERFORMS THE MULTIPATH PROBABILITY AND DETERMINES
C     IF ANY OTHER MODE WILL INTERFERE WITH THE MOST RELIABLE MODE
c**********************************************************************
c          As changed 26 March 2009
c          This return the reliability of the next most probable mode
c          that is within the Maximum tolerable time delay (DMP)
c          and whose signal power is greater than PMP below the
c          signal power of the MRM.
c**********************************************************************
C
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON / SON / ANGLE(13), ANGLER(13), CPROB(13), DBLOS(13),
     A DBLOSL(13), DBLOSU(13), DBU(13), DELAY(13), DBW(13), NHP(13),
     B XNYNOIS(13), PROBMP(13), RELIAB(13), SNDB(13), SNPR(13),
     C SNRLW(13), SNRUP(13), SPROB(13), VHIGH(13), RNEFF(13),MDL(13)
       CHARACTER MDL*1
      COMMON / allMODE /ABPS(20),CREL(20),FLDST(20),HN(20),HP(20),
     1PROB(20),RELY(20),RGAIN(20),SIGPOW(20),SN(20),
     2SPRO(20),TGAIN(20),TIMED(20),TLOSS(20),B(20),FSLOS(20),
     3grlos(20),adv(20),obf(20),
     CNMODE(20),TLLOW(20),TLHGH(20),EFF(20),NREL,NMMOD
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16,
     1                 LU20, LU25, LU26, LU35
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100

      PROBMP(IF) = 0.001
      IF(DMP.le.0. .or. PMP.le.0.) return        !  no multipath requested
c          Is this METHOD=30? If so, not valid>7000km
      if(method.eq.20 .and. mspec.eq.121 .and. gcdkm.gt.7000.) return
      sig_power=sigpow(nrel)         !  signal power of Most-Reliable-Mode
      sig_power_limit=sig_power-PMP  !  ignore signals below this level
      TTIM = timed(nrel)             !  time delay of MRM
ccc      write(luo,4) method,mspec,gcdkm
ccc4     format('method, mspec=',2i5,f10.2)
ccc      write(luo,1) nrel,sig_power,sig_power_limit,ttim,pmp,dmp
ccc1     format('mpath=',i3,5f10.3)

      DO 135 IM = 1,NMMOD   ! DO LOOP up to max. number of modes.
      IF(IM.EQ.NREL) GO TO 135  ! Don't check mode for MRM -- found already by RELBIL.FOR.
      IF(HP(IM).LE.0.) GO TO 135  ! This mode does not exist.
ccc      write(luo,2) im,timed(im),sigpow(im)
ccc2     format(i4,2f10.3)
      IF(ABS(TIMED(IM)-TTIM).LE.DMP) GO TO 135   !  inside acceptable time delay

C        Check on Multipath Power criterion set by operator.
C        The logic of this next part is revised from original code.
c        Ignore signal powers below this limit
      if(sigpow(im).lt.sig_power_limit) go to 135

      PROBMP(IF)=AMAX1(PROBMP(IF),RELY(IM))
ccc      write(luo,3) probmp(if),rely(im)
ccc3     format(20x,2f10.3)
135   CONTINUE
      RETURN
      END
C--------------------------------

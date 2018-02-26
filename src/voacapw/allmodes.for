c###allmodes.for
      SUBROUTINE ALLMODES(iflg,fval)
C--------------------------------
C
C     THIS ROUTINE outputs THE binary values for the allmodes method
c
      COMMON/ANOIS/ATNU,ATNY,CC,TM,RCNSE,DU,DL,SIGM,SYGU,SYGL,KJ,JK
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON / ZON / ABPS(7), CREL(7), EFF(7), FLDST(7), GRLOS(7),
     1 HN(7), HP(7), PROB(7), RELY(7), RGAIN(7), SIGPOW(7), SN(7),
     2 SPRO(7), TGAIN(7), TIMED(7), TLOSS(7), B(7), FSLOS(7), ADV(7),
     3 OBF(7),NMODE(7),TLLOW(7),TLHGH(7)
      COMMON / allMODE /zABPS(20),zCREL(20),zFLDST(20),zHN(20),zHP(20),
     1zPROB(20),zRELY(20),zRGAIN(20),zSIGPOW(20),zSN(20),
     2zSPRO(20),zTGAIN(20),zTIMED(20),zTLOSS(20),zB(20),zFSLOS(20),
     3zgrlos(20),zadv(20),zobf(20),
     CizNMODE(20),zTLLOW(20),zTLHGH(20),zEFF(20),NREL,NMMOD
C125PC      COMMON/SPALMOUT/RNZ(11,24,4,2),DUZ(11,24,4,2),DLZ(11,24,4,2),
C125PC     -ZTL(20,11,24,4,2),BBZ(20,11,24,4,2), ZLO(20,11,24,4,2),
C125PC     -ZHG(20,11,24,4,2)
C125PC      COMMON/INDICEZ/ISP,ISZ,IFQN,IUR,NMS(11,24,4,2)
C125PC      COMMON/LPALMOT2/zACAV(24,4,2),zASM(24,4,2),zDSL(24,4,2),
C125PC     +zDSU(24,4,2),zYMUF(4,24,4,2),zYFOT(4,24,4,2),zYHPF(4,24,4,2),
C125PC     +zFVMUF(4,24,4,2),zSIGL(4,24,4,2),zSIGU(4,24,4,2)
C125PC      common/lpalmcmp/imddl(45,2,11,24,4,2),igdfx(45,2,11,24,4,2),
C125PC     +itlos(45,2,11,24,4,2),ihpfx(45,2,11,24,4,2),lNANG(2,11,24,4,2)
C125PC      integer*2 imddl,igdfx,itlos,ihpfx,lNANG
c--------------------------------------------------------------
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON / GEOG / GYZ(5), RAT(5), GMDIP(5), CLCK(5), ABIY(5),
     1 ARTIC(5), SIGPAT(5), EPSPAT(5)
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
C125PC   character*13 FILNAM
C125PC   data FILNAM/'xmtrddddd.alm'/
C------------------------------------------------------------------------
C125PC      if(iflg.eq.200.AND.MSPEC.GT.120)GO TO 100
      if(iflg.eq.0)then
        NMMOD=0
        do 2 i=1,20
        zTLOSS(i)=99999.
        ztllow(i)=999.
        ztlhgh(i)=999.
        ZHP(I)=-1.
C125PC        IF(MSPEC.EQ.125)THEN
C125PC          BBZ(I,IFQN,IUR,ISZ,ISP)=99.
C125PC          ZTL(I,IFQN,IUR,ISZ,ISP)=99999.
C125PC          ZLO(I,IFQN,IUR,ISZ,ISP)=999.
C125PC          ZHG(I,IFQN,IUR,ISZ,ISP)=999.
C125PC        ENDIF
    2   continue
        freq=fval
        RETURN
C125PC      else if(iflg.eq.1.AND.MSPEC.NE.125)then
      else if(iflg.eq.1)then
        ist=1
        hp(2)=hp(1)
        zTLOSS(2)=32000.
        lst=1
        iznmode(NMMOD+2)=nmode(2)
        zb(NMMOD+2)=b(2)
      else if(iflg.eq.2)then
        ist=4
        lst=5
      else
        ist=1
        lst=6
      ENDIF
C125PC      IF(NMMOD.EQ.0.AND.MSPEC.EQ.125)THEN
C125PC        RNZ(IFQN,IUR,ISZ,ISP)=RCNSE
C125PC        DUZ(IFQN,IUR,ISZ,ISP)=DU
C125PC        DLZ(IFQN,IUR,ISZ,ISP)=DL
C125PC      ENDIF
      do 10 i=ist,lst
      if(hp(i).gt.0.)then
        NMMOD=NMMOD+1
C125PC        if(MSPEC.eq.125)then
C125PC          ihn=1000.*hn(i)
C125PC          ilay=100.*nmode(i)
C125PC          BBZ(NMMOD,IFQN,IUR,ISZ,ISP)=b(i)+ihn+ilay
C125PC          ZTL(NMMOD,IFQN,IUR,ISZ,ISP)=TLOSS(i)
C125PC          ZLO(NMMOD,IFQN,IUR,ISZ,ISP)=TLLOW(i)
C125PC          ZHG(NMMOD,IFQN,IUR,ISZ,ISP)=TLHGH(i)
C125PC        else
          zTLOSS(NMMOD)=TLOSS(i)
          ztllow(NMMOD)=tllow(i)
          ztlhgh(NMMOD)=tlhgh(i)
          zhp(NMMOD)=hp(i)
          zcrel(NMMOD)=crel(i)
          zrely(NMMOD)=rely(i)
          zhn(NMMOD)=hn(i)
          iznmode(NMMOD)=nmode(i)
          zsn(NMMOD)=sn(i)
          zfldst(NMMOD)=fldst(i)
          zsigpow(NMMOD)=sigpow(i)
          zb(NMMOD)=b(i)
          ztimed(NMMOD)=timed(i)
          zabps(NMMOD)=abps(i)
          zprob(NMMOD)=prob(i)
          zrgain(NMMOD)=rgain(i)
          ztgain(NMMOD)=tgain(i)
          zfslos(NMMOD)=fslos(i)
          zspro(NMMOD)=spro(i)
          zEFF(NMMOD)=EFF(i)
          zgrlos(NMMOD)=grlos(i)
          zadv(NMMOD)=adv(i)
          zobf(NMMOD)=obf(i)
C125PC        ENDIF
      ENDIF
   10 continue
C125PC      IF(MSPEC.EQ.125)THEN
C125PC        NMS(IFQN,IUR,ISZ,ISP)=NMMOD
C125PC        if(NMMOD.eq.0)NMS(IFQN,IUR,ISZ,ISP)=NMMOD+1
C125PC      ENDIF
C125PC  100 if(fval.EQ.999..AND.MSPEC.EQ.125)then
C125PC        IF(IFQN.EQ.11.AND.IUR.EQ.24.AND.ISZ.EQ.4.AND.ISP.EQ.2)THEN
C125SPLP          IF(GCDKM.GE.7000.)THEN
C125SPLP            NYEAR(1:1)='L'
C125SPLP          ELSE
C125PC            NYEAR(1:1)='A'
C125SPLP          ENDIF
C125PC          FILNAM(1:9)=itran(1)(1:9)
C125PC          open(M100,file=FILNAM,status='new',form='unformatted')
C125PC          WRITE(M100)tlatd,itlat,tLONGd,itLONG,RLATD,IRLAT,
C125PC     +    RLONGD,irLONG,btrd,gcdkm,rsn,lufp,nyear
C125PC          WRITE(M100)(FREL(if),if=1,11)
C125PC          WRITE(M100)((((NMS(if,it,IM,IS),(ztl(ih,if,it,IM,IS),
C125PC     -    BBZ(ih,if,it,IM,IS),zlo(ih,if,it,IM,IS),zhg(ih,if,it,IM,IS),
C125PC     -    ih=1,NMS(if,it,IM,IS)),RNZ(if,it,IM,IS),DUZ(if,it,IM,IS),
C125PC     -    DLZ(if,it,IM,IS),if=1,11),it=1,24),IM=1,4),IS=1,2)
C125SPLP          IF(GCDKM.GE.7000.)THEN
C125SPLP            WRITE(M100)(HI(1,ILY),FI(1,ILY),ILY=1,3),GYZ(3),AMIND,
C125SPLP     +      km,(SIGPAT(icp),EPSPAT(icp),icp=1,km)
C125SPLP            WRITE(m100)(((((LNANG(JJ,IF,IT,IM,IS),
C125SPLP     +      (imddl(ia,JJ,if,it,im,is),igdfx(ia,JJ,if,it,im,is),
C125SPLP     +      itlos(ia,JJ,if,it,im,is),ihpfx(ia,JJ,if,it,im,is),
C125SPLP     +      IA=1,LNANG(JJ,IF,IT,IM,IS)),JJ=1,2),if=1,11),it=1,24),
C125SPLP     +      IM=1,4),IS=1,2)
C125SPLP            WRITE(m100)(((zACAV(it,im,is),zASM(IT,IM,IS),
C125SPLP     +      zDSL(IT,IM,IS),zDSU(IT,IM,IS),(zYMUF(ILY,IT,IM,IS),
C125SPLP     +      zYFOT(ILY,IT,IM,IS),zYHPF(ILY,IT,IM,IS),
C125SPLP     +      zFVMUF(ILY,IT,IM,IS),zSIGL(ILY,IT,IM,IS),
C125SPLP     +      zSIGU(ILY,IT,IM,IS),ILY=1,4),it=1,24),IM=1,4),IS=1,2)
C125SPLP          ENDIF
C125PC          close(M100)
C125PC        ENDIF
C125PC      ENDIF
      RETURN
      END
C--------------------------------

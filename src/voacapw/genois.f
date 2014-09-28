c###genois.for
      SUBROUTINE GENOIS!  (rcnse_x,du_x,dl_x,sigm_x,sygu_x,sygl_x,rnse)
c          GENOIS for Power sum #1 = Spaulding's original with Caruana modification
c          This is the change per ITU submission to ITU-R P.372-8 April 2007 GRH
      common /c_noise/ mode_n1,mode_n2,mode_n3
      common /c_noises/ xn_atnos,xn_galactic,xn_mmn
      common /c_method_used/ method_used
C--------------------------------
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
C--------------------------------
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
C
C     THIS ROUTINE COMPUTES THE COMBINED NOISE DISTRIBUTION - as prescribed
C       in ITS Report 87-212 "Updated NOISE Model for use in IONCAP"
C
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/ANOIS/ATNU,ATNY,CC,TM,RCNSE,DU,DL,SIGM,SYGU,SYGL,KJ,JK
      COMMON /TON /ADJ, ADS, GNOS, GOT, REL, SL, SLS
     1, SPR, SU, SUS, XNOISE, ZNOISE, NF
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON / TWO / F2D(16,6,6), P(29,16,8), ABP(2,9), DUD(5,12,5),
     A FAM(14,12), SYS(9,16,6), PERR(9,4,6)
      COMMON / ZON / ABPS(7), CREL(7), EFF(7), FLDST(7), GRLOS(7),
     1 HN(7), HP(7), PROB(7), RELY(7), RGAIN(7), SIGPOW(7), SN(7),
     2 SPRO(7), TGAIN(7), TIMED(7), TLOSS(7), B(7), FSLOS(7), ADV(7),
     3 OBF(7),NMODE(7),TLLOW(7),TLHGH(7)
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
      real*8 vu,vl,au,al,vu1,vu2,vu3,vl1,vl2,vl3
      real*8 au1,au2,au3,al1,al2,al3,sigtsqu,sigtsql
      real*8 dxx,dxx2,dxx3
      DIMENSION XNINT(6),conn(6)
C.....MAN-MADE NOISE LEVELS
c                   #1         #2        #3     #4     #5     #6
c               industrial residential  rural remote  noisy  quiet
      data conn/   27.7   ,   27.7    , 27.7 , 28.6 , 37.5 , 29.1 /
      DATA XNINT / 76.8   ,   72.5    , 67.2 , 53.6 , 83.2 , 65.2 /
c         -27.7 for #4 (remote) changed 4/13/07 from -28.6 per GRH to match ITU report
c         changed back 8/14/07 to match ITU Rec372 - Radio Noise
C
      DATA DFAC,BFAC,CFAC /7.87384, 30.99872, 5.56765/
C
C        7.87384=SQRT(2 * 1.282**2 * 4.34294**2)
C
C       30.99872=(1.282**2)*(4.34294**2)
C
C        5.56765=4.34294 * 1.282
C
C.....DATA ARE FA VALUES AT 1 MHZ
C
C.....ATNU, ATNY ARE DB .GT. KTB FOR 1 MHZ
C.....ATNZ, ATNX ARE DB .GT. KTB FOR FREQ
C.....ATNOS, GNOS, XNOIS ARE DB .GT. KTB FOR ALL CALCULATIONS
C.....AND ARE CONVERTED TO DBW(1 HZ BWDTH) AT END OF ROUTINE
C.....UPPER LIMIT IS 55 MHZ FOR NOISE
      if(nyear.eq.' 2089') then    !  use old genois
         call genois_old
         return
      end if

      DUME=AMIN1(FREQ,55.)
      MAN=NOISE
C
C     FREQUENCY DEPENDENT ATMOSPHERIC NOISE
C
      IF (F2D(1,1,1)) 85,90,90
C.....NO IONOSPHERIC LONG TERM DATA BASE FILE
C.....FORCE MAN-MADE NOISE OR GALACTIC NOISE
   85 ATNOS=204.
      atnos=0.
      DUA=9.
      DLA=7.
      SMA=3.
      SUA=1.5
      SLA=1.5
      GO TO 95
   90 CONTINUE
C.....FREQUENCY DEPENDENCE
      CALL GENFAM(RLAT,KJ,DUME,ATNU,ATNZ,DU,DL,SIGM,SYGU,SYGL)
ccc      write(luo,'(''genfam='',i5,5f10.4)') kj,freq,atnu,atnz,du,dl
      CALL GENFAM(RLAT,JK,DUME,ATNY,ATNX,DX,DQ,SIGZ,SIGX,SIGSQ)
ccc      write(luo,'(''genfam='',i5,5f10.4)') jk,freq,atny,atnx,dx,dq
C.....BEGIN INTERPOLATION ON LOCAL TIME
      SLOP=ABS(CC-TM)/4.
      ATNOS=ATNZ+(ATNX-ATNZ)*SLOP
ccc      atnos=atnz
ccc      dua=du
ccc      dla=dl
ccc      sma=sigm
ccc      sua=sygu
ccc      sla=sygl
ccc      write(luo,'(''atnos='',5f10.4)') atnos,atnz,atnx,atnz,slop
      DUA=DU+(DX-DU)*SLOP
      DLA=DL+(DQ-DL)*SLOP
      SMA=SIGM+(SIGZ-SIGM)*SLOP
      SUA=SYGU+(SIGX-SYGU)*SLOP
      SLA=SYGL+(SIGSQ-SYGL)*SLOP
ccc      write(luo,'(''atnos='',3f10.4)') atnos,dua,dla
C.....END OF INTERPOLATION ON LOCAL TIME
C
C     (DUA/DFAC)**2=(DUA/1.282)**2/(2*4.34294**2)
C                  =(DUA/SQRT(2*1.282**2*4.34294**2))**2
C                  =(DUA/7.87384)**2
C
95    continue
      atnos_sav=atnos
cxxx      if(mode_n1.eq.0) atnos=204.
ccc      if(mode_n1.eq.0) atnos=0.
      AU1=EXP((DUA/DFAC)**2+(ATNOS/4.34294))
ccc      write(*,'(''au='',4f15.4)') au,dua,dfac,atnos
      VU1=AU1*AU1*(EXP(DUA*DUA/BFAC)-1.)
      AL1=EXP((DLA/DFAC)**2+(ATNOS/4.34294))
C
C     DLA*DLA/BFAC=(DLA/1.282)**2/(4.34294)**2
C                 =DLA**2/30.99872
C
      VL1=AL1*AL1*(EXP(DLA*DLA/BFAC)-1.)
cxxx      if(mode_n1.eq.0) then         !  ignore Atmospheric noise
cxxx         au1=0.
cxxx         vu1=0.
cxxx         al1=0.
cxxx         vl1=0.
cxxx      end if
      atnos=atnos_sav
c****************************************************************
C
C     GALACTIC NOISE
C
      GNOS=52.-23.*ALOG10(FREQ)
      DUG=2.
      DLG=2.
      SMG=.5
      SUG=.2
      SLG=.2
      mode_n2=1
cxxx      if(mode_n2.eq.0) gnos=0.    !  ignore galactic noise
ccc      write(*,'(''gnos='',f10.4)') gnos
      AU2=EXP((DUG/DFAC)**2+(GNOS/4.34294))
      VU2=AU2*AU2*(EXP(DUG*DUG/BFAC)-1.)
      AL2=EXP((DLG/DFAC)**2+(GNOS/4.34294))
      VL2=AL2*AL2*(EXP(DLG*DLG/BFAC)-1.)
ccc      write(*,'(''gnos ='',3f10.4)') gnos,dug,dlg
      IF(FREQ.le.FI(3,KFX)) then    !  Galactic noise does not penetrate -- ignore
ccc      if(mode_n2.eq.0) then         !  ignore Galactic noise
         mode_n2=0    !  ignore Galactic noise
         gnos=0.
         au2=0.
         vu2=0.
         al2=0.
         vl2=0.
      end if
c****************************************************************
C
C     MAN MADE NOISE
C
      MAN=NOISE
      XNOIS=MAN
      MA=IABS(MAN)
      ZNOISE=XNOIS
      IF (MAN) 120,114,115
C.....INDICATES -164 ON USER INPUT
  114 MA=4
      GO TO 120
C.....CONVERT 3 MHZ DB .LT. 1 WATT INPUT VALUE TO FA AT 1 MHZ
  115 XNOIS=204.0-XNOIS+13.22
C.....OBTAIN FA AT DESIRED FREQUENCY
      XNOIS=XNOIS-27.7*ALOG10(FREQ)
      GO TO 125
C.....NEGATIVE ON USER INPUT INDICATES INDEX
  120 MA=MIN0(4,MA)
      XNOIS=XNINT(MA)-CONN(MA)*ALOG10(FREQ)
      ZNOISE=204.0 - XNINT(MA) + conn(ma)*alog10(3.)   !  3 MHz noise
  125 DUM=9.7
ccc      write(luo,'(''znoise='',2f12.5,i5)') znoise,xnois,man
      DLM=6.
      SUM=1.5
      SMM=5.4
      SLM=1.5
cxxx      if(mode_n3.eq.0) xnois=0.
      AU3=EXP((DUM/DFAC)**2+(XNOIS/4.34294))
      VU3=AU3*AU3*(EXP(DUM*DUM/BFAC)-1.)
      AL3=EXP((DLM/DFAC)**2+(XNOIS/4.34294))
      VL3=AL3*AL3*(EXP(DLM*DLM/BFAC)-1.)
ccc      write(*,'(''mnos ='',3f10.4)') xnois,dum,dlm
cxxx      if(mode_n3.eq.0) then         !  ignore Man-Made noise
cxxx         au3=0.
cxxx         vu3=0.
cxxx         al3=0.
cxxx         vl3=0.
cxxx      end if
c****************************************************************
      VU=VU1+VU2+VU3
      VL=VL1+VL2+VL3
      AU=AU1+AU2+AU3
      AL=AL1+AL2+AL3
ccc      write(*,'(''vu='',4e15.7)') vu,vu1,vu2,vu3
ccc      write(*,'(''vl='',4e15.7)') vl,vl1,vl2,vl3
ccc      write(*,'(''au='',4e15.7)') au,au1,au2,au3
ccc      write(*,'(''al='',4e15.7)') al,al1,al2,al3
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C.....RECEIVER ANTENNA EFFICIENCY
C125PC      if(MSPEC.eq.125)then
c.......set rec EFF to 0. dB or unity
C125PC        reff=0.
C125PC      else
      reff=0
        CALL GAIN(2,0.0,FREQ,GDUM,reff)
C125PC      ENDIF
C.....SET ARRAY FOR ALL POSSIBLE MODES
      DO 196 IM=1,6
  196 EFF(IM) = reff
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C.....NOW DETERMINATION OF NOISE LEVEL IS ITS-78 (HFMUFES4)
C.....SWITCH TO DB .GT. WATT
ccc      write(luo,'(''atnos,gnos,xnois='',3f10.4)') atnos,gnos,xnois
      ATNOS=ATNOS-204.
      GNOS=GNOS-204.
      XNOIS=XNOIS-204.

      xn_atnos=atnos
      xn_galactic=gnos
      xn_mmn=xnois
      rnse=4.34294*alog((10.**(atnos*.1))+(10.**(gnos*.1))
     +     + 10.**((xnois*.1)))

c************************************************************
c          This is where Spaulding's original replaces simple power sum
c************************************************************

      SIGTSQu=dLOG(1.d0+VU/(AU*AU))
      SIGTSQl=dLOG(1.d0+VL/(AL*AL))
      method_used=2
      if(dua.le.12. .and. dla.le.12.) go to 200  ! use Spaulding original
c************************************************************
c          This is where Caruana's modification begins
c************************************************************
c          see if the Spaulding method is going to breakdown
      dxx=rnse         !  this is the simple power sum
      dxx=dxx+204.
      dxx2=2.d0*(dlog(au)-dxx/4.34294d0)
      dxx3=2.d0*(dlog(al)-dxx/4.34294d0)
ccc      dxx3=2.d0*(dlog(al)-rnse/4.34294d0)
ccc      write(*,'(''dxx='',8f15.5)') atnos,gnos,xnois,dxx,dxx2,
ccc     +       dxx3,sigtsqu,au
      if(sigtsqu.gt.dxx2 .and. dxx2.gt.0.) then
         method_used=1
         sigtsqu=dxx2   !  problem with Spaulding, limit SIGTSQu
ccc         sigtsql=dxx3   !  problem with Spaulding, limit SIGTSQl
      end if
      if(sigtsql.gt.dxx3 .and. dxx3.gt.0.) then
         method_used=1
         sigtsql=dxx3   !  problem with Spaulding, limit SIGTSQl
      end if
c************************************************************
c          End Caruana's modification
c************************************************************
200   XRNSE=4.34294*(dLOG(AU)-SIGTSQu/2.)-204.
ccc      write(luo,'(''xrnse='',5f15.4)') xrnse,au,sigtsq,xnois,rnse
C.....UPPER DECILE
C
C     CFAC=4.34294*1.282
C         =5.56765
C
      DU=CFAC*dSQRT(SIGTSQu)


C.....LOWER DECILE
      DL=CFAC*dSQRT(SIGTSQl)

      QPA=10.**((ATNOS-XRNSE)*0.1)
cxxx      if(mode_n1.eq.0) QPA=0.      !  no atmospheric noise requested
      QPG=10.**((GNOS-XRNSE)*0.1)
      if(mode_n2.eq.0) QPG=0.      !  no galactic    noise requested
C.....PREDICTION ERRORS
C.....SIGM IS MEDIAN, SYGU IS UPPER AND SYGL IS LOWER
      QPM=10.**((XNOIS-XRNSE)*0.1)
cxxx      if(mode_n3.eq.0) QPM=0.      !  no man-made    noise requested
      SIGM=SQRT((QPA*SMA)**2+(QPG*SMG)**2+(QPM*SMM)**2)  !  /QP
C
C     0.23026=1.0/4.34294
C
      PV=QPA*EXP((DUA-DU)*0.23026)
      SYGU1=(PV*SUA)**2+((PV-QPA)*SMA)**2
      PV=QPG*EXP((DUG-DU)*0.23026)
      SYGU2=(PV*SUG)**2+((PV-QPG)*SMG)**2
      PV=QPM*EXP((DUM-DU)*0.23026)
      SYGU3=(PV*SUM)**2+((PV-QPM)*SMM)**2
      SYGU=SQRT(SYGU1 + SYGU2 + SYGU3)

      PV=QPA*EXP((DLA-DL)*0.23026)
      SYGL1=(PV*SLA)**2+((PV-QPA)*SMA)**2
      PV=QPG*EXP((DLG-DL)*0.23026)
      SYGL2=(PV*SLG)**2+((PV-QPG)*SMG)**2    !  original code used SMA
CCCC  SYGL=SYGL+(PV*SLG)**2+((PV-QPG)*SMA)**2    !  as in this commented line
      PV=QPM*EXP((DLM-DL)*0.23026)
      SYGL3=(PV*SLM)**2 +((PV-QPM)*SMM)**2
      SYGL=SQRT(SYGL1 + SYGL2 + SYGL3)
c**************************************************************
c          End Spaulding's original
c**************************************************************
ccc      write(*,'(''sigm ='',4f12.4)') sigm,sma,smg,smm
ccc      write(*,'(''sigu ='',4f12.4)') sygu,sua,sug,sum
ccc      write(*,'(''sigl ='',4f12.4)') sygl,sla,slg,slm
ccc      write(*,'(''xrnse='',5f12.4)') 
ccc     +   xrnse+204.,atnos+204.,gnos+204.,xnois+204.,rnse+204.
ccc      write(*,'(''QP   ='',3f12.4)') qpa,qpg,qpm
C
C     RCVR SITE NOISE = TOTAL NOISE + ANTENNA EFFICENCY (ADDED TO
C                       SIGNAL WITH GAIN)
C
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C------------    CHANGED 9/24/91  (LONG PATH RCVR EFF CORRECTION)  FJR
C------------          210 RCNSE = XRNSE + reff
      RCNSE = XRNSE
      XNOISE=XNOIS
cxxx      rcnse_x=rcnse
ccc      write(luo,'(''xnois='',5f10.4)') rcnse,rcnse+204.,atnos+204.,
ccc     +          gnos+204.,xnois+204.
cxxx      du_x=du
cxxx      dl_x=dl
cxxx      sigm_x=sigm
cxxx      sygu_x=sygu
cxxx      sygl_x=sygl
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
      RETURN
      END
C--------------------------------
c###genois.for
      SUBROUTINE GENOIS_old
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
C--------------------------------
C
C     THIS ROUTINE COMPUTES THE COMBINED NOISE DISTRIBUTION - as prescribed
C       in ITS Report 87-212 "Updated NOISE Model for use in IONCAP"
C
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/ANOIS/ATNU,ATNY,CC,TM,RCNSE,DU,DL,SIGM,SYGU,SYGL,KJ,JK
      COMMON /TON /ADJ, ADS, GNOS, GOT, REL, SL, SLS
     1, SPR, SU, SUS, XNOISE, ZNOISE, NF
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON / TWO / F2D(16,6,6), P(29,16,8), ABP(2,9), DUD(5,12,5),
     A FAM(14,12), SYS(9,16,6), PERR(9,4,6)
      COMMON / ZON / ABPS(7), CREL(7), EFF(7), FLDST(7), GRLOS(7),
     1 HN(7), HP(7), PROB(7), RELY(7), RGAIN(7), SIGPOW(7), SN(7),
     2 SPRO(7), TGAIN(7), TIMED(7), TLOSS(7), B(7), FSLOS(7), ADV(7),
     3 OBF(7),NMODE(7),TLLOW(7),TLHGH(7)
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
      DIMENSION XNINT(4)
C.....MAN-MADE NOISE LEVELS
      DATA XNINT /76.8, 72.5, 67.2, 53.6/
C
      DATA DFAC,BFAC,CFAC /7.87384, 30.99872, 5.56765/
C
C        7.87384=SQRT(2 * 1.282**2 * 4.34294**2)
C
C       30.99872=(1.282**2)*(4.34294**2)
C
C        5.56765=4.34294 * 1.282
C
C.....DATA ARE FA VALUES AT 1 MHZ
C
C.....ATNU, ATNY ARE DB .GT. KTB FOR 1 MHZ
C.....ATNZ, ATNX ARE DB .GT. KTB FOR FREQ
C.....ATNOS, GNOS, XNOIS ARE DB .GT. KTB FOR ALL CALCULATIONS
C.....AND ARE CONVERTED TO DBW(1 HZ BWDTH) AT END OF ROUTINE
C.....UPPER LIMIT IS 55 MHZ FOR NOISE
      DUME=AMIN1(FREQ,55.)
      MAN=NOISE
C
C     FREQUENCY DEPENDENT ATMOSPHERIC NOISE
C
      IF (F2D(1,1,1)) 85,90,90
C.....NO IONOSPHERIC LONG TERM DATA BASE FILE
C.....FORCE MAN-MADE NOISE OR GALACTIC NOISE
   85 ATNOS=204.
      DUA=9.
      DLA=7.
      SMA=3.
      SUA=1.5
      SLA=1.5
      GO TO 95
   90 CONTINUE
C.....FREQUENCY DEPENDENCE
      CALL GENFAM(RLAT,KJ,DUME,ATNU,ATNZ,DU,DL,SIGM,SYGU,SYGL)
ccc      write(luo,'(''Genfam='',i5,3f10.4)') kj,freq,atnu,atnz
      CALL GENFAM(RLAT,JK,DUME,ATNY,ATNX,DX,DQ,SIGZ,SIGX,SIGSQ)
ccc      write(luo,'(''Genfam='',i5,3f10.4)') jk,freq,atny,atnx
C.....BEGIN INTERPOLATION ON LOCAL TIME
      SLOP=ABS(CC-TM)/4.
      ATNOS=ATNZ+(ATNX-ATNZ)*SLOP
ccc      write(luo,'(''atnos='',5f10.4)') atnos,atnz,atnx,atnz,slop
      DUA=DU+(DX-DU)*SLOP
      DLA=DL+(DQ-DL)*SLOP
      SMA=SIGM+(SIGZ-SIGM)*SLOP
      SUA=SYGU+(SIGX-SYGU)*SLOP
      SLA=SYGL+(SIGSQ-SYGL)*SLOP
C.....END OF INTERPOLATION ON LOCAL TIME
C
C     (DUA/DFAC)**2=(DUA/1.282)**2/(2*4.34294**2)
C                  =(DUA/SQRT(2*1.282**2*4.34294**2))**2
C                  =(DUA/7.87384)**2
C
   95 AU=EXP((DUA/DFAC)**2+(ATNOS/4.34294))
ccc      write(luo,'(''au='',4f15.4)') au,dua,dfac,atnos
      VU=AU*AU*(EXP(DUA*DUA/BFAC)-1.)
      AL=EXP((DLA/DFAC)**2+(ATNOS/4.34294))
C
C     DLA*DLA/BFAC=(DLA/1.282)**2/(4.34294)**2
C                 =DLA**2/30.99872
C
      VL=AL*AL*(EXP(DLA*DLA/BFAC)-1.)
C
C     GALACTIC NOISE
C
      IF(FREQ.le.FI(3,KFX)) then   !  FI(3,..) is F2 critical frequency
         GNOS=0.        !  GALACTIC NOISE DOES NOT PENETRATE
      else
         GNOS=52.-23.*ALOG10(FREQ)
      end if
      DUG=2.
ccc      write(luo,'(''gnos='',f10.4)') gnos
      AT=EXP((DUG/DFAC)**2+(GNOS/4.34294))
      AU=AU+AT
      VU=VU+AT*AT*(EXP(DUG*DUG/BFAC)-1.)
      DLG=2.
      AT=EXP((DLG/DFAC)**2+(GNOS/4.34294))
      AL=AL+AT
      VL=VL+AT*AT*(EXP(DLG*DLG/BFAC)-1.)
      SMG=.5
      SUG=.2
      SLG=.2
C
C     MAN MADE NOISE
C
      MAN=NOISE
      XNOIS=MAN
      MA=IABS(MAN)
      ZNOISE=XNOIS
      IF (MAN) 120,114,115
C.....INDICATES -164 ON USER INPUT
  114 MA=4
      GO TO 120
C.....CONVERT 3 MHZ DB .LT. 1 WATT INPUT VALUE TO FA AT 1 MHZ
  115 XNOIS=204.0-XNOIS+13.22
C.....OBTAIN FA AT DESIRED FREQUENCY
      XNOIS=XNOIS-27.7*ALOG10(FREQ)
      GO TO 125
C.....NEGATIVE ON USER INPUT INDICATES INDEX
  120 MA=MIN0(4,MA)
      CONN=27.7
ccc      IF (MA.EQ.4) CONN=28.6    !  removed 4/13/2007 GRH
      XNOIS=XNINT(MA)-CONN*ALOG10(FREQ)
      ZNOISE=204.0-XNINT(MA)+13.22
  125 DUM=9.7
      AT=EXP((DUM/DFAC)**2+(XNOIS/4.34294))
      AU=AU+AT
      VU=VU+AT*AT*(EXP(DUM*DUM/BFAC)-1.)
      DLM=6.
      AT=EXP((DLM/DFAC)**2+(XNOIS/4.34294))
      AL=AL+AT
      VL=VL+AT*AT*(EXP(DLM*DLM/BFAC)-1.)
      SUM=1.5
      SMM=5.4
      SLM=1.5
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C.....RECEIVER ANTENNA EFFICIENCY
C125PC      if(MSPEC.eq.125)then
c.......set rec EFF to 0. dB or unity
C125PC        reff=0.
C125PC      else
        CALL GAIN(2,0.0,FREQ,GDUM,reff)
C125PC      ENDIF
C.....SET ARRAY FOR ALL POSSIBLE MODES
      DO 196 IM=1,6
  196 EFF(IM) = reff
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C.....NOW DETERMINATION OF NOISE LEVEL IS ITS-78 (HFMUFES4)
C.....SWITCH TO DB .GT. WATT
ccc      write(luo,'(''atnos,gnos,xnois='',3f10.4)') atnos,gnos,xnois
      ATNOS=ATNOS-204.
      GNOS=GNOS-204.
      XNOIS=XNOIS-204.
      SIGTSQ=ALOG(1.+VU/(AU*AU))
      XRNSE=4.34294*(ALOG(AU)-SIGTSQ/2.)-204.
ccc      write(luo,'(''xrnse='',3f15.4)') xrnse,au,sigtsq
C.....UPPER DECILE
C
C     CFAC=4.34294*1.282
C         =5.56765
C
      DU=CFAC*SQRT(SIGTSQ)
      SIGTSQ=ALOG(1.+VL/(AL*AL))
C.....LOWER DECILE
      DL=CFAC*SQRT(SIGTSQ)
      IF (ITRUN-8) 205,210,205
  205 QPA=10.**((ATNOS-XRNSE)*0.1)
      QPG=10.**((GNOS-XRNSE)*0.1)
C.....PREDICTION ERRORS
C.....SIGM IS MEDIAN, SYGU IS UPPER AND SYGL IS LOWER
      QPM=10.**((XNOIS-XRNSE)*0.1)
      SIGM=SQRT((QPA*SMA)**2+(QPG*SMG)**2+(QPM*SMM)**2)
ccc      write(*,'(''sigm ='',4f10.4)') sigm,sma,smg,smm
ccc      write(*,'(''xrnse='',4f10.4)') 
ccc     +           xrnse+204.,atnos+204.,gnos+204.,xnois+204.
ccc      write(*,'(''QP?  ='',10x,3f10.4)') qpa,qpg,qpm
C
C     0.23026=1.0/4.34294
C
      PV=QPA*EXP((DUA-DU)*0.23026)
      SYGU=(PV*SUA)**2+((PV-QPA)*SMA)**2
      PV=QPG*EXP((DUG-DU)*0.23026)
      SYGU=SYGU+(PV*SUG)**2+((PV-QPG)*SMG)**2
      PV=QPM*EXP((DUM-DU)*0.23026)
      SYGU=SQRT(SYGU+(PV*SUM)**2+((PV-QPM)*SMM)**2)
      PV=QPA*EXP((DLA-DL)*0.23026)
      SYGL=(PV*SLA)**2+((PV-QPA)*SMA)**2
      PV=QPG*EXP((DLG-DL)*0.23026)
      SYGL=SYGL+(PV*SLG)**2+((PV-QPG)*SMG)**2    !  original code used SMA
CCCC  SYGL=SYGL+(PV*SLG)**2+((PV-QPG)*SMA)**2    !  as in this commented line
      PV=QPM*EXP((DLM-DL)*0.23026)
      SYGL=SQRT(SYGL+(PV*SLM)**2 +((PV-QPM)*SMM)**2)
C
C     RCVR SITE NOISE = TOTAL NOISE + ANTENNA EFFICENCY (ADDED TO
C                       SIGNAL WITH GAIN)
C
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C------------    CHANGED 9/24/91  (LONG PATH RCVR EFF CORRECTION)  FJR
C------------          210 RCNSE = XRNSE + reff
  210 RCNSE = XRNSE
      XNOISE=XNOIS
ccc      write(luo,'(''xnois='',5f10.4)') rcnse,rcnse+204.,atnos+204.,
ccc     +          gnos+204.,xnois+204.
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
      RETURN
      END
C--------------------------------

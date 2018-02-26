c###luffy.for
      SUBROUTINE LUFFY(IPFLAG)
C-------------------------------
C
C     LONG AND SHORT PATH SYSTEM PERFORMANCE AND LUFS.
C
C          IPFG        FUNCTION
C          ----        -----------------------------
C           100        SHORT PATH SYSTEM PERFORMANCE
C           200        LONG PATH SYSTEM PERFORMANCE
C           300        SHORT PATH LUF
C           400        LONG PATH LUF
C
      common /gh_ipfg/ ipfg
      COMMON/ANOIS/ATNU,ATNY,CC,TM,RCNSE,DU,DL,SIGM,SxGU,SxGL,KJ,JK
      common /cgains/ gaint(13),gainr(13)    !  transmitter & receiver gains
      common /sncom/ snxx(13)
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON / OUTLAB / LABEL(11), LAYTYP(5), IEAST, INORTH, ISOUTH,
     A IWEST, LABLI, LABLJ, LABLK
      CHARACTER IEAST*1, ISOUTH*1, INORTH*1, IWEST*1, LABEL*5, LAYTYP*2,
     A LABLI*5, LABLJ*5, LABLK*5
      COMMON / OUTPRT / LINBOT(26), LINBD(14), LINTOP(15), LINTP(14),
     A GRPTYP, JOUT, LINBYP, LINES, LINMAX, LINTYP, LPAGES, NLINE
      COMMON / SON / ANGLE(13), ANGLER(13), CPROB(13), DBLOS(13),
     A DBLOSL(13), DBLOSU(13), DBU(13), DELAY(13), DBW(13), NHP(13),
     B XNYNOIS(13), PROBMP(13), RELIAB(13), SNDB(13), SNPR(13),
     C SNRLW(13), SNRUP(13), SPROB(13), VHIGH(13), RNEFF(13),MDL(13)
       CHARACTER MDL*1
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24),FOT
     A (24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMUF(4)
     B ,HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     C ,YMUF(4)
      COMMON/SIGD/DSL,ASM,DSU,AGLAT,DSLF,ASMF,DSUF,ACAV,FEAV,AFE,BFE,HNU
     A,HTLOSS,XNUZ,XVE
      COMMON /SSP /SUN (2, 12), MONTH
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON /TON /ADJ, ADS, GNOS, GOT, REL, SL, SLS
     1, SPR, SU, SUS, XNOISE,ZNOISE,NF
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON/REFLX/DELFX(45,3),HPFLX(45,3),HTFLX(45,3),GDFLX(45,3),FVFLX
     A (45,3),DSKPKM(3),DELSKP(3),HPSKP(3),HTSKP(3),DMAXKM(3),FVSKP(3)
     B ,ISKP(3),IMODE(45,3),AFFLX(45,3),DELPEN(3,5),GML(45,3),FHP(45,3)
      COMMON /MODES /GHOP, DELMOD (6, 3), HPMOD (6, 3), HTMOD (6, 3), FV
     1MOD (6, 3), ITMOD (6, 3), AFMOD (6, 3)
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / allMODE /ABPS(20),CREL(20),FLDST(20),HN(20),HP(20),
     1PROB(20),RELY(20),RGAIN(20),SIGPOW(20),SN(20),
     2SPRO(20),TGAIN(20),TIMED(20),TLOSS(20),B(20),FSLOS(20),
     3grlos(20),adv(20),obf(20),
     CNMODE(20),TLLOW(20),TLHGH(20),EFF(20),NREL,NMMOD
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
C125PC      COMMON/INDICEZ/ISP,ISZ,IFQN,IUR,NMS(11,24,4,2)
      COMMON/INFORM/INFO,IHSHR,IHLNG
C125PC      COMMON/LPALMOT2/zACAV(24,4,2),zASM(24,4,2),zDSL(24,4,2),
C125PC     +zDSU(24,4,2),zYMUF(4,24,4,2),zYFOT(4,24,4,2),zYHPF(4,24,4,2),
C125PC     +zFVMUF(4,24,4,2),zSIGL(4,24,4,2),zSIGU(4,24,4,2)
CPC      COMMON/MAPS/VMAP(12,6,50,90),PNT(2,3),ILT,ILG,NLAT,NLON,IMAP
ccc      COMMON/MAPS/VMAP(3,3,45,50),PNT(2,3),ILT,ILG,NLAT,NLON,IMAP
C.....TEMPORARY STORAGE
      dimension xdbu(13,2),xreliab(13,2),xsndb(13,2),xsnpr(13,2),
     +xsnrlw(13,2),xsnrup(13,2),xsprob(13,2),yANGLE(13,2), yCPROB(13,2),
     +yDBLOS(13,2),yDBLOSL(13,2),yDBLOSU(13,2),yDELAY(13,2),
     +xprobmp(13,2),yDBW(13,2),iNHP(13,2),yVHIGH(13,2),zdu(13),zdl(13),
     +zangler(13),INMODe(13)
      dimension yrgain(13,2),ytgain(13,2)
      character*2 ymode(13,2),zmoder(13)
      CHARACTER XMDL*1
      CHARACTER MFLG(3)*3,ITF*1
      ITF=CHAR(12)
      IPFG=IPFLAG
      mspec_temp=mspec
ccc      write(luo,'('' In LUFFY('',2i5,'')  gcdkm='',2f10.2)') 
ccc     +      mspec,ipfg,gcdkm,gcd
      if(mspec.eq.121 .and. gcdkm.gt.10000.) ipfg=200   !  use long path method
      if(mspec.eq.121 .and. gcdkm.gt.10000.) mspec=0    !  beyond smoothing distance
ccc      write(luo,'('' In luffy('',2i5,'')  gcdkm='',2f10.2)') 
ccc     +      mspec,ipfg,gcdkm,gcd
      IF(IPFG.GT.200)THEN
C.......FREQUENCY COMPLEMENT
        CALL FRQCOM(FREA,0)
        PLUF = LUFP
C.......REQUIRED RELIABILITY
        PLUF = .01 * PLUF
      ENDIF
C.....SELECT CONTROLLING SAMPLE AREA
      CALL SELMOD
C  GET NOISE VALUE AT 1 MHZ  FOR RCVR SITE
      CALL ANOIS1
   83 IF((IPFG.EQ.100).OR.(IPFG.EQ.300))THEN
C.......SET FLAG TO INDICATE SHORT PATH MODEL
        JLONG = -1
        XMDL='S'
C.......SELECT SHORT PATH CONTROL AREA
        K = JMODE
      ELSE
C.......SET FLAG TO INDICATE LONG PATH MODEL
        JLONG = 1
        XMDL='L'
C.......SELECT LONG PATH TRANSMITTER AREA
        K=1
        ITXRCP(1)=1
        IF(MSPEC.EQ.121.AND.JMODE.EQ.K)GO TO 85             !  C121PC
C125PC        IF(MSPEC.EQ.125.AND.JMODE.EQ.K)GO TO 85
      ENDIF
C.....ELECTRON DENSITY PROFILE
   84 CALL LECDEN(K)
C.....IONOGRAM
      CALL GENION(K)
C.....REFLECTRIX TABLE
      CALL FOBBY(K)
C.....DEVIATION LOSS FACTOR
      CALL ALOSFV(K)
      IF((IPFG.EQ.100).OR.(K.GT.1))GO TO 87
C.....FIND LONG PATH RECEIVER INDEX
   85 K=KFX
      ITXRCP(2)=KFX
      IF(MSPEC.EQ.121.AND.JMODE.EQ.K)GO TO 87             !  C121PC
C125PC      IF(MSPEC.EQ.125.AND.JMODE.EQ.K)GO TO 87
      IF(KFX.EQ.1)THEN
        ITXRCP(2)=2
        GO TO 87
      ENDIF
      GO TO 84
C.....CAN RUN FOR SHORT PATHS ALSO
C.....SO SET ALL SAMPLE AREAS
   87 CALL SETLNG
C.....SET ABSORPTION LOSS PARAMETERS, ADJUST SIGNAL DISTRIBUTION TABLES
ccc      write(*,'(''before sigdis'')')
      CALL SIGDIS
C```````````````````````````````````````````````````````````````
C125SPLP      IF(MSPEC.EQ.125.AND.GCDKM.GE.7000.)THEN
C125SPLP        zacav(IUR,ISZ,ISP)=acav
C125SPLP        zasm(IUR,ISZ,ISP)=asm
C125SPLP        zdsl(IUR,ISZ,ISP)=dsl
C125SPLP        zdsu(IUR,ISZ,ISP)=dsu
C125SPLP        do 88 ILY=1,4
C125SPLP        zymuf(ILY,IUR,ISZ,ISP)=ymuf(ILY)
C125SPLP        zyfot(ILY,IUR,ISZ,ISP)=yfot(ILY)
C125SPLP        zyhpf(ILY,IUR,ISZ,ISP)=yhpf(ILY)
C125SPLP        zfvmuf(ILY,IUR,ISZ,ISP)=fvmuf(ILY)
C125SPLP        zSIGL(ILY,IUR,ISZ,ISP)=SIGL(ILY)
C125SPLP        zSIGU(ILY,IUR,ISZ,ISP)=SIGU(ILY)
C125SPLP   88   continue
C125SPLP      ENDIF
C-----------------------------------------------
      IHMIN=NHOPMF(1)
      IF(NHOPMF(2).GT.0.AND.NHOPMF(2).LT.IHMIN)IHMIN=NHOPMF(2)
      IF(NHOPMF(3).LT.IHMIN)IHMIN=NHOPMF(3)
      IHMX=NHOPMF(1)
      IF(NHOPMF(2).GT.0.AND.NHOPMF(2).GT.IHMX)IHMX=NHOPMF(2)
      IF(NHOPMF(3).GT.IHMX)IHMX=NHOPMF(3)
C-----------------------------------------------
C.....START FREQUENCY LOOP
      NHP   (13) = -1
      MDL(13)=xmdl
      do 90 IFX=1,12
   90 MDL(IFX)=' '
      DO 265 IFX=1,12
      IMAP=0                         !  force IMAP=0
ccc      IF(IMAP.GT.0.AND.IFX.GT.2)THEN
ccc        IF(IFX.LT.12)GO TO 265
ccc      ENDIF
      IPFG=IPFLAG
      IF(IPFG.LT.300)THEN
C.......PRESET IN COMMON/SON/
        NHP   (IFX) = -1
        SPROB(IFX)=.000
        PROBMP(IFX)=.000
        IF (FREL(IFX).le.0.) go to 265     !  no frequency 
        FREQ = FREL(IFX)
      ELSE
        FREQ=FREA(IFX)
        RELIAB(IFX)=0.0
      ENDIF
C--------------------------------------------------
        iflag=0
ccc        IFQN=IFX
        call allMODES(iflag,freq)
C125PC      ENDIF
C.....NOISE DISTRIBUTION
ccc      write(*,'(''before genois'')')
      CALL GENOIS
      IF((IPFG.EQ.100).OR.(IPFG.EQ.300))THEN
        IF(IHSHR.GT.0.OR.IAND(INFO,8).GT.0)THEN
          DO 121 ILI=1,3
  121     MFLG(ILI)='   '
          MFLG(MODMUF)='(*)'
          IF(IFX.EQ.1)WRITE(99,'(A1,/,10x,23hVCLS VERSION Short Path)')
          WRITE(99,'(/,1X,A10,A7,1X,A3,6H  SSN ,F4.0,F6.0,5H UT  ,F7.0,
     1    5H KM  ,F8.3,4H MHZ)')ITF,ITRAN,IMON(MONTH),SSN,GMT,GCDKM,FREQ
          WRITE(99,'(A,3(A3,I1,A2,1H=,F7.2,2X),/,1X,66(1H=))')
     1    '  CIRCUIT(*) MUFS   ',(MFLG(IL),NHOPMF(IL),LAYTYP(IL),
     2    YMUF(IL),IL=1,3)
        ENDIF
C.......RAY SET TABLE
        CALL FINDF(K)
        IF( DMAXKM(K) ) 122,122,123
  122   IF(IPFG.EQ.300)then
          go to 265
        else
C-----------------------------------------------
C.......ONLY ONE OVER THE MUF MODE
C??????????????????????????????????????????
          IHSRT = IHMIN
          IHSTP = IHMIN
        ENDIF
        GO TO 124
C.......UP TO THREE HOPS
  123   IHSRT = GCDKM/DMAXKM(K) + 1.
        IHMAX = GCDKM/DSKPKM(K)
        IF(IHSRT.LT.IHMIN)IHSRT = IHMIN
        IF(IHMAX.LT.IHSRT)IHMAX = IHSRT
        IHSTP =MIN0(IHMAX,IHSRT+2)
        IF(IHSRT.GT.IHMIN)IHSRT=MAX0(IHMIN,IHSTP-2)
C-----------------------------------------------
  124   IXHP = 0
C.......HOP LOOP
      IF(IAND(INFO,1).GT.0)THEN
      WRITE(99,'(A,4I3,3F8.2)')'  IHMIN,IHMAX,IHSRT,IHSTP,DMAX,DSKP,DKM'
     1,IHMIN,IHMAX,IHSRT,IHSTP,DMAXKM(K),DSKPKM(K),GCDKM
      ENDIF
ccc      write(*,'(''before do 262'')')
        DO 262 IHOP = IHSRT, IHSTP
        IXHP = IXHP + 1
        HOP = IHOP
C.......HOP DISTANCE
ccc       write(*,'(''ihop,gcd='',i5,f10.2)') ihop,gcd
        GHOP = GCD/HOP
C.......FIND UP TO SIX MODES
        CALL FDIST(IHOP,K,NUMMOD)
        CALL INMUF(IHOP,NUMMOD)
C.......SAVE ALL MODES AVAILABLE
        CALL ALLMODES(IPFG,HOP)
        IF(IHSHR.GT.0)THEN
          WRITE(99,'(1x,66(1h-))')
        ENDIF
  262   CONTINUE
ccc      write(*,'(''after  do 262'')')
C.......END OF THE HOP LOOP
C.......UP TO 2 ES MODES
        CALL ESMOD
C.......ONE ES - F MODES
        CALL ESREG
        IF(IAND(INFO,8).GT.0)THEN
          WRITE(99,'(1x,66(1h-))')
        ENDIF
        FVAL=999.
C125SPLP        IF(MSPEC.EQ.125.AND.GCDKM.GE.7000.)FVAL=990.
        CALL ALLMODES(2,FVAL)
      ELSE
        IF(IHLNG.GT.0)THEN
          IF(IFX.EQ.1)WRITE(99,'(A1,/,10x,22hVCLS VERSION LONG Path)')
     1    ITF
          DO 263 ILI=1,3
  263     MFLG(ILI)='   '
          MFLG(MODMUF)='(*)'
          WRITE(99,'(/,1X,A10,A7,1X,A3,6H  SSN ,F4.0,F6.0,5H UT  ,F7.0,
     1    5H KM  ,F8.3,4H MHZ)')ITRAN,IMON(MONTH),SSN,GMT,GCDKM,FREQ
          WRITE(99,'(A,3(A3,I1,A2,1H=,F7.2,2X),/,1X,66(1H=))')
     1    '  CIRCUIT(*) MUFS   ',(MFLG(IL),NHOPMF(IL),LAYTYP(IL),
     2    YMUF(IL),IL=1,3)
        ENDIF
C.......AREA COVERAGE, TRANSMITTER END
        CALL FINDF(ITXRCP(1))
C.......AREA COVERAGE, RECEIVER END
        CALL FINDF(ITXRCP(2))
C.......GAIN MINUS LOSS
        CALL GMLOSS
C125SPLP        IF(MSPEC.EQ.125)THEN
C125SPLP          CALL ALLMODES(ipfg,999.)
C125SPLP          GO TO 265
C125SPLP        ENDIF
C````````````````````````````````````````````````````````````````````````
C.......SELECT OPTIMUM AT TRANSMITTER/RECEIVER ENDS
        CALL SELTXR
C.......DO MODE CALCULATIONS
ccc      write(luo,'(''calling LNGPAT'')')
        CALL LNGPAT
        IF(IHLNG.GT.0)THEN
          WRITE(99,'(1x,66(1h-))')
        ENDIF
      ENDIF
C125PC      IF(MSPEC.EQ.125)GO TO 265
C.....COMBINED RELIABILITY
ccc      write(*,'(''before genois2'')')
      call genois
ccc      write(luo,'(''before relbil'')')
      call relbil(IFX,freq)
      IF(IPFG.LT.300)THEN
C.......CHECK TO SEE IF SERVICE PROBABILITIES ARE TO BE OUTPUT
        IF( LINBOT(14) ) 285,285,280
C.......CALCULATE SERVICE PROBABILITIES
  280   CALL SERPRB(IFX)
  285   CONTINUE
        IF(IPFG.EQ.100)THEN
C.........CALCULATE MULTIPATH
          CALL MPATH(IFX)
        ELSE
C.........SET RECEIVER END
          IS = NMODE(2)
          MODER(IFX) = LAYTYP(IS)
          ANGLER(IFX)=B(2)
        ENDIF
C.......normal ALL MODE OUTPUT
        CALL OUTALL(IFX)
      ELSE
C.......TEST RELIABILITY
        IF(RELIAB(IFX).GE.PLUF)GO TO 165
      ENDIF
C121PC      ******************************************************
ccc      write(luo,'(''before IF, mspec,ipflag='',2i5)') mspec,ipflag
ccc      write(luo,'(''angle('',i2,'')='',2f10.3)') 
ccc     +        ifx,angle(ifx),dblos(ifx)
      IF(MSPEC.EQ.121)THEN
        idx=(ipflag+1)/100
ccc        idx=(ipfg+1)/100
ccc      write(luo,'(''ifx,idx,ipflag,ipfg='',4i5)') ifx,idx,ipflag,ipfg
        yangle(IFX,idx)=angle(IFX)
        ycprob(IFX,idx)=cprob(IFX)
        ydblos(IFX,idx)=dblos(IFX)
        ydblosl(IFX,idx)=dblosl(IFX)
        ydblosu(IFX,idx)=dblosu(IFX)
        ydelay(IFX,idx)=delay(IFX)
        ydbw(IFX,idx)=dbw(IFX)
        inhp(IFX,idx)=nhp(IFX)
        yvhigh(IFX,idx)=vhigh(IFX)
        ymode(IFX,idx)=mode(IFX)
        xdbu(IFX,idx)=dbu(IFX)
        xreliab(IFX,idx)=reliab(IFX)
        xsndb(IFX,idx)=sndb(IFX)
        xsnpr(IFX,idx)=snpr(IFX)
        xsnrlw(IFX,idx)=snrlw(IFX)
        xsnrup(IFX,idx)=snrup(IFX)
        xsprob(IFX,idx)=sprob(IFX)
        xprobmp(IFX,idx)=probmp(IFX)
        ytgain(ifx,idx)=tgain(1)
        yrgain(ifx,idx)=rgain(1)
ccc        write(luo,'('' saving gains('',2i3,2h)=,5f10.4)') 
ccc     +    ifx,idx,tgain(1),rgain(1),angle(ifx)!  ,tgain(ifx),rgain(ifx)
ccc      if(ipflag.eq.200) write(luo,1234) du,dl,angler(ifx)
ccc1234  format('ipflag=200=',3f10.3)
       
        IF(IPFLAG.EQ.200)THEN
C.........SAVE LONG PATH PARAMETERS
          ZDU(IFX)=DU
          ZDL(IFX)=DL
          INMODE(IFX)=NMODE(1)
ccc      write(luo,'(''save long path='',i5,f10.3)') ifx,angler(ifx)
          zangler(IFX)=angler(IFX)
ccc          ZRGAIN(IFX)=RGAIN(1)
          zmoder(IFX)=moder(IFX)
        ENDIF
      ENDIF
ccc      write(*,'(''before 265'')')
C121PC      ******************************************************
  265 CONTINUE
C.....END OF FREQUENCY LOOP
ccc      IF(IMAP.GT.0)THEN
cccC.......IF IMAP SAVE DATA
ccc        VMAP(JTX,3,ILT,ILG)=ALLMUF(IT)
ccc        DO 266 IF=1,2
ccc        IF(FREL(IF).GT.0.)THEN
ccc          VMAP(JTX,IF,ILT,ILG)=RSN-SNPR(IF)
ccc        ENDIF
ccc  266   CONTINUE
ccc        RETURN
ccc      ENDIF
C.....IF ALLMODES AND DIST GT 7000 RECYCLE TO CALCULATE LONG PATH MODEL
C125PC      if(MSPEC.eq.125)THEN
C125SPLP        IF(GCDKM.GE.7000..AND.IPFLAG.EQ.100)THEN
C125SPLP          IPFLAG=200
C125SPLP          IPFG=200
C125SPLP          GO TO 83
C125SPLP        ENDIF
C ADD IAND(INFO,16) HERE, IF L/S OUTPUT DESIRED FOR MSPEC 125
C125PC        RETURN
C125PC      ENDIF
      IF(method.eq.25) go to 999    !  done
      IF(IPFG.ge.300) go to 300
C121PC  ************************************************************
c           Smoothing ONLY for distances  7000 - 10000 km
        IF(MSPEC.ne.121) go to 999    !  done
ccc      write(luo,'(''smoothing?'',f10.2)') gcdkm
        IF(gcdkm.lt.7000.) go to 999   !  done
        IF(gcdkm.gt.10000.) go to 999  !!  why was this commented out?
c.........Do LONG and Short Path Model
          if(IPFLAG.EQ.100)THEN
            IPFLAG=200
            IPFG=200
ccc            call outlin
            GO TO 83
          ENDIF
ccc            call outlin
c...........Use LONG Path/Short Path Smoothing From VOA Memo  15 Jan 1991
            MDL(13)='M'
ccc      write(luo,111)
ccc111   format(' Path     gcdkm      if  Freq    ydbw    ',
ccc     +       'ydblosl   s?pld   delpow')
            DO 290 IF=1,12
            mdl(if)=' '
            freq=frel(if)
            if(freq.le.0.)go to 290
            SLpld=YDBW(IF,2)-abs(YDBLOSL(IF,2))   !  abs added 9/14/94 per VOA
            SSpld=YDBW(if,1)-abs(YDBLOSL(if,1))   !  abs added 9/14/94 per VOA
            Delpow=slpld-sspld
ccc      write(luo,112) gcdkm,if,freq,ydbw(if,1),ydblosl(if,1),sspld,delpow
ccc112   format(' Short=',f10.2,i5,f7.3,4f9.3)
ccc      write(luo,113)               ydbw(if,2),ydblosl(if,2),slpld
ccc113   format(' Long =',22x,3f9.3)

ccc            if(delpow.lt.0. .and. gcdkm.le.10000.)then
c          for distances > 10000km, use higher signal level
            if(delpow.lt.0.)then
C.............Use short path
              idx=1
              mdl(if)='S'
              moder(if)=ymode(if,1)
              angler(if)=yangle(if,1)
              tgain(if)=ytgain(IF,1)
              rgain(if)=yrgain(IF,1)
              gaint(if)=tgain(if)      !  this is what is really printed
              gainr(if)=rgain(if)      !  this is what is really printed
              snxx(if)=RSN - xsnpr(if,1)   !  save short path for printing
ccc              write(luo,'('' short smoothing gains='',2f10.4,i5)') 
ccc     +                   tgain(if),rgain(if),if
            else
C.............Use LONG Path or Use Smoothing if Dist LT 10000
              idx=2
              mdl(if)='L'
              moder(if)=zmoder(if)
              angler(IF)=zangler(if)
              du=ZDU(IF)
              dl=ZDL(IF)
              B(1)=yangle(if,2)
              Tllow(1)=ydblosl(if,2)
              Tlhgh(1)=ydblosu(if,2)
              Timed(1)=ydelay(if,2)
              hn(1)=inhp(if,2)
              hp(1)=yvhigh(if,2)
              PROB(1)=YCPROB(IF,2)
              nmode(1)=INMODE(IF)
ccc              rgain(1)=ZRGAIN(IF)
              tgain(1)=ytgain(IF,2)
              rgain(1)=yrgain(IF,2)
ccc              write(luo,'('' long smoothing gains='',2f10.4,i5)') 
ccc     +                   tgain(1),rgain(1),if
              EFF(1)=RNEFF(if)
ccc      write(luo,'(''eff='',f10.3)') eff(1)
            ENDIF
            mode(if)=ymode(if,idx)
            angle(if)=yangle(if,idx)
            cprob(if)=ycprob(if,idx)
            dblos(if)=ydblos(if,idx)
            dblosl(if)=ydblosl(if,idx)
            dblosu(if)=ydblosu(if,idx)
            delay(if)=ydelay(if,idx)
            dbw(if)=ydbw(if,idx)
            nhp(if)=inhp(if,idx)
            RCNSE=XNYNOIS(if)
            vhigh(if)=yvhigh(if,idx)
            dbu(if)=xdbu(if,idx)
            reliab(if)=xreliab(if,idx)
            sndb(if)=xsndb(if,idx)
            snpr(if)=xsnpr(if,idx)
            snrlw(if)=xsnrlw(if,idx)
            snrup(if)=xsnrup(if,idx)
            sprob(if)=xsprob(if,idx)
            probmp(if)=xprobmp(if,idx)
            if(delpow.ge.0. .and. gcdkm.lt.10000.)then
ccc      write(*,'(''delpow>0  gcdkm<10000.'')')
c.............Use Smoothing From VOA Memo  15 Jan 1991
              mdl(if)='M'
              disint=(gcdkm-7000.)/3000.
ccc      write(*,'(''disint='',e15.7,f10.3)') disint,delpow
              if(delpow.gt.375.) then
                 delx=10.**37.5         !  avoid overflow problem
              else
                 delx=10.**(delpow/10.)
              end if
ccc      write(*,'(''delx='',e15.7)') delx
              Smooth=sspld+10.*alog10(disint*(delx-1.)+1.)
              sigpow(1)=smooth+tllow(1)
              TLOSS(1)=PWRDB(FREQ)-SIGPOW(1)
              sn(1)=sigpow(1)-RCNSE-EFF(1)
              fldst(1)=107.2+pwrdb(FREQ)+20.*alog10(freq)-TLOSS(1)-
     +                                                    rgain(1)
              NMMOD=1
              call genois
              CALL RELBIL(if,freq)
              IF( LINBOT(14).gt.0) CALL SERPRB(IF)
            ENDIF
            IF(IAND(INFO,16).GT.0)THEN
              IF(IF.EQ.1)WRITE(99,'(A1,/,10x,22hVCLS VERS L/S SMOOTHIN,
     1        1hG)')ITF
              WRITE(99,'(/,1x,66(1h=))')
              WRITE(99,291)FREQ,GCDKM,MDL(IF),SSPLD,SLPLD,
     1        (YDBW(IF,IP),IP=1,2),SMOOTH,SIGPOW(1)
  291         FORMAT(' "LUFFY"  FREQ=',F6.2,'   DIST=',F7.0,'   L/S PA',
     1       'TH MODEL= "',A1,'"',/,7X,'Short Path SIG L-Decile=',F6.2,
     2        5X,'LONG Path SIG L-Decile=',F6.2,/,7X,'Short Path SIG  ',
     3       'DBW=   ',F6.2,5X,'LONG Path SIG  DBW=    ',F6.2,/,7X,'Sm',
     4       'oothed SIG L-Decile= ',F6.2,5X,'Smoothed SIGPOW','=     ',
     5       '  ',F6.2)
              WRITE(99,'(1x,66(1h-))')
            ENDIF
  290       CONTINUE
C121PC  ************************************************************
        go to 999    !  done
C     NO LUF FOUND. ,FIND HIGHEST RELIABILITY AND QUIT.
300   IG =1
      REL = RELIAB(1)
      DO 160 IF = 2,12
      IF(RELIAB(IF).GT.REL) then     !  is this really correct or should the
         IG = IF                     !  test be RELIAB(IF).gt.RELIAB(IG)
      end if
  160 CONTINUE
      XLUF(IT) = -FREA(IG)
      FREA(13) =  FREA(IG)
      go to 999     !  done
c********************************************************
165   IF=IFX
      IF(IF.eq.1) then            !  1st frequency is good
         XLUF(IT) = FREQ
      else             !  NO ITERATION, SELECT THE FREQUENCY COMPLEMENT FIRST
         FLOW = FREA(IF-1)
         FHIGH= FREA(IF)
         RLOW = RELIAB(IF-1)
         RHIGH= RELIAB(IF)
         IF = 13
C.....USE LINEAR INTERPOLATION
         XLUF(IT) = FLOW + (FHIGH-FLOW)*(PLUF-RLOW)/(RHIGH-RLOW)
      end if
      FREA(13) = XLUF(IT)
999   mspec=mspec_temp
      RETURN
      END
C--------------------------------

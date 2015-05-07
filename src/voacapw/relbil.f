c###relbil.for
      SUBROUTINE RELBIL (IF,freq)
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
C--------------------------------
C
C     THIS ROUTINE COMPUTES THE RELIABILITY FOR EACH MODE AT A
C     PARTICULAR FREQUENCY, SELECTS THE MOST RELIABLE, CALCULATES THE
C     COMBINED DISTRIBUTION FOR ALL MODES AND THE RELIABILITY OF THE
C     COMBINATION, AND THE REQUIRED POWER PLUS GAIN TO ACHIEVE THE
C     REQUIRED RELIABILITY
C
C     SN(IM) = MEAN SIGNAL TO NOISE FOR MODE IM
C
C.....SIGNAL STATISTICS
C DSU =UPPER DECILE SIGNAL LEVEL ADJUSTMENT FROM MEDIAN
C ASM = AURORAL ADJUSTMENT TO MEDIAN SIGNAL LEVEL
C DSL = LOWER DECILE SIGNAL LEVEL ADJUSTMENT FROM MEDIAN
C     ADS = MEAN PREDICTION ERROR
C     SUS = UPPER DECILE PREDICTION ERROR
C     SLS = LOWER DECILE PREDICTION ERROR
C
C.....NOISE STATISTICS
C     RCNSE = MEAN NOISE LEVEL
C     DU = UPPER DECILE NOISE LEVEL
C     DL = LOWER DECILE NOISE LEVEL
C
C     RSN = REQUIRED SIGNAL TO NOISE RATIO
C     DRSN = 2. = RSN PREDICTION ERROR
C     LUFP = GIVEN REQUIRED RELIABILITY
C     IF = FREQUENCY INDEX, FREQ = FREL(IF)
C
C
C     RELIAB(IF) = RELIABILITY
C     SPROB(IF) = SERVICE PROBABILITY
C
c
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
      COMMON/ANOIS/ATNU,ATNY,CC,TM,RCNSE,DU,DL,SIGM,SXGU,SXGL,KJ,JK
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / OUTLAB / LABEL(11), LAYTYP(5), IEAST, INORTH, ISOUTH,
     A IWEST, LABLI, LABLJ, LABLK
      CHARACTER IEAST*1, ISOUTH*1, INORTH*1, IWEST*1, LABEL*5, LAYTYP*2,
     A LABLI*5, LABLJ*5, LABLK*5
      COMMON/SIGD/DSL,ASM,DSU,AGLAT,DSLF,ASMF,DSUF,ACAV,FEAV,AFE,BFE,HNU
     A ,HTLOSS,XNUZ,XVE
      COMMON / SON / ANGLE(13), ANGLER(13), CPROB(13), DBLOS(13),
     A DBLOSL(13), DBLOSU(13), DBU(13), DELAY(13), DBW(13), NHP(13),
     B XNYNOIS(13), PROBMP(13), RELIAB(13), SNDB(13), SNPR(13),
     C SNRLW(13), SNRUP(13), SPROB(13), VHIGH(13), RNEFF(13),MDL(13)
       CHARACTER MDL*1
      COMMON /DUDL_NOIS/ DU_NOIS(13),DL_NOIS(13)
      COMMON / allMODE /ABPS(20),CREL(20),FLDST(20),HN(20),HP(20),
     1PROB(20),RELY(20),RGAIN(20),SIGPOW(20),SN(20),
     2SPRO(20),TGAIN(20),TIMED(20),TLOSS(20),B(20),FSLOS(20),
     3grlos(20),adv(20),obf(20),
     CNMODE(20),TLLOW(20),TLHGH(20),EFF(20),NREL,NMMOD
      COMMON/INFORM/INFO,IHSHR,IHLNG
      common /sncom/ snxx(13)
      common /cgains/ gaint(13),gainr(13)    !  transmitter & receiver gains
      DIMENSION TME(10)
C.....NORMAL DISTRIBUTION
      DATA TME/ 0.0,0.1257,0.2533, 0.3853,0.5244,0.6745,0.8416,1.0364,
     A 1.2815,1.6449/
      DATA XEPS/0.05/
      INUM=NMMOD
C.....IF NO MODES RETURN...............................................
      IF(INUM.LE.0)RETURN
C.....BEGINNING OF RELIABILITY CALCULATION FOR EACH MODE
C.....USED TO SELECT 1 OF UP TO 20 MODES FOR EACH FREQ
      DU2=DU*DU
      DL2=DL*DL
      DO 310 IM = 1,INUM
      IF( HP(IM).le. 70. ) then
         CREL(IM) = 0.001
         RELY(IM) = 0.001
      else
         IS = NMODE(IM)
         DSLF = TLLOW(IM)
         DSUF = TLHGH(IM)
C.....REQUIRED SIGNAL TO NOISE DISTRIBUTION
c%lc:gsp 28-DEC-1994:1st change, subroutine RELBIL *********************
c%lc    D10R is the lower distribution variable for the snr; values of snr
c%lc    at this end of the snr distribution reflect high noise & low signal.
c%lc    As such, D10R = f(DU2, DSLF).  Similarly, D90R = f(DL2, DSUF)
         D10R = SQRT(DU2 + DSLF*DSLF)
         D50R = SN(IM)
         D90R = SQRT( DL2 + DSUF*DSUF )
c%lc        D10R = SQRT(DL2 + DSLF*DSLF)
c%lc        D50R =  SN(IM)
c%lc        D90R = SQRT( DU2 + DSUF*DSUF )
c%lc:gsp 28-DEC-1994:end change ***************************************
         Z =  RSN - D50R
         IF(Z.le.0.) then
            Z = Z/(D10R/1.28)
         else
            Z = Z/(D90R/1.28)
         end if
         RELY(IM) = 1. - FNORML(Z)
C.....CREL IS NOT USED NOW
         CREL(IM) =  1000.
      end if
  310 CONTINUE
C
C.....END OF RELIABILITY CALCULATION FOR EACH MODE
C  MOST RELIABLE  MODE
C
      IRmethod=1       !  original - if RELs are within .05 of each other
ccc      IRmethod=2       !  if RELs are within 5% of each other
ccc      IRmethod=3       !  original, but if MRM REL is < .01, use mode with max REL
      IRmax=1
c          the original way of looking for the MRM in the 140 loop could
c          produce an improper mode.
c          Thus, first find the max REL mode
      do 10 im=2,inum
      if(RELY(IM).gt.RELY(IRmax)) IRmax=IM     !  IRmax will be the mode with max REL
10    continue

      IR=IRmax
      XREL = RELY(IR)
      XHN  = HN(IR)
      XSN  = SN(IR)
C.....IF ONLY ONE MODE USE IT........................................
ccc      write(luo,311) ir,rely(ir),hn(ir),sn(ir)
      DO 140 IM= 1,INUM
      if(im.eq.IRmax) go to 140       !  don't compare this mode
ccc      write(luo,311) im,rely(im),hn(im),sn(im)
ccc311   format('mode=',i5,3f10.5)
C.....MAKE SELECTION BASED ON RELIABILITY FIRST BUT IF CLOSE SELECT ON
C.....LOWER NUMBER OF HOPS (IF THE NUMBER OF HOPS ARE EQUAL SELECT BY
C.....MEDIAN SNR)
      if(xrel.lt..00000001) xrel=.00000001
      RELtest=ABS(RELY(IM)-XREL)
      if(IRmethod.eq.2) RELtest=RELtest/XREL        !  use percentage
      IF(RELtest .GE. XEPS ) go to 140              !  REL not close to max REL
C.......CLOSE SO TEST IF NUMBER OF HOPS ARE EQUAL.....................
        IF(ABS(XHN - HN(IM)).LE.XEPS)THEN
C.........NUMBER OF HOPS ARE EQUAL SO TEST MEDIAN SNR.................
          IF( XSN.LT.SN(IM) )THEN
            GO TO 139
          ENDIF
        ELSE IF(XHN.GT.HN(IM))THEN
C.........THIS ONE HAS FEWER HOPS.....................................
          GO TO 139
        ENDIF
      GO TO 140
C.....THIS MODE IS BETTER SO TRY IT...................................
  139 IR = IM
      XHN=HN(IM)
      XSN=SN(IM)
      XREL = RELY(IM)
  140 CONTINUE
c          if REL is small (<.01) use mode with highest REL
      if(IRmethod.eq.3 .and. RELY(IR).lt..01) IR=IRmax
      NREL = IR
      IS = NMODE(IR)
      MR=HN(IR)
ccc      write(luo,141) nrel
ccc141   format('nrel=',i5)
      IF(INUM.eq.1)THEN
C.......ONLY ONE MODE SO SET MOST RELIABLE VALUES....................
        RELIAB(IF) = RELY(IR)
        DBLOSL(IF) = TLLOW(IR)
        DBLOSU(IF) = TLHGH(IR)
        DBU(IF) = FLDST(IR)
        SNDB(IF) = SN(IR)
ccc        write(12,'('' in relbil, sndb('',i2,'')=sn('',i2,'')='',f9.3)')
ccc     +       if,ir,sndb(if)
        DBW(IF) = SIGPOW(IR)
      ELSE
C.......ADD THE SIGNALS RANDOM PHASE i.e. ADD THE POWERS IN WATTS....
        XDSLW = 0.0
        XSIGS = 0.0
        XDSUP = 0.0
        XFLD = 0.
        DXSIGS=-1000.
        DXFLD=-1000.
        DXDSLW=-1000.
        DXDSUP=-1000.
        DO 369 IV=1,INUM
        DXSIGS=AMAX1(DXSIGS,SIGPOW(IV))
        DXFLD=AMAX1(DXFLD,FLDST(IV))
        DXDSLW=AMAX1(DXDSLW,SIGPOW(IV)-TLLOW(IV))
        DXDSUP=AMAX1(DXDSUP,SIGPOW(IV)+TLHGH(IV))
  369   CONTINUE
        DO 370 IM = 1,INUM
        ZEXP = .1*(SIGPOW(IM) - TLLOW(IM)-DXDSLW)
        if(ZEXP.gt.-10.) XDSLW = XDSLW + 10. ** ZEXP
        ZEXP = .1*(SIGPOW(IM)-DXSIGS)
        if(ZEXP.gt.-10.) XSIGS = XSIGS +10. ** ZEXP
        ZEXP = .1 * (SIGPOW(IM) + TLHGH(IM)-DXDSUP)
        if(ZEXP.gt.-10.) XDSUP = XDSUP + 10. ** ZEXP
C  MUST DO FIELD STRENGTH SEPARATE  BECAUSE OF RECEIVE ANTENNA
        ZEXP = .1*(FLDST(IM)-DXFLD)
        if(ZEXP.gt.-10.) XFLD = XFLD + 10.**ZEXP
  370   CONTINUE
	SIGMED=-500.
        if(XSIGS.gt.0.) SIGMED = DXSIGS+ 10.*ALOG10(XSIGS)
	if(XDSLW.gt.0.) then
           DBLOSL(IF) = ABS( SIGMED - 10.*ALOG10(XDSLW)-DXDSLW )
        else
	   DBLOSL(IF)=0.
        end if
        if(DBLOSL(IF).lt. .2) DBLOSL(IF)=.2    !  set min GRH 8/21/02
        if(DBLOSL(IF).gt.30.) DBLOSL(IF)=30.   !  set max GRH 8/21/02
	if(XDSUP.gt.0.) then
           DBLOSU(IF) = ABS( DXDSUP+10.*ALOG10(XDSUP) - SIGMED )
        else
	   DBLOSU(IF)=0.
        end if
        if(DBLOSU(IF).lt. .2) DBLOSU(IF)=.2    !  set min GRH 8/21/02
        if(DBLOSU(IF).gt.30.) DBLOSU(IF)=30.   !  set max GRH 8/21/02
        DBW(IF) = SIGMED
        DELSIG = SIGMED - SIGPOW(IR)
	if(XFLD.gt.0.) then
           DBU(IF) = DXFLD + 10.*ALOG10(XFLD)
        else
	   DBU(IF)=-500.
	end if
        SNDB(IF) = SN(IR) + DELSIG
      if(prob(ir).lt..0001) then      !  MUFday says freq cannot work
ccc         dbu(if)=-500.
ccc         sndb(if)=-500.
ccc         sn(if)=-500.
      end if
ccc        write(12,'('' in relbil, sndb('',i2,'')=sn('',i2,'')='',2f9.3)')
ccc     +       if,ir,sndb(if),delsig
C.......REDO RELIABILITY FOR SUM OF MODES............................
c%lc:gsp 22-FEB-1995:change missed earlier, subroutine RELBIL ***************
        D10R = SQRT( DU2 + DBLOSL(IF)*DBLOSL(IF) )
        D50R = SNDB(IF)
        D90R = SQRT( DL2 + DBLOSU(IF)*DBLOSU(IF) )
c%lc:        D10R = SQRT( DL2 + DBLOSL(IF)*DBLOSL(IF) )
c%lc:        D50R = SNDB(IF)
c%lc:        D90R = SQRT( DU2 + DBLOSU(IF)*DBLOSU(IF) )
c%lc:gsp 28-DEC-1994:end change ***************************************
        if(D10R.lt. .2) D10R=.2
        if(D90R.gt.30.) D90R=30.
        Z = RSN - D50R
        IF(Z.le.0.) then
           Z = Z/(D10R/1.28)
        else
           Z = Z/(D90R/1.28)
        end if
        RELIAB(IF) = 1. - FNORML(Z)
      ENDIF
ccc      write(luo,777) if,reliab(if)
ccc777   format('if=',i5,'     REL=',f10.6)
      gaint(if)=TGAIN(IR)              !  save transmitter gain
      gainr(if)=RGAIN(IR)              !  save receiver    gain
      SNRLW(IF) = D10R
      SNRUP(IF) = D90R
      ANGLE(IF) =  B(IR)
      VHIGH(IF)=HP(IR)
      DELAY(IF)=TIMED(IR)
      DBLOS (IF)= TLOSS(IR)
c         new LOSS calculation 11/13/2014
c         since we have summed the signal powers, need to adjust transmission loss
      dblos(if)=pwrdb(freq)-dbw(if)     !  recalculate transmission loss
      CPROB(IF)=PROB(IR)
      MODE  (IF)= LAYTYP(IS)
      NHP   (IF)= HN(IR)
      XNYNOIS(IF)= RCNSE
      DU_NOIS(IF)=DU
      DL_NOIS(IF)=DL
C------------    ADDED 9/24/91  (LONG PATH RCVR EFF CORRECTION)
      RNEFF(IF)=EFF(IR)
ccc      write(luo,'(''eff='',2f10.3,2i5)') eff(ir),rgain(ir),ir,if
C  REQUIRED POWER GAIN  FOR SPECIFIED RELIABILITY.
      ITM = IABS( (LUFP - 50))/5 +1
      ITM = MIN0(ITM,10)
      TMX = TME(ITM)/TME(9)
      IF( LUFP.LT.50) THEN
         SNPR(IF) = - (D50R + TMX * D90R) + RSN
      ELSE
         SNPR(IF) = - (D50R - TMX * D10R) + RSN
      ENDIF
ccc      sn90(if)=d50r - d10r             !  lower decile (90%)
c          any % decile created 14.NOV.94
      snxx(if)=RSN     - SNPR(IF)     !  xx decile (xx%)
      IF(IAND(INFO,1).GT.0)THEN
        WRITE(99,'(29X,A,I4,A2)')'MOST RELIABLE MODE',NHP(IF),MODE(IF)
      ENDIF
      RETURN
      END
C--------------------------------

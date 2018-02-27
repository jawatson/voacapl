c###curmuf.for
      SUBROUTINE CURMUF
c***********************************************
c          Alex Shovkoplyas made some changes to:
c             curmuf
c             seltxr
c             settxr
c             sigdis
c          Normally if the version (..\database\version.w32)
c             is yy.mmddW then the original version is used.
c                yy.mmddA then Alex's version is used.
c                yy.mmdda then Alex's version is used with IONCAP absorption.
c***********************************************
      common /CVERSN/ VERSN
      character VERSN*8
      if(versn(8:8).eq.'A' .or. versn(8:8).eq.'a') then
         call curmuf_alex       !  use Alex's modified version
      else
         call curmuf_orig       !  use original version
      end if
      return
      end
C--------------------------------
      SUBROUTINE CURMUF_orig
C--------------------------------
C
C     THIS ROUTINE CALCULATES MUF FOR ALL LAYERS AND CIRCUIT MUF (NO ES)
C     MUF FACTORS ARE COMPUTED USING COMPLETE ELECTRON DENSITY PROFILES
C
      COMMON / MFAC / F2M3(5),HPF2(5),ZENANG(5),ZENMAX(5),IEDP,FSECV(3)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     A,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMUF
     B (4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     C ,YMUF(4)
      COMMON/ES/FS(3,5),HS(5)
      COMMON / GEOG / GYZ(5), RAT(5), GMDIP(5), CLCK(5), ABIY(5),
     1 ARTIC(5), SIGPAT(5), EPSPAT(5)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      COMMON/INFORM/INFO,IHSHR,IHLNG
      COMMON /CON / D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      DATA FQDEL/0.1/, NFQ/ 4/
      DATA AE/0.5/, A1/0.5/, A2/0.5/
      DATA DZF/2000./,BEX/9.5/
      KT =1
      IF(KFX -2) 100,105,110
C     RECEIVER SITE SAMPLE AREA
  100 KR =1
      GO TO 115
  105 KR =2
      GO TO 115
  110 KR =3
  115 CONTINUE
C  SELECT CONTROLING SAMPLE AREA.
      IF(KR -2) 125,120,130
  120 CONTINUE
      IF(FI(1,KT)- FI(1,KR))  125,125,135
  125 KS = KT
      GO TO 140
C    THE 0.01 MUST AGREE WITH SELMOD
  130 IF(ABS(FI(3,KT) - FI(3,KR)) - .01) 120,120,132
  132 IF(FI(3,KT)- FI(3,KR)) 125,120,135
  135 KS = KR
  140 CONTINUE
C  CALCULATE ELECTRON DENSITY PROFILE.
      CALL  LECDEN(KS)
C  TANGENT FREQUENCIES
      XTE = 1.0 / SQRT(1.0 + AE * YI(1,KS) / HI(1,KS))
      FXE = XTE*FI(1,KS)
      FX1 = 0.0
      IF(FI(2,KS)) 150,150,145
  145 XT1 = 1.0 / SQRT(1.0 + A1 * YI(2,KS) / HI(2,KS))
      FX1 = XT1*FI(2,KS)
  150 XT2 = 1.0 / SQRT(1.0 + A2 * YI(3,KS) / HI(3,KS))
      FX2 = XT2*FI(3,KS)
C FORCE F2MUF TO APPROACH MUF(0)
      IF( GCDKM- DZF ) 146,147,147
  146 A = -1. + 1./XT2
      BETA = 1.+ A* EXP(-BEX* GCDKM/DZF)
      FX2  = BETA * FX2
      XT2 = XT2 * BETA
  147 CONTINUE
C
C  ELAYER MUF
C
C  CALCULATE TRUE AND VIRTUAL HEIGHT
      IF( IEDP ) 155,160,160
  155 CALL GETHP(FXE,HPE,HTE)
      GO TO 165
C.....EQUIVALENT RESULT TO CALL OF SUBROUTINE GETHP FOR E LAYER
C.....PARAMETERS OF 110 AND 20
  160 HTE = 104.25
      HPE = 125.30
  165 CONTINUE
      DELE = AMAX1(0.0,AMIND)
      DEL  = DELE * D2R
      PHE=ASIN(RZ*COS(DEL)/(RZ+HPE))
      NHOPS = .5 * GCDKM / ((PIO2 - DEL - PHE) * RZ)
      PSI = GCDKM/(2.*RZ)
      XHOPS=NHOPS+1
      PSI=PSI/XHOPS
      CPSI = COS(PSI)
      SPSI = SIN(PSI)
      TANP = SPSI / (1. - CPSI + HPE / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ+ HTE)
      SECP = 1./SQRT(1.-SPHE*SPHE)
      EMUF(IT) = FXE*SECP
      DELE = DEL * R2D
C.....DISTRIBUTION FOR E LAYER MUF
      SIGL(1) = AMAX1(0.01,0.1*EMUF(IT) )
      SIGU(1) = SIGL(1)
C.....DELMUF(1) IS THE ANGLE FOR THE E LAYER
C.....HPMUF(1) IS THE VIRTUAL HEIGHT FOR THE E LAYER
C.....HTMUF(1) IS THE TRUE HEIGHT FOR THE E LAYER
C.....FVMUF(1) IS THE EQUIVALENT VERTICAL FREQUENCY FOR THE E LAYER
C.....AFMUF(1) IS THE DEVIATIVE LOSS FACTOR FOR THE E LAYER
C.....NHOPMF(1) IS THE NUMBER OF HOPS FOR THE E LAYER
C.....YFOT(1) IS THE LOWER DECILE FOR THE E LAYER
C.....YHPF(1) IS THE UPPER DECILE FOR THE E LAYER
C.....YMUF(1) IS THE MEDIAN FOR THE E LAYER
C.....NOTE THAT THE SAME VARIABLES WITH INCREASING SUBSCRIPTS DENOTE THE
C.....E LAYER (SUBSCRIPT=1), F1 LAYER (SUBSCRIPT=2), F2 LAYER
C.....(SUBSCRIPT=3), AND ES SPORADIC E LAYER (SUBSCRIPT=4)
      DELMUF(1)= DELE
      HPMUF(1) = HPE
      HTMUF(1) = HTE
      FVMUF(1) = FXE
      AFMUF(1) = 0.0
      NHOPMF(1)= XHOPS
      YFOT(1) = EMUF(IT) - 1.28*SIGL(1)
      YHPF(1) = EMUF(IT) + 1.28*SIGU(1)
      YMUF(1) = EMUF(IT)
C
C F2  LAYER MUF
C
C CALCULATE TRUE AND VIRTUAL HEIGHTS
      IF(IEDP) 170,175,175
  170 CALL GETHP(FX2,HP2,HT2)
      GO TO 180
C.....PARABOLIC E AND F2 LAYERS
  175 HT2 = HI(3,KS) - YI(3,KS) + YI(3,KS)*(1.-SQRT(1.-XT2*XT2))
      IF(FI(2,KS)) 176, 176, 170
  176 HP2 = HI(3,KS) - YI(3,KS) + BENDY(3,KS,FX2)
     A       +( PEN(1,KS,FX2) - 2.*YI(1,KS) )
  180 CONTINUE
      DEL2 = AMAX1(0.0,AMIND)
      DEL = DEL2 * D2R
      PHE=ASIN(RZ*COS(DEL)/(RZ+HP2))
      NHOPS = .5 * GCDKM / ((PIO2 - DEL - PHE) * RZ)
      XHOPS=NHOPS+1
      PSI=GCDKM/((2.*RZ)*XHOPS)
      CPSI = COS(PSI)
      SPSI = SIN(PSI)
C  DEL2 IS TOO SMALL.
      TANP = SPSI / (1. - CPSI + HP2 / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ+ HT2)
      SECP = 1./SQRT(1.-SPHE*SPHE)
      FOB1 = FX2 * SECP
      NTRY = 0
      FOB2 = FOB1
      XHP = (HP2 - HT2) / RZ
c-------------fjr-------------------------begin iteration for F2MUF, HPX2
c-------------fjr----------allows 4 tries to obtain epsilon of 0.1
  190 CONTINUE
      FOB1 = FOB2
C.....CORRECTION TO MARTYN"S THEOREM
      XMUT = SPHE*SPHE
      XFSQ=  FOB1*FOB1/( FI(3,KS)*FI(3,KS) )
      SPH = XFSQ *XMUT*XHP*(HT2 + 2.*(RZ + HT2)*XHP)
c.....SPH=amin1(SPH,60.)
C  CORRECTED VIRTUAL HEIGHT.
      HPX2 = HP2 + SPH
      TANP = SPSI / (1. - CPSI + HPX2 / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ+ HT2)
      SECP = 1./SQRT(1.-SPHE*SPHE)
      FOB2 = FX2* SECP
      HPX1 = HPX2
      NTRY = NTRY +1
      IF(IAND(INFO,8).GT.0)THEN
        WRITE(99,'(A,I2,3(A,F8.2))')' "CURMUF"  F2-LAYER  NTRY=',NTRY,
     1  '  HP=',HP2,'  MARTYN CORR=',SPH,'  FOB=',FOB2
      ENDIF
C.....CORRECTION TO HPX2 IS SUFFICIENT
      IF( ABS(FOB2-FOB1) - FQDEL )  200,200,195
  195 IF(NFQ - NTRY)  200,190,190
c-------------------------fjr---------------end iteration for F2MUF, HPX2
  200 F2MUF(IT) = FOB2
      DEL2 = DEL * R2D
C.....MULTIPLY BY ANY VALUE LESS THAN ONE
      FREQ = .9* F2MUF(IT)
C.....OBTAIN THE F2 MUF DISTRIBUTION FROM THE F2 M(3000) TABLES
      DUMMY1 = F2MUF(IT)
      DUMMY2 = CLAT(KS)
      DUMMY3 = CLCK(KS)
      CALL F2DIS(DUMMY1,SSN,DUMMY2,FREQ,DUMMY3,SIG)
      SIGL(3) = AMAX1( 0.01,SIG)
      FREQ = 1.1 * F2MUF(IT)
      DUMMY1 = F2MUF(IT)
      DUMMY2 = CLAT(KS)
      DUMMY3 = CLCK(KS)
      CALL F2DIS(DUMMY1,SSN,DUMMY2,FREQ,DUMMY3,SIG)
      SIGU(3) = AMAX1(0.01, SIG)
      DELMUF(3)= DEL2
      HPMUF(3) = HPX2
      HTMUF(3) = HT2
      FVMUF(3) = FX2
      AFMUF(3) = 0.0
      NHOPMF(3) = XHOPS
      YFOT(3) = F2MUF(IT) - 1.28 *SIGL(3)
      YMUF(3)  = F2MUF(IT)
      YHPF(3) = F2MUF(IT) + 1.28 *SIGU(3)
      IF( FI(2,KS) ) 210,210,215
C.....SET F1 TO E WHEN THERE IS NO F1 PRESENT
  210 F1MUF(IT) = EMUF(IT)
      SIGL(2)  = +SIGL(1)
      SIGU(2)  = +SIGU(1)
      DELMUF(2) = DELMUF(1)
      HPMUF(2) =  HPMUF(1)
      HTMUF(2) =  HTMUF(1)
      FVMUF(2) =  FVMUF(1)
      AFMUF(2) =  AFMUF(1)
      NHOPMF(2)=  NHOPMF(1)
      YFOT(2)  =  YFOT(1)
      YMUF(2)  = YMUF(1)
      YHPF(2)  =  YHPF(1)
      GO TO 240
C
C  F1 LAYER MUF
C
  215 CONTINUE
      CALL GETHP(FX1,HP1,HT1)
      DEL1 = AMAX1(0.0,AMIND)
      DEL  = DEL1 * D2R
      PHE=ASIN(RZ*COS(DEL)/(RZ+HP1))
      NHOPS = .5 * GCDKM / ((PIO2 - DEL - PHE) * RZ)
      PSI = GCDKM/(2.*RZ)
      XHOPS=NHOPS+1
      PSI=PSI/XHOPS
      CPSI= COS(PSI)
      SPSI = SIN(PSI)
      TANP = SPSI / (1. - CPSI + HP1 / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ+ HT1)
      SECP = 1./SQRT(1.-SPHE*SPHE)
      FOB1 = FX1 * SECP
      NTRY = 0
      FOB2 = FOB1
      XHP = (HP1 - HT1) / RZ
c-------------fjr-------------------------begin iteration for F1MUF, HPF1
c-------------fjr----------allows 4 tries to obtain epsilon of 0.1
  225 CONTINUE
      FOB1 = FOB2
      XFSQ = FOB1*FOB1 / ( FI(2,KS)*FI(2,KS)  )
      XMUT = SPHE*SPHE
      SPH = XFSQ*XMUT*XHP*(HT1 + 2.*(RZ + HT1)*XHP)
c.....SPH=amin1(SPH,60.)
      HPY2 = HP1 + SPH
      TANP = SPSI / (1. - CPSI + HPY2 / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ + HT1)
      SECP = 1. / SQRT(1. - SPHE * SPHE)
      FOB2 = FX1 *SECP
      NTRY = NTRY +1
      HPY1 = HPY2
      IF(IAND(INFO,8).GT.0)THEN
        WRITE(99,'(A,I2,3(A,F8.2))')' "CURMUF"  F1-LAYER  NTRY=',NTRY,
     1  '  HP=',HP1,'  MARTYN CORR=',SPH,'  FOB=',FOB2
      ENDIF
      IF( ABS(FOB2-FOB1) - FQDEL)  235,235,230
  230 IF(NFQ - NTRY)  235,225,225
c-------------------------fjr---------------end iteration for F1MUF, HPF1
  235 F1MUF(IT) = FOB2
      DEL1 = DEL * R2D
      SIGL(2) = AMAX1(0.01, 0.1*F1MUF(IT) )
      SIGU(2) = SIGL(2)
      DELMUF(2)= DEL1
      HPMUF(2) = HPY2
      HTMUF(2) = HT1
      FVMUF(2) = FX1
      AFMUF(2) = 0.0
      NHOPMF(2)= XHOPS
      YFOT(2) = F1MUF(IT) - 1.28*SIGL(2)
      YMUF(2) = F1MUF(IT)
      YHPF(2) = F1MUF(IT) + 1.28*SIGU(2)
  240 CONTINUE
C
C  SPORADIC E MUF, PROB. OF REFLECTION = 0.5.
C
      DELS = AMAX1(0.0, AMIND)
      ESMUF(IT) = 1000.
      DO 270 K=1,KM
      IF(FS(2,K)) 270,270,260
  260 CONTINUE
      DEL = DELS * D2R
      PHE = ASIN(RZ * COS(DEL) / (RZ + HS(K)))
      NHOPS = .5 * GCDKM / ((PIO2 - DEL - PHE) * RZ)
      HOP = NHOPS + 1
      PSI  = 0.5* GCD/HOP
      TDEL = (COS(PSI) - RZ/(RZ+HS(K)))/ SIN(PSI)
      CDEL = 1. / SQRT(1. + TDEL * TDEL)
      SPHE = RZ * CDEL / (RZ + HS(K))
      SECP = 1. / SQRT(1. - SPHE * SPHE)
      ESD = FS(2,K) * SECP
      IF( ESMUF(IT) -ESD)  270,270,265
  265 ESMUF(IT) = ESD
      KSX = K
      TDELX = TDEL
      SECPX =SECP
  270 CONTINUE
      IF(ESMUF(IT) - 1000.) 272,271,271
  271 DELMUF(4) = 0.0
       HPMUF(4) = 0.0
      HTMUF(4) = 0.0
       FVMUF(4) = 0.0
       AFMUF(4) = 0.0
      NHOPMF(4) = 0.0
       YFOT(4) = 0.0
      YMUF(4) = 0.0
       YHPF(4) = 0.0
       SIGL(4) = 0.0
       SIGU(4) = 0.0
       ESMUF(IT) = 0.0
       GO TO 274
  272 CONTINUE
      DELMUF(4) = ATAN(TDELX) * R2D
      HPMUF(4) = HS(KSX)
      HTMUF(4) = HPMUF(4)
      FVMUF(4) = FS(2,KSX)
      AFMUF(4) = 0.0
      NHOPMF(4) = HOP
      YMUF(4) = ESMUF(IT)
C.....FS IS ES OR E CRITICAL FREQUENCY
      XES = FI(1,KS) - FS(1,KSX)
      ZDIF = 0.1
      FZERO = 0.1
C.....UPPER STANDARD DEVIATION (LOWER FROM MAPS)
      SIGU(4) = ( FS(3,KSX) - FS(2,KSX))/ 1.28
      SIGB    = ( FS(2,KSX) - FS(1,KSX))/ 1.28
      SIGZ    = ( FS(2,KSX) - FZERO)/3.1
C.....MAXIMUM LOWER - WANT PROB .GT. .001 FOR F .GT. FZERO
      IF( XES +ZDIF) 315,320,320
C.....IF LOWER DECILE IS A REGULAR E USE THE UPPER DECILE
  315 SIGL(4) = SIGB
      GO TO 335
  320 IF(XES - ZDIF)  325,330,330
  325 SIGL(4) = SIGB + ((XES+ZDIF)/(2.* ZDIF)) * (SIGU(4) - SIGB)
      GO TO 335
  330 SIGL(4) = SIGU(4)
C.....IF UPPER DECILE IS TO LARGE RESET TO LOWER DECILE
  335 SIGL(4) = AMIN1(SIGL(4), SIGZ)
      FS(1,KSX) = FS(2,KSX) - 1.28* SIGL(4)
C NOW DO  OBLIQUE CASE
      SIGL(4) = SIGL(4)* SECPX
      SIGU(4) = SIGU(4) * SECPX
      YFOT(4) = ESMUF(IT) - 1.28 * SIGL(4)
      YHPF(4) = ESMUF(IT) + 1.28 * SIGU(4)
  274 CONTINUE
C
C  CIRCUIT MUF
C
C.....NOTE THAT ES IS NOT INCLUDED HERE
      FOT(IT) = AMAX1(YFOT(1),YFOT(2),YFOT(3) )
      ALLMUF(IT)  = AMAX1(EMUF(IT),F1MUF(IT), F2MUF(IT) )
      HPF(IT)    = AMAX1(YHPF(1),YHPF(2),YHPF(3))
      IF(EMUF(IT)- ALLMUF(IT))  280,275,275
  275 ANGMUF(IT) = DELMUF(1)
      MODMUF = 1
      GO TO 295
  280 IF(F1MUF(IT) - ALLMUF(IT))  290,285,285
  285 ANGMUF(IT) = DELMUF(2)
      MODMUF = 2
      GO TO 295
  290 ANGMUF(IT) = DELMUF(3)
      MODMUF = 3
  295 CONTINUE
      RETURN
      END
C--------------------------------
c###curmuf.for
      SUBROUTINE CURMUF_alex
C--------------------------------
C
C     THIS ROUTINE CALCULATES MUF FOR ALL LAYERS AND CIRCUIT MUF (NO ES)
C     MUF FACTORS ARE COMPUTED USING COMPLETE ELECTRON DENSITY PROFILES
C
      COMMON / MFAC / F2M3(5),HPF2(5),ZENANG(5),ZENMAX(5),IEDP,FSECV(3)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     A,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMUF
     B (4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     C ,YMUF(4)
      COMMON/ES/FS(3,5),HS(5)
      COMMON / GEOG / GYZ(5), RAT(5), GMDIP(5), CLCK(5), ABIY(5),
     1 ARTIC(5), SIGPAT(5), EPSPAT(5)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      COMMON/INFORM/INFO,IHSHR,IHLNG
      COMMON /CON / D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      DATA FQDEL/0.1/, NFQ/ 4/
      DATA AE/0.5/, A1/0.5/, A2/0.5/
      DATA DZF/2000./,BEX/9.5/
c>>>>>A.S.
      DIMENSION LX(3)
      DATA LX/2,3,4/
c<<<<<       
      KT =1
      IF(KFX -2) 100,105,110
C     RECEIVER SITE SAMPLE AREA
  100 KR =1
      GO TO 115
  105 KR =2
      GO TO 115
  110 KR =3
  115 CONTINUE
C  SELECT CONTROLING SAMPLE AREA.
      IF(KR -2) 125,120,130
  120 CONTINUE
      IF(FI(1,KT)- FI(1,KR))  125,125,135
  125 KS = KT
      GO TO 140
C    THE 0.01 MUST AGREE WITH SELMOD
  130 IF(ABS(FI(3,KT) - FI(3,KR)) - .01) 120,120,132
  132 IF(FI(3,KT)- FI(3,KR)) 125,120,135
  135 KS = KR
  140 CONTINUE
C  CALCULATE ELECTRON DENSITY PROFILE.
      CALL  LECDEN(KS)
C  TANGENT FREQUENCIES
      XTE = 1.0 / SQRT(1.0 + AE * YI(1,KS) / HI(1,KS))
      FXE = XTE*FI(1,KS)
      FX1 = 0.0
      IF(FI(2,KS)) 150,150,145
  145 XT1 = 1.0 / SQRT(1.0 + A1 * YI(2,KS) / HI(2,KS))
      FX1 = XT1*FI(2,KS)
  150 XT2 = 1.0 / SQRT(1.0 + A2 * YI(3,KS) / HI(3,KS))
      FX2 = XT2*FI(3,KS)
C FORCE F2MUF TO APPROACH MUF(0)
      IF( GCDKM- DZF ) 146,147,147
  146 A = -1. + 1./XT2
      BETA = 1.+ A* EXP(-BEX* GCDKM/DZF)
      FX2  = BETA * FX2
      XT2 = XT2 * BETA
  147 CONTINUE
C
C  ELAYER MUF
C
C  CALCULATE TRUE AND VIRTUAL HEIGHT
      IF( IEDP ) 155,160,160
  155 CALL GETHP(FXE,HPE,HTE)
      GO TO 165
C.....EQUIVALENT RESULT TO CALL OF SUBROUTINE GETHP FOR E LAYER
C.....PARAMETERS OF 110 AND 20
  160 HTE = 104.25
      HPE = 125.30
  165 CONTINUE
      DELE = AMAX1(0.0,AMIND)
      DEL  = DELE * D2R
      PHE=ASIN(RZ*COS(DEL)/(RZ+HPE))
      NHOPS = .5 * GCDKM / ((PIO2 - DEL - PHE) * RZ)
      PSI = GCDKM/(2.*RZ)
      XHOPS=NHOPS+1
      PSI=PSI/XHOPS
      CPSI = COS(PSI)
      SPSI = SIN(PSI)
      TANP = SPSI / (1. - CPSI + HPE / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ+ HTE)
      SECP = 1./SQRT(1.-SPHE*SPHE)
      EMUF(IT) = FXE*SECP
      DELE = DEL * R2D
C.....DISTRIBUTION FOR E LAYER MUF
      SIGL(1) = AMAX1(0.01,0.1*EMUF(IT) )
      SIGU(1) = SIGL(1)
C.....DELMUF(1) IS THE ANGLE FOR THE E LAYER
C.....HPMUF(1) IS THE VIRTUAL HEIGHT FOR THE E LAYER
C.....HTMUF(1) IS THE TRUE HEIGHT FOR THE E LAYER
C.....FVMUF(1) IS THE EQUIVALENT VERTICAL FREQUENCY FOR THE E LAYER
C.....AFMUF(1) IS THE DEVIATIVE LOSS FACTOR FOR THE E LAYER
C.....NHOPMF(1) IS THE NUMBER OF HOPS FOR THE E LAYER
C.....YFOT(1) IS THE LOWER DECILE FOR THE E LAYER
C.....YHPF(1) IS THE UPPER DECILE FOR THE E LAYER
C.....YMUF(1) IS THE MEDIAN FOR THE E LAYER
C.....NOTE THAT THE SAME VARIABLES WITH INCREASING SUBSCRIPTS DENOTE THE
C.....E LAYER (SUBSCRIPT=1), F1 LAYER (SUBSCRIPT=2), F2 LAYER
C.....(SUBSCRIPT=3), AND ES SPORADIC E LAYER (SUBSCRIPT=4)
      DELMUF(1)= DELE
      HPMUF(1) = HPE
      HTMUF(1) = HTE
      FVMUF(1) = FXE
      AFMUF(1) = 0.0
      NHOPMF(1)= XHOPS
      YFOT(1) = EMUF(IT) - 1.28*SIGL(1)
      YHPF(1) = EMUF(IT) + 1.28*SIGU(1)
      YMUF(1) = EMUF(IT)
C
C F2  LAYER MUF
C
C CALCULATE TRUE AND VIRTUAL HEIGHTS
      IF(IEDP) 170,175,175
  170 CALL GETHP(FX2,HP2,HT2)
      GO TO 180
C.....PARABOLIC E AND F2 LAYERS
  175 HT2 = HI(3,KS) - YI(3,KS) + YI(3,KS)*(1.-SQRT(1.-XT2*XT2))
      IF(FI(2,KS)) 176, 176, 170
  176 HP2 = HI(3,KS) - YI(3,KS) + BENDY(3,KS,FX2)
     A       +( PEN(1,KS,FX2) - 2.*YI(1,KS) )
  180 CONTINUE
      DEL2 = AMAX1(0.0,AMIND)
      DEL = DEL2 * D2R
      PHE=ASIN(RZ*COS(DEL)/(RZ+HP2))
      NHOPS = .5 * GCDKM / ((PIO2 - DEL - PHE) * RZ)
      XHOPS=NHOPS+1
      PSI=GCDKM/((2.*RZ)*XHOPS)
      CPSI = COS(PSI)
      SPSI = SIN(PSI)
C  DEL2 IS TOO SMALL.
      TANP = SPSI / (1. - CPSI + HP2 / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ+ HT2)
      SECP = 1./SQRT(1.-SPHE*SPHE)
      FOB1 = FX2 * SECP
      NTRY = 0
      FOB2 = FOB1
      XHP = (HP2 - HT2) / RZ
c-------------fjr-------------------------begin iteration for F2MUF, HPX2
c-------------fjr----------allows 4 tries to obtain epsilon of 0.1
  190 CONTINUE
      FOB1 = FOB2
C.....CORRECTION TO MARTYN"S THEOREM
      XMUT = SPHE*SPHE
      XFSQ=  FOB1*FOB1/( FI(3,KS)*FI(3,KS) )
      SPH = XFSQ *XMUT*XHP*(HT2 + 2.*(RZ + HT2)*XHP)
c.....SPH=amin1(SPH,60.)
C  CORRECTED VIRTUAL HEIGHT.
      HPX2 = HP2 + SPH
      TANP = SPSI / (1. - CPSI + HPX2 / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ+ HT2)
      SECP = 1./SQRT(1.-SPHE*SPHE)
      FOB2 = FX2* SECP
      HPX1 = HPX2
      NTRY = NTRY +1
      IF(IAND(INFO,8).GT.0)THEN
        WRITE(99,'(A,I2,3(A,F8.2))')' "CURMUF"  F2-LAYER  NTRY=',NTRY,
     1  '  HP=',HP2,'  MARTYN CORR=',SPH,'  FOB=',FOB2
      ENDIF
C.....CORRECTION TO HPX2 IS SUFFICIENT
      IF( ABS(FOB2-FOB1) - FQDEL )  200,200,195
  195 IF(NFQ - NTRY)  200,190,190
c-------------------------fjr---------------end iteration for F2MUF, HPX2
  200 F2MUF(IT) = FOB2
      DEL2 = DEL * R2D
C.....MULTIPLY BY ANY VALUE LESS THAN ONE
      FREQ = .9* F2MUF(IT)
C.....OBTAIN THE F2 MUF DISTRIBUTION FROM THE F2 M(3000) TABLES
      DUMMY1 = F2MUF(IT)
c>>>>>A.S.
      DUMMY2 = CLAT(LX(KS))
      DUMMY3 = CLCK(LX(KS))
c<<<<<       
      CALL F2DIS(DUMMY1,SSN,DUMMY2,FREQ,DUMMY3,SIG)
      SIGL(3) = AMAX1( 0.01,SIG)
      FREQ = 1.1 * F2MUF(IT)
      DUMMY1 = F2MUF(IT)
c<<<<<       
      DUMMY2 = CLAT(LX(KS))
      DUMMY3 = CLCK(LX(KS))
c<<<<<       
      CALL F2DIS(DUMMY1,SSN,DUMMY2,FREQ,DUMMY3,SIG)
      SIGU(3) = AMAX1(0.01, SIG)
      DELMUF(3)= DEL2
      HPMUF(3) = HPX2
      HTMUF(3) = HT2
      FVMUF(3) = FX2
      AFMUF(3) = 0.0
      NHOPMF(3) = XHOPS
      YFOT(3) = F2MUF(IT) - 1.28 *SIGL(3)
      YMUF(3)  = F2MUF(IT)
      YHPF(3) = F2MUF(IT) + 1.28 *SIGU(3)
      IF( FI(2,KS) ) 210,210,215
C.....SET F1 TO E WHEN THERE IS NO F1 PRESENT
  210 F1MUF(IT) = EMUF(IT)
      SIGL(2)  = +SIGL(1)
      SIGU(2)  = +SIGU(1)
      DELMUF(2) = DELMUF(1)
      HPMUF(2) =  HPMUF(1)
      HTMUF(2) =  HTMUF(1)
      FVMUF(2) =  FVMUF(1)
      AFMUF(2) =  AFMUF(1)
      NHOPMF(2)=  NHOPMF(1)
      YFOT(2)  =  YFOT(1)
      YMUF(2)  = YMUF(1)
      YHPF(2)  =  YHPF(1)
      GO TO 240
C
C  F1 LAYER MUF
C
  215 CONTINUE
      CALL GETHP(FX1,HP1,HT1)
      DEL1 = AMAX1(0.0,AMIND)
      DEL  = DEL1 * D2R
      PHE=ASIN(RZ*COS(DEL)/(RZ+HP1))
      NHOPS = .5 * GCDKM / ((PIO2 - DEL - PHE) * RZ)
      PSI = GCDKM/(2.*RZ)
      XHOPS=NHOPS+1
      PSI=PSI/XHOPS
      CPSI= COS(PSI)
      SPSI = SIN(PSI)
      TANP = SPSI / (1. - CPSI + HP1 / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ+ HT1)
      SECP = 1./SQRT(1.-SPHE*SPHE)
      FOB1 = FX1 * SECP
      NTRY = 0
      FOB2 = FOB1
      XHP = (HP1 - HT1) / RZ
c-------------fjr-------------------------begin iteration for F1MUF, HPF1
c-------------fjr----------allows 4 tries to obtain epsilon of 0.1
  225 CONTINUE
      FOB1 = FOB2
      XFSQ = FOB1*FOB1 / ( FI(2,KS)*FI(2,KS)  )
      XMUT = SPHE*SPHE
      SPH = XFSQ*XMUT*XHP*(HT1 + 2.*(RZ + HT1)*XHP)
c.....SPH=amin1(SPH,60.)
      HPY2 = HP1 + SPH
      TANP = SPSI / (1. - CPSI + HPY2 / RZ)
      PHE = ATAN(TANP)
      DEL = PIO2 - PHE - PSI
      CDEL = COS(DEL)
      SPHE = RZ*CDEL/(RZ + HT1)
      SECP = 1. / SQRT(1. - SPHE * SPHE)
      FOB2 = FX1 *SECP
      NTRY = NTRY +1
      HPY1 = HPY2
      IF(IAND(INFO,8).GT.0)THEN
        WRITE(99,'(A,I2,3(A,F8.2))')' "CURMUF"  F1-LAYER  NTRY=',NTRY,
     1  '  HP=',HP1,'  MARTYN CORR=',SPH,'  FOB=',FOB2
      ENDIF
      IF( ABS(FOB2-FOB1) - FQDEL)  235,235,230
  230 IF(NFQ - NTRY)  235,225,225
c-------------------------fjr---------------end iteration for F1MUF, HPF1
  235 F1MUF(IT) = FOB2
      DEL1 = DEL * R2D
      SIGL(2) = AMAX1(0.01, 0.1*F1MUF(IT) )
      SIGU(2) = SIGL(2)
      DELMUF(2)= DEL1
      HPMUF(2) = HPY2
      HTMUF(2) = HT1
      FVMUF(2) = FX1
      AFMUF(2) = 0.0
      NHOPMF(2)= XHOPS
      YFOT(2) = F1MUF(IT) - 1.28*SIGL(2)
      YMUF(2) = F1MUF(IT)
      YHPF(2) = F1MUF(IT) + 1.28*SIGU(2)
  240 CONTINUE
C
C  SPORADIC E MUF, PROB. OF REFLECTION = 0.5.
C
      DELS = AMAX1(0.0, AMIND)
      ESMUF(IT) = 1000.
      DO 270 K=1,KM
      IF(FS(2,K)) 270,270,260
  260 CONTINUE
      DEL = DELS * D2R
      PHE = ASIN(RZ * COS(DEL) / (RZ + HS(K)))
      NHOPS = .5 * GCDKM / ((PIO2 - DEL - PHE) * RZ)
      HOP = NHOPS + 1
      PSI  = 0.5* GCD/HOP
      TDEL = (COS(PSI) - RZ/(RZ+HS(K)))/ SIN(PSI)
      CDEL = 1. / SQRT(1. + TDEL * TDEL)
      SPHE = RZ * CDEL / (RZ + HS(K))
      SECP = 1. / SQRT(1. - SPHE * SPHE)
      ESD = FS(2,K) * SECP
      IF( ESMUF(IT) -ESD)  270,270,265
  265 ESMUF(IT) = ESD
      KSX = K
      TDELX = TDEL
      SECPX =SECP
  270 CONTINUE
      IF(ESMUF(IT) - 1000.) 272,271,271
  271 DELMUF(4) = 0.0
       HPMUF(4) = 0.0
      HTMUF(4) = 0.0
       FVMUF(4) = 0.0
       AFMUF(4) = 0.0
      NHOPMF(4) = 0.0
       YFOT(4) = 0.0
      YMUF(4) = 0.0
       YHPF(4) = 0.0
       SIGL(4) = 0.0
       SIGU(4) = 0.0
       ESMUF(IT) = 0.0
       GO TO 274
  272 CONTINUE
      DELMUF(4) = ATAN(TDELX) * R2D
      HPMUF(4) = HS(KSX)
      HTMUF(4) = HPMUF(4)
      FVMUF(4) = FS(2,KSX)
      AFMUF(4) = 0.0
      NHOPMF(4) = HOP
      YMUF(4) = ESMUF(IT)
C.....FS IS ES OR E CRITICAL FREQUENCY
      XES = FI(1,KS) - FS(1,KSX)
      ZDIF = 0.1
      FZERO = 0.1
C.....UPPER STANDARD DEVIATION (LOWER FROM MAPS)
      SIGU(4) = ( FS(3,KSX) - FS(2,KSX))/ 1.28
      SIGB    = ( FS(2,KSX) - FS(1,KSX))/ 1.28
      SIGZ    = ( FS(2,KSX) - FZERO)/3.1
C.....MAXIMUM LOWER - WANT PROB .GT. .001 FOR F .GT. FZERO
      IF( XES +ZDIF) 315,320,320
C.....IF LOWER DECILE IS A REGULAR E USE THE UPPER DECILE
  315 SIGL(4) = SIGB
      GO TO 335
  320 IF(XES - ZDIF)  325,330,330
  325 SIGL(4) = SIGB + ((XES+ZDIF)/(2.* ZDIF)) * (SIGU(4) - SIGB)
      GO TO 335
  330 SIGL(4) = SIGU(4)
C.....IF UPPER DECILE IS TO LARGE RESET TO LOWER DECILE
  335 SIGL(4) = AMIN1(SIGL(4), SIGZ)
      FS(1,KSX) = FS(2,KSX) - 1.28* SIGL(4)
C NOW DO  OBLIQUE CASE
      SIGL(4) = SIGL(4)* SECPX
      SIGU(4) = SIGU(4) * SECPX
      YFOT(4) = ESMUF(IT) - 1.28 * SIGL(4)
      YHPF(4) = ESMUF(IT) + 1.28 * SIGU(4)
  274 CONTINUE
C
C  CIRCUIT MUF
C
C.....NOTE THAT ES IS NOT INCLUDED HERE
      FOT(IT) = AMAX1(YFOT(1),YFOT(2),YFOT(3) )
      ALLMUF(IT)  = AMAX1(EMUF(IT),F1MUF(IT), F2MUF(IT) )
      HPF(IT)    = AMAX1(YHPF(1),YHPF(2),YHPF(3))
      IF(EMUF(IT)- ALLMUF(IT))  280,275,275
  275 ANGMUF(IT) = DELMUF(1)
      MODMUF = 1
      GO TO 295
  280 IF(F1MUF(IT) - ALLMUF(IT))  290,285,285
  285 ANGMUF(IT) = DELMUF(2)
      MODMUF = 2
      GO TO 295
  290 ANGMUF(IT) = DELMUF(3)
      MODMUF = 3
  295 CONTINUE
      RETURN
      END
C--------------------------------

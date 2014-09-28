c###fdist.for
      SUBROUTINE FDIST (IHOP,K,NUMMOD)
C--------------------------------
C
C     THIS ROUTINE FINDS UP TO SIX RAYSETS FOR THE CURRENT HOP DISTANCE
C     (SET IN SUBROUTINE LUFFY) AND FREQUENCY
C     LOOK FOR E, F1 AND F2 LOW ANGLES AND HIGH ANGLES
C
C     K IS THE SAMPLE AREA (USED AS AN INDEX)
C
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON/REFLX/DELFX(45,3),HPFLX(45,3),HTFLX(45,3),GDFLX(45,3),FVFLX
     A (45,3),DSKPKM(3),DELSKP(3),HPSKP(3),HTSKP(3),DMAXKM(3),FVSKP(3)
     B ,ISKP(3),IMODE(45,3),AFFLX(45,3),DELPEN(3,5),GML(45,3),FHP(45,3)
      COMMON /MODES /GHOP, DELMOD (6, 3), HPMOD (6, 3), HTMOD (6, 3), FV
     1MOD (6, 3), ITMOD (6, 3), AFMOD (6, 3)
      COMMON/INFORM/INFO,IHSHR,IHLNG
      character MODZ(6)*3,mlay(3)*3
      data mlay/'  E',' F1',' F2'/
      do 1 i=1,6
    1 MODZ(i)='   '
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C.....DHOP= HOP DISTANCE KM
      DHOPKM = GHOP*RZ
      DO 100 IM = 1, 6
C.....THIS INDICATES NO MODE
      HPMOD (IM, K) = - 1.
  100 ITMOD(IM,K) = 5
CRMFORT      IF (DHOPKM - DMAXKM (K))135, 230, 230
ccc      IF(DHOPKM+0.1.GT.DMAXKM(K)) GO TO 230
c    The code below was done to fix a problem of no modes being found.
c    This was done 1/10/97 by Greg Hand.  Apparently the original line
c    was commented out with CRMFORT.  I am assuming this was done by NRL to
c    solve some problem with Ryan McFarlan Fortran compiler.
      IF(DHOPKM.GE.DMAXKM(K)) GO TO 230
      IH = 0
      IL = 0
  140 IL = IL + 1
  145 IH = IH + 1
      IF(IH.gt.44) go to 230
      IF(HPFLX(IH+1,K).le.0.) go to 230
      IF(GDFLX(IH,K) - GDFLX(IH+1,K)) 170, 160, 215
ccc  160 IF(DHOPKM - GDFLX(IH, K)) 145, 165, 145
  160 IF(abs(DHOPKM-GDFLX(IH, K)).le..001) go to 145
  165 DELMOD (IL, K) = DELFX (IH, K)
      HPMOD (IL, K) = HPFLX (IH, K)
      HTMOD (IL, K) = HTFLX (IH, K)
      ITMOD (IL, K) = IMODE (IH, K)
      AFMOD (IL, K) = AFFLX (IH, K)
      FVMOD (IL, K) = FVFLX (IH, K)
      GO TO 305
  170 IF(GDFLX(IH,K) - DHOPKM) 175, 165, 145
  175 IF(DHOPKM - GDFLX(IH+1,K)) 180, 176, 145
  176 IH = IH + 1
      GO TO 165
  180 CONTINUE
      DTH2 = GDFLX (IH + 1, K) - GDFLX (IH, K)
      IF (ABS (DTH2) - 0.001)165, 165, 185
  185 CONTINUE
      DTH = DHOPKM  - GDFLX(IH,K)
C.....BEGINNING OF SECTION TO SEARCH IN TABLE, DO LINEAR INTERPOLATION
      THET = 0.5 * DHOPKM / RZ
      HP1 = HPFLX (IH, K)
      HP2 = HPFLX (IH + 1, K)
      HT1 = HTFLX (IH, K)
      HT2 = HTFLX (IH + 1, K)
      HP = HP1 + (HP2 - HP1) * DTH / DTH2
      HT = HT1 + (HT2 - HT1) * DTH / DTH2
      AFMOD(IL,K)=AFFLX(IH,K)+(AFFLX(IH+1,K)-AFFLX(IH,K))*DTH/DTH2
      HPMOD (IL, K) = HP
      ST = SIN(THET)
      TANP = ST/(1. - COS(THET)  + HP/RZ)
      PHE  = ATAN(TANP)
C.....BUT FORCE CORRECT GEOMETRY BY CALCULATING RADIATION ANGLE AND
C.....SNELL"S LAW BY CALCULATING FV
      DELMOD(IL,K) =  (PIO2 - PHE - THET ) * R2D
      SPHI = RZ * COS (DELMOD (IL, K) * D2R) / (RZ + HT)
      SPHI = 1. - SPHI * SPHI
      SPHI = AMAX1 (SPHI, 0.000001)
      FVMOD (IL, K) = FREQ * SQRT (SPHI)
      HTMOD (IL, K) = HT
      ITMOD (IL, K) = IMODE (IH, K)
C.....CHECK ON MINIMUM ANGLE AFTER INTERPOLATION
  305 IF(DELMOD(IL,K).ge.AMIND) go to 315
      HPMOD(IL,K) = - ABS(HPMOD(IL,K))
      go to 145        !  added from ICEPAC fdist
C.....END OF SECTION TO SEARCH IN TABLE, DO LINEAR INTERPOLATION
  315 IF (IL - 6)140, 230, 230
  215 IF(GDFLX(IH,K) - DHOPKM) 145, 165, 220
  220 IF(DHOPKM - GDFLX(IH+1,K)) 145, 176, 180
  230 CONTINUE
          NUMMOD=0
          do 236 ilm=1,6
          if(HPMOD(ilm,k).lt.0.)go to 236
          NUMMOD=NUMMOD+1
          MODZ(NUMMOD)=mlay(ITMOD(ilm,k))
  236     continue
          IF(IAND(INFO,1).GT.0)THEN
           WRITE(99,'(16h "FDIST"   Nhop=,I1,3x,6hHopkm=,f6.0,7h  Mods=,
     1      I1,3X,6A3)')IHOP,DHOPKM,NUMMOD,MODZ
          ENDIF
      RETURN
      END
C--------------------------------

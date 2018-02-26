c###seltxr.for
      SUBROUTINE SELTXR
c***********************************************
c          Alex Shovkoplyas made some changes to:
c             curmuf
c             seltxr
c             settxr
c             sigdis
c          Normally if the version (..\database\version.w32)
c             is yy.mmddW then the original version is used.
c                yy.mmddA then Alex's version is used.
c***********************************************
      common /CVERSN/ VERSN
      character VERSN*8
      if(versn(8:8).eq.'A' .or. versn(8:8).eq.'a') then
         call seltxr_alex       !  use Alex's modified version
      else
         call seltxr_orig       !  use original version
      end if
      return
      end
C--------------------------------
      SUBROUTINE SELTXR_orig
C--------------------------------
C------------ SUBROUTINES SELRCR AND SELTMT COMBINED  1/10/91 FJR
C
C     THIS SUBROUTINE SELECTS THE OPTIMUM TRANSMISSION ANGLE
C     (NOTE THAT THIS IS AT THE TRANSMITTER/RECEIVER ENDS)
C
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/LOSX/ANDVX(45,3),ADVX(45,3),AOFX(45,3),ARFX(45,3),GRLOSX(45
     A ,3),TGAINX(45,3),TLSKM(45,3),EFFlp(45),IAFTXR(3)
      COMMON/LPATH/ GCDLNG,TXRGML(45,2),DELOPT,GMIN,YMIN,LTXRGM(2)
      COMMON/REFLX/DELFX(45,3),HPFLX(45,3),HTFLX(45,3),GDFLX(45,3),FVFLX
     A (45,3),DSKPKM(3),DELSKP(3),HPSKP(3),HTSKP(3),DMAXKM(3),FVSKP(3)
     B ,ISKP(3),IMODE(45,3),AFFLX(45,3),DELPEN(3,5),GML(45,3),FHP(45,3)
      COMMON/INFORM/INFO,IHSHR,IHLNG
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      DO 1000 JJ=1,2
      K=ITXRCP(JJ)
      DEND = AMIN1(GCDKM,4000.)
      LTXRGM(JJ) = 1
C.....ONE MODE GOOD, AS OVER THE MUF FORCED
      GO TO 105
   95 LTXRGM(JJ) = LTXRGM(JJ)-1
      IF(IAND(INFO,8).GT.0)THEN
        IF(LTXRGM(JJ).GT.1)THEN
          WRITE(99,'(13h "SELTXR" JJ=,I1,27h  CHANGE IN MODE INDEX FROM,
     1    9h "1" TO ",I2,25H" USING "LTXRGM-1" VICE 1)')JJ,LTXRGM(JJ)
        ENDIF
      ENDIF
      GO TO 1000
C.....FIND FIRST GOOD MODE (START)
  105 XHPM = DEND / GDFLX(LTXRGM(JJ),K)
      IHOP = XHPM
      IF(XHPM - 0.9) 110, 115, 115
  110 LTXRGM(JJ) = LTXRGM(JJ)+1
      IF(HPFLX(LTXRGM(JJ),K).LE.70.)GO TO 95
C.....CHECK MINIMUM ANGLE
      IF(LTXRGM(JJ) - IAFTXR(K)) 112, 112, 95
  112 IF(DELFX(LTXRGM(JJ),K) - AMIND) 110, 115, 115
  115 HOPX = IHOP
      YHPM = XHPM- HOPX
c------      IF(YHPM -.05) 120,120,116
c------      Changed 9/23/91 to reflect proper parTIAL hop.   FJR
C------      IF(YHPM -.5) 120,120,116
      IST=2
      IDNXS=LTXRGM(JJ)
      IF(IAND(INFO,4).GT.0)IST=1
      DO 999 IDF=IST,2
      IF(IDF.EQ.2)THEN
        XFRACT=0.5
        LTXRGM(JJ)=IDNXS
      ELSE
        XFRACT=.05
      ENDIF
      IF(YHPM - XFRACT)120,120,116
C.....HOP INCREMENT
  116 YHPM = 1.-YHPM
C.....GAIN-LOSS
  120 GMAX =TGAINX(LTXRGM(JJ),K)-XHPM*(ANDVX(LTXRGM(JJ),K)
     +  +ADVX(LTXRGM(JJ),K))
C.....ANGLE INCREMENT
      DELMAX= ABS(DELFX(LTXRGM(JJ),K) - DELOPT)
C.....FIND FIRST GOOD MODE (END)
      LS = LTXRGM(JJ)
      DO 165  IA=LS,IAFTXR(K)
C.....DOES THE MODE EXIST (QUESTION MARK)
      IF(HPFLX(IA,K) - 70.) 165,165,125
C.....NUMBER OF HOPS
  125 XHOP = DEND / GDFLX(IA,K)
      IHOP = XHOP
C.....AT LEAST NEAR 1
      IF(XHOP - 0.9) 165, 126, 126
  126 CONTINUE
      HOPX = IHOP
      YHOP = XHOP - HOPX
      IF( YHOP - 0.5) 135,135,130
C.....HOP INCREMENT
  130 YHOP = 1.-YHOP
C.....GAIN-LOSS
  135 GNOW = TGAINX(IA,K) - XHOP * (ANDVX(IA,K) + ADVX(IA,K))
      GML(IA,K)=GNOW
      FHP(IA,K)=YHOP
C.....ANGLE INCREMENT
      DELNOW = ABS(DELFX(IA,K) - DELOPT)
C  FIRST CHOICE ON SMALLEST GAIN MINUS LOSS.
      IF(ABS(GNOW -GMAX) - GMIN) 145,145,140
  140 IF( GNOW - GMAX) 165,165,160
C SECOND CHOICE ON CLOSEST TO SPECULAR REFLECTION (INTEGER NO. OF HOPS).
  145 IF( ABS(YHOP -YHPM) -YMIN)  155,155,150
  150 IF( YHOP - YHPM)  160,165,165
C THIRD   CHOICE ON CLOSEST TO APRIORI OPTIMUM TAKEOFF ANGLE.
  155 IF( DELNOW -DELMAX) 160,165,165
  160 LTXRGM(JJ) = IA
      XHPM = XHOP
      GMAX = GNOW
      DELMAX = DELNOW
      YHPM   = YHOP
  165 CONTINUE
      IF(IST.EQ.1)THEN
        IF(IDF.EQ.1)THEN
          LSAV=LTXRGM(JJ)
        ELSE
          IF(LTXRGM(JJ).NE.LSAV)THEN
            WRITE(99,'(A,I2,A,A,I3,A,I3)')' "SELTXR" JJ= ',JJ,'  CHANG',
     1      'ING FROM 0.05 TO 0.5 CHANGED SELECT MODE FROM ',LSAV,' TO',
     2      LTXRGM(JJ)
          ENDIF
        ENDIF
      ENDIF
  999 CONTINUE
 1000 CONTINUE
      RETURN
      END
C--------------------------------
c###seltxr.for
      SUBROUTINE SELTXR_alex
C--------------------------------
C------------ SUBROUTINES SELRCR AND SELTMT COMBINED  1/10/91 FJR
C
C     THIS SUBROUTINE SELECTS THE OPTIMUM TRANSMISSION ANGLE
C     (NOTE THAT THIS IS AT THE TRANSMITTER/RECEIVER ENDS)
C
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/LOSX/ANDVX(45,3),ADVX(45,3),AOFX(45,3),ARFX(45,3),GRLOSX(45
     A ,3),TGAINX(45,3),TLSKM(45,3),EFFlp(45),IAFTXR(3)
      COMMON/LPATH/ GCDLNG,TXRGML(45,2),DELOPT,GMIN,YMIN,LTXRGM(2)
      COMMON/REFLX/DELFX(45,3),HPFLX(45,3),HTFLX(45,3),GDFLX(45,3),FVFLX
     A (45,3),DSKPKM(3),DELSKP(3),HPSKP(3),HTSKP(3),DMAXKM(3),FVSKP(3)
     B ,ISKP(3),IMODE(45,3),AFFLX(45,3),DELPEN(3,5),GML(45,3),FHP(45,3)
      COMMON/INFORM/INFO,IHSHR,IHLNG
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      DO 1000 JJ=1,2
      K=ITXRCP(JJ)
      DEND = AMIN1(GCDKM,4000.)
c>>>>>A.S.  
c     find the first good mode
c     at least one good mode is alvays available
c     finding it is easy since all bad modes now have HPFLX = 0
c     and all good modes have HPFLX > 0
      DO 310 IA=1,45
      IF(HPFLX(IA,K) .GE. 70.) THEN
        LTXRGM(JJ) = IA
        GOTO 320 !mode found, exit the loop
        ENDIF
310   CONTINUE      
320   XHPM = DEND / GDFLX(LTXRGM(JJ),K)
      IHOP = XHPM
c<<<<< 
  115 HOPX = IHOP
      YHPM = XHPM- HOPX
c------      IF(YHPM -.05) 120,120,116
c------      Changed 9/23/91 to reflect proper parTIAL hop.   FJR
C------      IF(YHPM -.5) 120,120,116
      IST=2
      IDNXS=LTXRGM(JJ)
      IF(IAND(INFO,4).GT.0)IST=1
      DO 999 IDF=IST,2
      IF(IDF.EQ.2)THEN
        XFRACT=0.5
        LTXRGM(JJ)=IDNXS
      ELSE
        XFRACT=.05
      ENDIF
      IF(YHPM - XFRACT)120,120,116
C.....HOP INCREMENT
  116 YHPM = 1.-YHPM
C.....GAIN-LOSS
  120 GMAX =TGAINX(LTXRGM(JJ),K)-XHPM*(ANDVX(LTXRGM(JJ),K)
     +  +ADVX(LTXRGM(JJ),K))
C.....ANGLE INCREMENT
      DELMAX= ABS(DELFX(LTXRGM(JJ),K) - DELOPT)
C.....FIND FIRST GOOD MODE (END)
      LS = LTXRGM(JJ)
      DO 165  IA=LS,IAFTXR(K)
C.....DOES THE MODE EXIST (QUESTION MARK)
      IF(HPFLX(IA,K) - 70.) 165,165,125
C.....NUMBER OF HOPS
  125 XHOP = DEND / GDFLX(IA,K)
      IHOP = XHOP
C.....AT LEAST NEAR 1
c>>>>>A.S. 
c      this line was causing problems and has been removed
c      no need to check XHOP 
c      since all bad modes now have HPFLX = 0
c      IF(XHOP - 0.9) 165, 126, 126
c<<<<< 
  126 CONTINUE
      HOPX = IHOP
      YHOP = XHOP - HOPX
      IF( YHOP - 0.5) 135,135,130
C.....HOP INCREMENT
  130 YHOP = 1.-YHOP
C.....GAIN-LOSS
  135 GNOW = TGAINX(IA,K) - XHOP * (ANDVX(IA,K) + ADVX(IA,K))
      GML(IA,K)=GNOW
      FHP(IA,K)=YHOP
C.....ANGLE INCREMENT
      DELNOW = ABS(DELFX(IA,K) - DELOPT)
C  FIRST CHOICE ON SMALLEST GAIN MINUS LOSS.
      IF(ABS(GNOW -GMAX) - GMIN) 145,145,140
  140 IF( GNOW - GMAX) 165,165,160
C SECOND CHOICE ON CLOSEST TO SPECULAR REFLECTION (INTEGER NO. OF HOPS).
  145 IF( ABS(YHOP -YHPM) -YMIN)  155,155,150
  150 IF( YHOP - YHPM)  160,165,165
C THIRD   CHOICE ON CLOSEST TO APRIORI OPTIMUM TAKEOFF ANGLE.
  155 IF( DELNOW -DELMAX) 160,165,165
  160 LTXRGM(JJ) = IA
      XHPM = XHOP
      GMAX = GNOW
      DELMAX = DELNOW
      YHPM   = YHOP
  165 CONTINUE
      IF(IST.EQ.1)THEN
        IF(IDF.EQ.1)THEN
          LSAV=LTXRGM(JJ)
        ELSE
          IF(LTXRGM(JJ).NE.LSAV)THEN
            WRITE(99,'(A,I2,A,A,I3,A,I3)')' "SELTXR" JJ= ',JJ,'  CHANG',
     1      'ING FROM 0.05 TO 0.5 CHANGED SELECT MODE FROM ',LSAV,' TO',
     2      LTXRGM(JJ)
          ENDIF
        ENDIF
      ENDIF
  999 CONTINUE
 1000 CONTINUE
      RETURN
      END
C--------------------------------

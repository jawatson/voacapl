c###alosfv.for
      SUBROUTINE ALOSFV(K)
C--------------------------------
C
C     THIS ROUTINE COMPUTES THE DEVIATIVE LOSS FACTOR
C
C     THIS IS ZERO EXCEPT FOR THOSE MODES COMING FROM HEIGHTS ABOVE THAT
C     CORRESPONDING TO THE MUF (HIGH ANGLE MODES)
C     THE ADJUSTMENT IS BASED ON A THEORETICAL EQUATION
C     THE LOSS IS PROPORTIONAL TO THE PRODUCT OF THE AVERAGE COLLISION
C     FREQUENCY AND THE DIFFERENCE BETWEEN THE GROUP PATH AND THE
C     PHASE PATH BUT IS NORMALIZED TO THE LOSS EQUATION
C
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     1,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMU
     2F(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     3 ,YMUF(4)
      DIMENSION A(3),HM(2)
      DATA  A/0.2,0.1,0.1/
C  E LAYER
      HM(1) = HPMUF(1) - HTMUF(1)
      HZ    = HI(1,K)  - YI(1,K)
      DO 105 IH =1,10
      IF( HTRUE(IH,K)- HTMUF(1) ) 100,100,102
  100 AFAC(IH,K) = 0.0
      GO TO 105
C.....COLLISION FREQUENCY TERM ADJUSTMENT FACTOR
  102 ZEXP = -2. * ( HTRUE(IH,K) - HZ ) / YI(1,K)
      ZEXP = AMAX1(ZEXP,-10.)
      CF = A(1) * EXP( ZEXP )
      AFAC(IH,K) = CF * (HPRIM(IH,K) - HTRUE(IH,K) - HM(1))
  105 CONTINUE
      IF( FI(2,K) )  115,115,140
  115 CONTINUE
C   F2 LAYER WITH NO F1 LEDGE
C
C
C.....CONTINUITY AT E TO F2 CUSP
      A(2) = CF
      HZ = HTRUE(11,K)
      HM(2) = HPRIM(13,K) - HTRUE(13,K)
      DO 120  IH = 11,12
      ZEXP = -2. * ( HTRUE(IH,K) - HZ ) / YI(3,K)
      ZEXP = AMAX1(ZEXP,-10.)
      CF = A(2) * EXP( ZEXP )
      CF = AMAX1(CF,0.05)
  120 AFAC(IH,K) = CF* ( HPRIM(IH,K) - HTRUE(IH,K) - HM(2))
      AFAC(13,K) = 0.0
      A(3) = 0.1
      HM(2) = HPMUF(3) - HTMUF(3)
      DO 135  IH = 14,30
      IF( HTRUE(IH,K) - HTMUF(3) ) 125,125,130
  125 AFAC(IH,K) =0.0
      GO TO 135
  130 ZEXP = -2. * ( HTRUE(IH,K) - HZ ) / YI(3,K)
      ZEXP = AMAX1(ZEXP,-10.)
      CF = A(3) * EXP( ZEXP )
C.....FORCE THE MINIMUM VALUE OF CF TO BE NO LESS THAN 0.5
      CF = AMAX1(CF,0.5)
      AFAC(IH,K) = CF *(HPRIM(IH,K) - HTRUE(IH,K) - HM(2) )
  135 CONTINUE
      GO TO 200
  140 CONTINUE
      IF( FI(2,K) ) 115,115,145
  145 CONTINUE
C  F1  LAYER
C.....CONTINUITY AT E TO F1 CUSP
      A(2) = CF
      HZ  = HTRUE(11,K)
      HM(2) = HPRIM(13,K) - HTRUE(13,K)
      DO  150  IH = 11,12
      ZEXP = -2. * ( HTRUE(IH,K) - HZ ) / YI(2,K)
      ZEXP = AMAX1(ZEXP,-10.)
      CF = A(2) * EXP( ZEXP )
      CF = AMAX1(CF, 0.05)
  150 AFAC(IH,K) = CF * (HPRIM(IH,K) - HTRUE(IH,K) - HM(2) )
      AFAC(13,K) = 0.0
      A(2) = 0.1
      HTM = HTMUF(2)
      HTP = HPMUF(2)
      HM(2) = HTP - HTM
      DO 165 IH = 14, 20
      IF(HTRUE(IH,K)  - HTM)  155,155,160
  155 AFAC(IH,K) = 0.0
      GO TO 165
  160 ZEXP = -2. * ( HTRUE(IH,K) - HZ ) / YI(2,K)
      ZEXP = AMAX1(ZEXP,-10.)
      CF = A(3) * EXP( ZEXP )
      CF = AMAX1( CF, 0.05)
      AFAC(IH,K) = CF *( HPRIM(IH,K) - HTRUE(IH,K) - HM(2) )
  165 CONTINUE
C  F2 LAYER WITH F1 LEDGE
C.....FORCES CONTINUITY AT F1 TO F2 LAYER
      A(3) = CF
      HZ   = HTRUE(21,K)
      HM(2) = HPRIM(23,K) - HTRUE(23,K)
      DO 170 IH = 21,22
      ZEXP = -2. * ( HTRUE(IH,K) - HZ ) / YI(3,K)
      ZEXP = AMAX1(ZEXP,-10.)
      CF = A(3) * EXP( ZEXP )
      CF = AMAX1(0.05,CF)
  170 AFAC(IH,K) = CF * (HPRIM(IH,K) - HTRUE(IH,K) - HM(2))
      AFAC(23,K) = 0.0
      A(3) = 0.1
      HTM = HTMUF(3)
      HTP = HPMUF(3)
      HM(2) = HTP - HTM
      DO 185 IH = 24,30
      IF(HTRUE(IH,K) - HTM )  175,175,180
  175 AFAC(IH,K) = 0.0
      GO TO 185
  180 ZEXP = -2. * ( HTRUE(IH,K) - HZ ) / YI(3,K)
      ZEXP = AMAX1(ZEXP,-10.)
      CF = A(3) * EXP( ZEXP )
      CF = AMAX1(CF,.05)
      AFAC(IH,K) = CF*(HPRIM(IH,K) - HTRUE(IH,K) - HM(2) )
  185 CONTINUE
  200 CONTINUE
C     HPMUF HAS ADDED CORRECTION ALREADY
      DO 205 IH = 1,30
      AFAC(IH,K) = AMAX1(0.,AFAC(IH,K) )
  205 CONTINUE
      RETURN
      END
C------
C--------------------------------

      SUBROUTINE MAGFIT (FLAT,FLON,HEIGHT,DIP,GYRO)
c>>(b)
C.........VERSION 28 OCT 1993   note 'HEIGHT' additional input parameter
C.........COMPUTATION OF DIP AND GYRO FREQUENCY
C.........AT HEIGHT=300 Km FOR DETERMINATION OF fOF2,MUF etc (REC. 434)
C.........AT HEIGHT=100 Km FOR use in absorption
      DIMENSION P(7,7),DP(7,7),CP(7),AOR(7),SP(7),G(7,7),H(7,7),CT(7,7)
c>>(b)WP-6A SEPT. 93 MODS REMOVE VARIABLE 'AR' FROM DATA STATEMEMT
      DATA P/1.,48*0./,CP/1.,6*0./,DP,SP,AOR/63*0./,CT/2*0.
     1,.33333333,.266666666,.25714286,.25396825,.25252525,3*0.,.2,
     2.22857142,.23809523,.24242424,4*0.,.14285714,.19047619,.21212121,
     35*0.,.1111111111,.16161616,6*0.,.09090909,14*0./,G/0.,.304112,
     4.024035,-.031518,-.041794,.016256,-.019523,0.,.021474,-.051253,
     5.062130,-.045298,-.034407,-.004853,2*0.,-.013381,-.024898,-.021795
     6,-.019447,.003212,3*0.,-.006496,.007008,-.000608,.021413,4*0.,
     7-.002044,.002775,.001051,5*0.,.000697,.000227,6*0.,.001115/,H/8*0.
     8,-.057989,.033124,.014870,-.011825,-.000796,-.005758,2*0.,-.001579
     9,-.004075,.010006,-.002,-.008735,3*0.,.00021,.00043,.004597,
     A-.003406,4*0.,.001385,.002421,-.000118,5*0.,-.001218,-.001116,6*0.
     B,-.000325/
c>>(b)WP-6A SEPT. 93 MODS add 1 line  variable 'AR'
      AR=6371./(6371. +HEIGHT)
      AOR(1)=AR**2
      DO 1  M=2,7
1     AOR(M)=AOR(M-1)*AR
      PHI=FLON/57.295779513
C.....ENSURE -90 < LATITUDE < 90 DEGREES
      IF(ABS(FLAT).GT.90.0) THEN
       IF(FLAT.LE.0.0) THEN
        FLAT=-89.99999
       ELSE
        FLAT= 89.99999
       END IF
      END IF
      DLAT= FLAT/57.295779513
C.....C, S = COSINE, SINE OF CO-LATITUDE
      C=SIN (DLAT)
      S=COS (DLAT)
      SP(2)=SIN (PHI)
      CP(2)=COS (PHI)
      DO 2 M=3,7
      SP(M)=SP(2)*CP(M-1)+CP(2)*SP(M-1)
2     CP(M)=CP(2)*CP(M-1)-SP(2)*SP(M-1)
      BV=0.
      BN=0.0
      BPHI=0.0
C.....
      DO 8 N=2,7
      FN=N
      SUMR=0.0
      SUMT=0.0
      SUMP=0.0
C.....
      DO 7 M=1,N
      IF(N-M) 4,3,4
3     P(N,N)=S*P(N-1,N-1)
      DP(N,N)=S*DP(N-1,N-1)+C*P(N-1,N-1)
      GOTO 6
4     IF(N.EQ.2) GOTO 5
      P(N,M)=C*P(N-1,M)-CT(N,M)*P(N-2,M)
      DP(N,M)=C*DP(N-1,M)-S*P(N-1,M) - CT(N,M)*DP(N-2,M)
      GOTO 6
5     P(N,M)=C*P(N-1,M)
      DP(N,M)=C*DP(N-1,M)-S*P(N-1,M)
6     FM=M-1
      TS=G(N,M)*CP(M)+H(N,M)*SP(M)
      SUMR=SUMR+P(N,M)*TS
      SUMT=SUMT+DP(N,M)*TS
7     SUMP=SUMP+FM*P(N,M)*(-G(N,M)*SP(M)+H(N,M)*CP(M))
C.....
      BV=BV+AOR(N)*FN*SUMR
      BN=BN-AOR(N)*SUMT
8     BPHI=BPHI-AOR(N)*SUMP
C.....
      BPHS=BPHI/S
      ZW=BN*BN + BPHS*BPHS
      DIP=ATAN (BV/SQRT (ZW))
      ZW=ZW+BV*BV
      GYRO=2.8 * SQRT (ZW)
      RETURN
      END

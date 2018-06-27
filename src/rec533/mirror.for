c###mirror.for
      SUBROUTINE MIRROR(Xr,y,H,d,KFA,JKA,K1,Hr)
C.....
C.....SUBROUTINE MIRROR: CALCULATES F2-MIRROR REFLECTION HEIGHT
C.....                   CCIR WP-6A Doc 8,Geneva,Nov 1991
C.....INPUT: Xr=FREQUENCY/foF2
C............ y=foF2/foE ,MEASURE OF THE UNDERLYING IONISATION (MIN=1.8)
C.............H=HmF2-140
C.............d=HOP RANGE IN km
C...........IF2=COUNT OF F2-modes (1-6)
C.....OUTPUT: Hr= REFLECTION HT. IN km, MAX VALUE = 800 km (KONTROL=0)
C.....
      INTEGER*4 KFA(5),JKA(5)
      REAL*4 J,A1A(5),B1A(5),dsa(5),ZA(5),A2A(5),B2A(5),UA(5),JA(5)
      IF(y.GT.3.3301) THEN
C.....LOW UNDERLYING IONISATION (frequency dependent refl. ht)
      IF(Xr.GE.1.0) THEN
C.....WHERE FREQ/FOF2 .GE.1
C.....CALC. MIN. HT(=A1),MAX. HT(A1+B1) AND SKIP DIST.(ds)
C.....FOR A GIVEN HMF2 AND FREQUENCY RATIO Xr
C.....NEED ONLY TO CALC. FUNCTIONS OF Xr (E1,F1 etc on 1'st entry for
C.....a given frequency)
      IF(KFA(K1).EQ.0) THEN
       Xr2=Xr*Xr
       Xr3=Xr2*Xr
       Xr4=Xr3*Xr
       E1=-0.09707*Xr3+0.6870*Xr2-0.7506*Xr+0.6
       A1=(H-47.)*E1+140.
       IF(Xr.LE.1.71) THEN
        F1=-1.862*Xr4+12.95*Xr3-32.03*Xr2+33.50*Xr-10.91
       ELSE
        F1=1.21+0.2*Xr
       END IF
       B1=(H-17.)*F1+150.0-A1
       IF(Xr.LE.3.7) THEN
        G=-2.102*Xr4+19.5*Xr3-63.15*Xr2+90.47*Xr-44.73
       ELSE
        G=19.25
       END IF
C......CALCULATE SKIP DISTANCE ds (km)
      ds=(H+43.0)*G+160.0
      ds=AMAX1(ds,0.0)
C......STORE VALUES OF A1,B1,ds CALCULATED ON FIRST ENTRY(AT A GIVEN
C......FREQUENCY) FOR USE WITH HIGHER-ORDER MODES
      KFA(K1)=K1
      A1A(K1)=A1
      B1A(K1)=B1
      dsa(K1)=ds
      END IF
       A1=A1A(K1)
       B1=B1A(K1)
       ds=dsa(K1)
       a=(d-ds)/(H+140.0)
       IF(B1.GE.0.0.AND.a.GE.0.0) THEN
        c=2.4
        Hr=A1+B1/c**a
       ELSE
        Hr=A1+B1
       END IF
      ELSE
C.....Xr .LT. 1
C.....NEED ONLY TO CALC. FUNCTIONS OF Xr (Z,E2 etc on 1'st entry)
      IF(KFA(K1).EQ.0) THEN
       Z=AMAX1(Xr,0.1)
       Z2=Z*Z
       E2= 0.1906*Z2+0.00583*Z+0.1936
       A2=(H-47.)*E2+151.0
       F2= 0.645*Z2+0.883*Z+0.162
       B2=(H-24.)*F2+141.0-A2
C......STORE VALUES OF Z,A2,B2 CALCULATED ON FIRST ENTRY(AT A GIVEN
C......FREQUENCY) FOR USE WITH HIGHER-ORDER MODES
       KFA(K1)=K1
       ZA(K1)=Z
       A2A(K1)=A2
       B2A(K1)=B2
      END IF
       Z=ZA(K1)
       A2=A2A(K1)
       B2=B2A(K1)
       IF(B2.GE.0.0) THEN
C.......CALCULATE NORMALISED DISTANCE,df
        df=0.115*d/(Z*(H+140.))
        df=AMIN1(df,0.65)
        b=-7.535*df**4+15.75*df**3-8.834*df**2-0.378*df +1.0
        Hr=A2+B2*b
       ELSE
        Hr=A2+B2
       END IF
      END IF
C******
      ELSE
C.....HIGH UNDERLYING IONISATION (ionisation dependant height, depends
C.....on ratio foF2/foE but not on freq.)
C.....NEED ONLY CALC. TERMS ONCE PER CONTROL POINT AT EACH TIME
      IF(JKA(K1).EQ.0) THEN
       U=8.0E-05*(H-80.0)*(1.0+11.0/(y**2.2))
     1       +1.2D-03*H/(y**3.6)
       J=-0.7126*y**3+5.863*y**2-16.13*y+16.07
       UA(K1)=U
       JA(K1)=J
       JKA(K1)=K1
      ELSE
       U=UA(K1)
       J=JA(K1)
      END IF
       Hr=H*J+U*d+115.0
      END IF
C.....
      Hr=AMAX1(Hr,0.0)
      Hr=AMIN1(Hr,800.0)
      RETURN
      END

c###xlin.for
      FUNCTION XLIN(X,XN,YN,NTAB,K)
C--------------------------------
C
C     THIS ROUTINE DOES LINEAR INTERPOLATION
C
C     XN AND YN MAY BE MULTIPLE SUBSCRIPTED
C
      DIMENSION XN(*),YN(*)
      ISMIN=NTAB*(K-1)
      ISMAX=NTAB*K
      IS=ISMIN+1
      IF(XN(IS)-X) 3,3,2
    2 XLIN=YN(IS)
      RETURN
    3 NMIN1=NTAB-1
      DO 19 J=1,NMIN1
      IS=ISMIN+J
      IF(XN(IS)-XN(IS+1)) 8,5,11
    5 IF(X-XN(IS)) 19,7,19
    7 XLIN=YN(IS)
      RETURN
    8 IF(XN(IS)-X) 9,9,10
    9 IF(X-XN(IS+1)) 10,19,19
   10 A=YN(IS)
      B=X-XN(IS)
      C=YN(IS+1)-YN(IS)
      D=XN(IS+1)-XN(IS)
      XLIN=A+B*C/D
      RETURN
   11 IF(XN(IS)-X) 19,12,12
   12 IF(X-XN(IS+1)) 19,19,10
   19 CONTINUE
      XLIN=YN(ISMAX)
      RETURN
      END
C--------------------------------

C SUBROUTINE CMPINV           (NEXT)
      SUBROUTINE CMPINV (A, B, C, D, N) 
C 
C     CMPINV - COMPLEX MATRIX OPERATION THAT FINDS INVERSE, C+ID, OF
C        A+IB WHERE A,B,C, AND D ARE REAL SQUARE MATRICES.  CMPINV
C        CALLS MATINV TO INVERT REAL MATRICES AND SQMULT TO MULTIPLY
C        SQUARE MATRICES IN ORDER TO COMPUTE THE COMPLEX INVERSE
C                      C=INV(A + B*(INV(A))*B)
C                      D=-C*B*(INV(A))
C        A, B, C, AND D ARE ALL REAL N BY N MATRICIES.
C        IF A IS SINGULAR, EXCHANGE A AND B (FACTOR OUT I) AND TRY AGAIN
C     THIS ROUTINE IS SHARE NO. ANL F103 BY B.S.GARBOW OF ARGONNE 
C     NATIONAL LABORATORY, ARGONNE, ILLINOIS
C 
      DIMENSION A (20, 20), B (20, 20), C (20, 20), D (20, 20)
C 
      COMMON /PON /INDEX (20, 2), IPIVOT (20), PIVOT (20) 
C 
C.....INVERT -A.........................................................
      M = N 
      L = 1 
 100  IF (M .LT. 1) GO TO 160 
      DO 105 I = 1, M 
      IF (M .LT. 1) GO TO 165 
      DO 105 J = 1, M 
 105  D (I, J) = - A (I, J) 
 165  CONTINUE
 160  CONTINUE
      CALL MATINV (D, M)
C.....CHECK IF INV(-A) EXISTS (IF NOT GO TO 6)..........................
      IF (M .LT. 1) GO TO 170 
      DO 110 K = 1, M 
      IF (IPIVOT (K) .NE. 1) GO TO 125
 110  CONTINUE
 170  CONTINUE
C.....COMPUTE C=INV(A - B*(INV(-A))*B)..................................
      CALL SQMULT (B, D, D, M)
      CALL SQMULT (D, B, C, M)
      IF (M .LT. 1) GO TO 175 
      DO 115 I1 = 1, M
      IF (M .LT. 1) GO TO 180 
      DO 115 J1 = 1, M
 115  C (I1, J1) = A (I1, J1) - C (I1, J1)
 180  CONTINUE
 175  CONTINUE
      CALL MATINV (C, M)
C.....CHECK IF C EXISTS (IF NOT GO TO 300)..............................
      IF (M .LT. 1) GO TO 185 
      DO 120 K1 = 1, M
      IF (IPIVOT (K1) .NE. 1) GO TO 150 
 120  CONTINUE
 185  CONTINUE
C.....COMPUTE D=C*B*(INV(-A))...........................................
      CALL SQMULT (C, D, D, M)
      IF(L.NE.2) GO TO 145
      GO TO 135 
C.....A IS SINGULAR, INTERCHANGE A AND B AND TRY AGAIN..................
 125  IF (M .LT. 1) GO TO 190 
      DO 130 I2 = 1, M
      IF (M .LT. 1) GO TO 195 
      DO 130 J2 = 1, M
      TEMP = A (I2, J2) 
      A (I2, J2) = B (I2, J2) 
 130  B (I2, J2) = TEMP 
 195  CONTINUE
 190  CONTINUE
C.....IF A AND B BOTH SINGULAR, GO TO 310...............................
      IF (L .EQ. 2) GO TO 155 
      L = 2 
      GO TO 100 
C.....RE-INTERCHANGE A AND B, AND INTERCHANGE C AND D WITH SIGNS CHANGED
C     DUE TO THE -I FACTORED OUT WHEN A AND B ORIGINALLY INTERCHANGED...
 135  IF (M .LT. 1) GO TO 200 
      DO 140 I3 = 1, M
      IF (M .LT. 1) GO TO 205 
      DO 140 J3 = 1, M
      TEMP = A (I3, J3) 
      A (I3, J3) = B (I3, J3) 
      B (I3, J3) = TEMP 
      TEMP = - C (I3, J3) 
      C (I3, J3) = - D (I3, J3) 
 140  D (I3, J3) = TEMP 
 205  CONTINUE
 200  CONTINUE
 145  RETURN
C 
C.....ERROR STOP - NO INVERSE EXISTS....................................
 150  WRITE (* , 1500)
1500  FORMAT ('0MATRIX C IN SUBROUTINE CMPINV DOES NOT EXIST.'//) 
      STOP      
C.....ERROR STOP - INVERSE CANNOT BE FOUND BY THIS METHOD...............
 155  WRITE (* , 1502)
1502  FORMAT ('0MATRICES A AND B BOTH SINGULAR IN SUBR CMPINV.'//)
      STOP      
      END 

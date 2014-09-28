C SUBROUTINE SQMULT           (NEXT)
      SUBROUTINE SQMULT (A, B, C, N)
C 
C     SQUARE MATRIX MULTIPLICATION (COMPANION TO CMPINV IN ANL F103)
C 
      DIMENSION A (20, 20), B (20, 20), C (20, 20)
C 
      COMMON /PON /INDEX (20, 2), IPIVOT (20), PIVOT (20) 
C 
C.....C=A*B.............................................................
      IF (N .LT. 1) GO TO 110 
      DO 105 J = 1, N 
      IF (N .LT. 1) GO TO 115 
      DO 100 K = 1, N 
 100  PIVOT (K) = B (K, J)
 115  CONTINUE
      IF (N .LT. 1) GO TO 120 
      DO 105 I = 1, N 
      C (I, J) = 0.0
      IF (N .LT. 1) GO TO 125 
      DO 105 L = 1, N 
 105  C (I, J) = C (I, J) + A (I, L) * PIVOT (L)
 125  CONTINUE
 120  CONTINUE
 110  CONTINUE
      RETURN
      END

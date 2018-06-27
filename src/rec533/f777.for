****  Kishcha - Cubic Spline Function
      REAL FUNCTION F777(Z)
        DIMENSION  A(28,4),XX(28)
        COMMON /R4/ A,XX,NPO
        DO 1 I=1,NPO-1
        IF (Z.GE.XX(I).AND.Z.LT.XX(I+1)) GO TO 2
    1   CONTINUE
    2   XI=Z-XX(I)
        F777=A(I,1)+(A(I,2)+(A(I,3)+A(I,4)*XI)*XI)*XI
        RETURN
        END
c*******************************************************************

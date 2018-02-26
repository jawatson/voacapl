c# cisi.f
      SUBROUTINE CISI (X, CI, SI)
C--------------------------------
C
C X
C CI IS COSINE INTEGRAL OF X
C SI IS SINE   INTEGRAL OF X
C
      IF (X - 1.)100, 120, 120
 100  SQ = X * X
      CI = .5772156649 + ALOG (X)
      TERM = - 1. * SQ / 4.
      G = 4.
 105  CI = CI + TERM
      TERM = - 1. * TERM * SQ * (G - 2.) / ((G - 1.) * G * G)
      G = G + 2.
      IF (ABS (TERM) - .00005)110, 110, 105
 110  SI = 0.
      TERM = X
      G = 3.
 115  SI = SI + TERM
      TERM = - 1. * TERM * SQ * (G - 2.) / ((G - 1.) * G * G)
      G = G + 2.
      IF (ABS (TERM) - .00005)125, 125, 115
 120  X2 = X * X
      T = COS (X)
      S = SIN (X)
      G = ((((X2 + 48.196927) * X2 + 482.485984) * X2 + 1114.978885) * X
     12 + 449.690326) * X2
      G = ((((X2 + 42.252855) * X2 + 302.757865) * X2 + 352.018498) * X2
     1 + 21.821899) / G
      F = ((((X2 + 40.021433) * X2 + 322.624911) * X2 + 570.236280) * X2
     1 + 157.105423) * X
      F = ((((X2 + 38.027264) * X2 + 265.187033) * X2 + 335.677320) * X2
     1 + 38.102495) / F
      SI = 1.5708 - F * T - G * S
      CI = F * S - G * T
 125  RETURN
      END
C--------------------------------

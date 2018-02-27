c###f2dis.for
      SUBROUTINE F2DIS (FMUF, SSN, CLAT, FREQ, ABB, SIG)
C--------------------------------
c          re-written 3/20/2009 by Greg Hand to modern Fortran
C--------------------------------
C
C     THIS ROUTINE OBTAINS THE UPPER AND LOWER DECILE FOR THE F2 MUF
C
C     USES TABLES OF F2 MUF(3000)  (IE MUF FOR A DISTANCE OF 3000 KM)
C
C FMUF IS F2 MUF,MHZ.
C SSN  IS SUNSPOT NUMBER.
C CLAT IS GEOGRAPHIC LATITUDE, RADIANS.
C FREQ IS OPERATING FREQUENCY,MHZ.
C ABB  IS LOCAL MEAN TIME (HOURS)   SEE CLCK(K)
C SIG  IS STANDARD DEVIATION OF F2 MUF (NORMAL)
C
      COMMON / TWO / F2D(16,6,6), P(29,16,8), ABP(2,9), DUD(5,12,5),
     A FAM(14,12), SYS(9,16,6), PERR(9,4,6)

      IF(F2D(1,1,1).le.0) then   !  No ionospheric data - use classical values
         SIG=0.15*FMUF/1.28155
         SIG=AMAX1(SIG,0.001)
         RETURN
      end if

      CL = CLAT * 57.29577    !  convert to degrees
      ICC = ABB / 4.0 + 1.55  !  4 hour time blocks
      IF(ICC.gt.6) ICC=1      !  make sure index is within limits

      IF(FREQ.le.FMUF) then
         I = 8                !  lower decile
      else
         I = 0                !  upper decile
      end if

      ICL = 9.5 - ABS (CL) / 10.
      if(ICL.lt.1) ICL=1    !  limit index
      if(ICL.gt.8) ICL=8    !  limit index
      IZ = ICL + I

      J=2                     !  50 < SSN <= 100
      if(SSN.le.50.) J=1      !  SSN <=  50
      if(SSN.gt.100.) J=3     !  SSN >  100

      if(cl.le.0.) J=J+3      !  Southern Hemisphere
      SIG = ABS(FMUF - F2D(IZ,J,ICC) * FMUF) / 1.28
      IF (SIG .LE. 0.001) SIG = 0.001
      RETURN
      END
C--------------------------------

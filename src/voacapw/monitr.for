c# monitr.f
      FUNCTION MONITR(LU,NX)
C--------------------------------
C
C / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
C
C     THIS FUNCTION READS A CARD IMAGE FROM LOGICAL UNIT LU.
C     THE FIRST TEN COLUMNS CONTAIN A STRING OF 10 HOLLERITH
C     CHARACTERS CALLED THE "NAME IDENTIFIER". THE "NAME IDENTIFIER"
C     IS COMPARED WITH THE LIST OF 10-CHARACTER STRINGS STORED IN THE
C     ARRAY CALLED "NAMES". IF A MATCH IS FOUND, THE VALUE OF MONITR
C     IS SET EQUAL TO THE SUBSCRIPT OF THE MATCHING NAME IN THE LIST.
C     A MAXIMUM OF NX NAMES WILL BE SEARCHED. IF NO MATCHING NAME
C     IS FOUND, THE VALUE OF MONITR WILL BE SET TO NX + 1.
C
C / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
C
      COMMON / NAMEX / NAMES(100), INPUT
      CHARACTER NAMES*10, INPUT*85
      CHARACTER NAME*10
C
C
C-----------------------------------------------------------------------
C
C.....READ ONE CARD WITH THE "NAME IDENTIFIER" IN COLUMNS 1 - 10.
      READ(LU,'(a)',end=900) INPUT
      NAME = INPUT (1:10)
      IF (NX .LT. 1) GO TO 145
      DO 100 I = 1, NX
      IF(NAME .EQ. NAMES(I)) GO TO 105
 100  CONTINUE
 145  CONTINUE
C - - - - - - THERE WAS NO MATCHING NAME IN THE LIST OF NAMES.
      MONITR = NX + 1
      RETURN
C - - - - - - A MATCHING NAME WAS FOUND.
 105  MONITR = I
ccc      write(*,'('' nx='',i5)') nx
ccc      write(*,2) i,lu,input
ccc2     format(' monitr=',i3,'  lu=',i3,' input=',8a10)
      RETURN
900   stop 'EOF read in monitr'
      END
C--------------------------------

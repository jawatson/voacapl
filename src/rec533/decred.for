c###decred.for
      SUBROUTINE DECRED
      use voacapl_defs
      use crun_directory
C.....VERSION 10.APRIL.92
C     THIS SUBROUTINE CALLS FUNCTION MONITR TO READ THE CONTROL CARDS
C     THE CARD IMAGES ARE THEN DECODED , PROCESSED AND THE DATA STORED
C     NO. OF OUTPUT TYPES CURRENTLY =6
      INTEGER IWCARD(20),PERCENT
      CHARACTER*1 MIN,IMIN
      CHARACTER*5 CJRSN
      CHARACTER*4 NAMES,APW
      CHARACTER*40 VERSN
      CHARACTER*85 INPUT
      character gainfile*10
c      common /crun_directory/ run_directory
c         character run_directory*50
      common /ccoeff/ coeff
      character coeff*4
      common /C_digital/ idigital,A_ratio,Tw,Fw
      common /CFS_limit/ FS_limit
      common /cantenna/ numants,iats( 5),anttype( 6),antname( 6),
     +                  xfqs( 5),xfqe( 5),designfreq( 5),antfile( 5),
     +                  beammain( 5),offazim( 5),cond( 5),diel( 5),
     +                  array(30,91,12),aeff(30, 5)
      character anttype*10,antname*70,antfile*24
      dimension array91(91)
      integer*2 iarray360(360,91,2)
      equivalence (array,iarray360)        !  for area coverage
      common /pantenna/ pwrkw(5),pwrdba(5)
      COMMON /RGRID/ IPROJ,PLATD,PLOND,XMIN,XMAX,YYMIN,YMAX,NX,NY
      common /chours/ nhours,ihours(24)
      common /cmonssn/ months(12),ssns(12)
      COMMON /CON/D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RO, VOFL
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      COMMON /FRQ/ FREL(12),FREL5(11,7),FW5(11,7),MAXF,MF,KOF,FREQ
     A, JKF,FXEMAX
      COMMON/GEOG/ABIY(5),CLAT(5),CLONG(5),CLCK(5),EPSPAT(5),F2M3(5)
c>>(b)WP-6A SEPT. 93 MODS add 2 lines REMOVE 1 LINE
     A,FI(3,5),GLAT(5),GMDIP(5),GYZ100(5),GYR100,GYZ300(5),HPF2(5),RD(5)
     B,SIGPAT(5),CYCEN(5)
CX   A,FI(3,5),GLAT(5),GMDIP(5),GYZ(5),HPF2(5),RD(5),SIGPAT(5),CYCEN(5)
c>>(b)WP-6A SEPT. 93 MODS end of change
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      COMMON/NAMEX/NAMES(20),ISSN,IRED,LINES,LPAGES,MAPIN,KRUN
      COMMON /SOL/ DECL12(12),EQT12(12),DECL,EQT,MONTH,IMON(12)
      COMMON /XK/ YTAB(24,12), YSPWR(24,12), XLY
      COMMON / HOG / IANTT, BANTT, IANTR, BANTR
      COMMON /NSTATS/ DU,DL,DUA,DLA,DUM,DLM,DUG,DLG
      LOGICAL YNOISE
      COMMON /TNOISE/BDWTH,JBW,JRSN,LUF,MAN,RLUF,RSN,XSN(2),YNOISE
C.WP3L.RG4  Jun mod 1 line
      COMMON /FADE/ PERCENT,FRACX,FSX,FBX,DUSN,DLSN,SDU,SDL,SHU,SHL
      character path*1
      DATA IMONTH/0/,IWCARD/20*1/,IEXEC/0/,MAXNAM/20/
      DATA IMIN/'M'/
      data lu26/26/
C ----------------------------------------------------------------------
C
C     SUBROUTINE REDMAP MAY READ THE IONOSPHERIC LONG TERM DATA BASE
C     FILE (LU2) DEPENDENT ON TASK OPTION AND USER DEFINED INPUT
C     THE PROGRAM WILL TERMINATE IF AN ERROR OCCURS (SET ITRUN=0)
C
C     ON THE FIRST CALL TO THIS SUBROUTINE CONTROL PASSES TO STATEMENT
C     LABEL 100 TO READ THE FIRST CONTROL CARD FROM LUI WHICH INITIALLY
C     IS SET TO LU5.  CONTROL THEN PASSES TO STATEMENT LABEL 105 TO
C     BRANCH AND PROCESS THE CONTROL CARD.
C
C ----------------------------------------------------------------------
      CMD(X)=AINT(X)+AMOD(X,1.0)/0.6
C ----------------------------------------------------------------------
      idigital=0
      FS_limit=-999.
      nch_run=lcount(run_directory,50)
      IF(IEXEC) 870, 87, 20
   20 IEXEC=0
   87 IF(IRED) 105, 105, 90
   90 CONTINUE
C.....READ INPUT CARD FROM LUI USING MONITR
  100 ICARD=MONITR(LUI,NAMES,MAXNAM,INPUT)
  105 CONTINUE
ccc      write(luo,'(''icard='',i5,1h=,a)') icard,input
C.....ICARD IS AN INDEX OF THE CARD NAME
C.....ICARD=1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18
C.....FOR UPWARD COMPATIBILITY, TREAT UNKNOWN CARDS AS BLANK
       IF(ICARD-MAXNAM) 106,106,870
  106 GO TO(110,165,180,210,235,270,280,340,410,440,115,470,570,670
     A ,770,780,790,795,798,800,870),ICARD
C-----------------------------------------------------------------------
C..... ICARD 1-10 ARE FORMATTED FOR THE CURRENT VERSION OF REC533-0
C..... PREVIOUS VERSIONS OF REC533-0 USE ICARD 3,6-10 AND 11-15
C..... NOTE HOWEVER THIS VERSION OF REC533-0 CAN READ CARDS IN
C..... BOTH FORMATS
C-----------------------------------------------------------------------
C     OUTPUT,METHOD CARDS                                 ICARDS=1,11
C-----------------------------------------------------------------------
C.....THESE CARDS ARE USED FOR PROGRAM CONTROL
  110 IF(IWCARD(1).LE.0) GO TO 900
      GO TO 114
  115 IF(IWCARD(11).LE.0) GO TO 900
  114 IF(ICARD.EQ.1) READ(INPUT,1503) METHOD,NPSL,XXLZ,XXLY
     A ,IDMAX,NPAGO,MAXF
      IF(ICARD.EQ.11) READ(INPUT,1509) METHOD,XXLZ,XXLY,IDMAX,NPAGO
     A ,METSEL,MAXF
       METHOD=MIN0(METHOD,6)
       METSEL=4
C.....TERMINATE EXECUTION IF METHOD .LE. 0 OR .GT. 7
C.....NO NEED TO CALC. S/N OTHER THAN FOR LUF FOR METHODS 1,4,AND 5
      YNOISE=(METHOD.EQ.1.OR.METHOD.EQ.4.OR.METHOD.EQ.5)
C.....HOWEVER, A "QUIT" CARD IS REQUIRED AS THE LAST CONTROL CARD
C    DEFAULT TO NORMAL VALUE
      IF(XXLZ .EQ. 0.0) GO TO 116
      XLZ=XXLZ
  116 IF(XXLY .EQ. 0.0) GO TO 117
      XLY=XXLY
  117 CONTINUE
      IF(IDMAX.GT.0) GCDFTZ=IDMAX
      IF(METHOD.LE.0) GO TO 120
  122 IF(METHOD.LE.7) GO TO 125
  120 ITRUN=0
      ITOUT=0
      GO TO 920
C.....SET RUN AND OUTPUT INDICATORS DEPENDING ON METHOD
  125 ITRUN=1
C.....SET STARTING PAGE NUMBER FOR OUTPUT FILE
      IF(NPAGO) 135, 135, 130
  130 LPAGES=NPAGO-1
  135 IF(MAXF.LE.0) MAXF=10
      MF=MAXF+1
      IWCARD(ICARD)=-1
      GO TO 90
C ----------------------------------------------------------------------
C     MONTH AND SUNSPOT CARD                                  ICARD=2
C ----------------------------------------------------------------------
  165 stop 'MSUN card no longer valid'
C ----------------------------------------------------------------------
C     TIME CARD                                               ICARD=3
C ----------------------------------------------------------------------
  180 IF(IWCARD(3)) 900, 900, 185
  185 READ(INPUT,1500) IT1,IT2,ITSTEP
C.....SET VARIABLES TO DEFAULT VALUES IF NOT SET ON SYSTEM CARD
ccc      IF(IT1.LT.0.OR.IT1.GT.24) IT1=1
ccc      IF(IT2.LT.0.OR.IT2.GT.24) IT2=24
ccc      IF(IT1.NE.0.AND.IT2.NE.0) GO TO 190
ccc      IT1=1
ccc      IT2=24
ccc  190 IF(IABS(ITSTEP).EQ.0.OR.IABS(ITSTEP).GT.24) ITSTEP=1
      IWCARD(3)=-1
c          New time concept by Greg Hand 5/18/93
c          Time may now run around midnight (e.g. 22 to 04)
      if(itstep.le.0 .or. itstep.gt.12) itstep=1
      istart=it1
      if(istart.eq.0) istart=24    !  0 = 24 = midnight
      iend=it2
      if(iend.lt.istart) iend=iend+24
      nhours=0
      do 195 ihr=istart,iend,itstep
      jhr=ihr
      if(jhr.gt.24) jhr=jhr-24
      nhours=nhours+1
195   ihours(nhours)=jhr
      GO TO 90
C ----------------------------------------------------------------------
C     TRANSMITTER CARD                                        ICARD=4
C ----------------------------------------------------------------------
  210 READ(INPUT,1505) ITRANS,TLATD,ITLAT,TLONGD,ITLONG,path,MIN
      if(path.ne.'L') npsl=0     !  short path
      if(path.eq.'L') npsl=1     !  long  path
       LOC=1
       IWCARD(4)=-1
       IF(MIN.EQ.IMIN) then
C.....ENTER WITH MINUTES AFTER DECIMAL PLACE
          TLATD=CMD(TLATD)
          TLONGD=CMD(TLONGD)
       end if
      if(ITLAT(1:1).eq.'S') TLATD=-TLATD
      if(ITLONG(1:1).eq.'W') TLONGD=-TLONGD
      GO TO 90
C ----------------------------------------------------------------------
C     RECEIVER CARD                                           ICARD=5
C ----------------------------------------------------------------------
  235 READ(INPUT,1505) IRCVR,RLATD,IRLAT,RLONGD,IRLONG,path,MIN
ccc      if(path.ne.'L') npsl=0     !  short path
ccc      if(path.eq.'L') npsl=1     !  long  path
       LOC=1
       IWCARD(5)=-1
       IF(MIN.EQ.IMIN) then
C.....ENTER WITH MINUTES AFTER DECIMAL PLACE
          RLATD=CMD(RLATD)
          RLONGD=CMD(RLONGD)
       end if
      if(IRLAT(1:1).eq.'S') RLATD=-RLATD
      if(IRLONG(1:1).eq.'W') RLONGD=-RLONGD
ccc      write(luo,'(''in DECRED, rlongd='',f8.3)') rlongd
      GO TO 90
C ----------------------------------------------------------------------
C     SYSTEM CARD                                             ICARD=6
C ----------------------------------------------------------------------
  270 IF(IWCARD(6).le.0) go to 900
      READ(INPUT,1507) PWX,AMIND,IANTR,BANTR
1507  format(10x,f10.4,f5.2,i5,f5.1)
      AMIN=AMIND* D2R
      APW='KW  '
      IWCARD(6)=-1
      GO TO 90
C ----------------------------------------------------------------------
C     NOISE CARD                                              ICARD=7
C ----------------------------------------------------------------------
  280 IF(IWCARD(7).le.0) go to 900
      READ(INPUT,1510) MAN,JBW,CJRSN,PERCENT
      LUF=50
      RLUF=0.5
      IF(PERCENT.EQ.0) PERCENT=50
      IF(PERCENT.LT.10) PERCENT=10
      IF(PERCENT.GT.90) PERCENT=90
      FRACX=FLOAT(PERCENT)*0.01
C.....RLUF=PRESET % PROBABILITY THAT S/N EXCEEDS REQ'D
C.....VALUE FOR CALCULATION OF LUF (RLUF=LUF/100)
C.....SET VARIABLES TO DEFAULT VALUES IF NOT SET ON SYSTEM CARD
      MAN=MIN0(MAN,6)
      IF(MAN.EQ.0) MAN=-151
      IF(JBW.LE.0) JBW=1
      BDWTH=FLOAT(JBW)
      IF(CJRSN.EQ.'     ') THEN
       IF(JBW.EQ.1) THEN
        JRSN=48
       ELSE
        JRSN=10
       END IF
      ELSE
       READ(CJRSN,1620) JRSN
 1620  FORMAT(I5)
      END IF
      RSN=FLOAT(JRSN)
      IWCARD(7)=-1
      GO TO 90
C ----------------------------------------------------------------------
C     FREQUENCY CARD                                          ICARD=8
C ----------------------------------------------------------------------
  340 IF(IWCARD(8).le.0) go to 900
      READ(INPUT,1511) (FREL(IFK),IFK=1,MAXF)
      IWCARD(8)=-1
      GO TO 90
C ----------------------------------------------------------------------
C     EXECUTE CARD                                            ICARD=9
C ----------------------------------------------------------------------
C.....THE "EXECUTE" CARD CAUSES PROGRAM EXECUTION
  410 READ(INPUT,1500)
      IRED=1
  415 CONTINUE
      DO 420 IC=1,MAXNAM
  420 IWCARD(IC)=1
      IEXEC=1
      LOC=0
      GO TO 920
C ----------------------------------------------------------------------
C     QUIT CARD                                               ICARD=10
C ----------------------------------------------------------------------
C.....THE "QUIT" CARD IS USED TO TERMINATE PROGRAM EXECUTION
  440 CONTINUE
      ITRUN=0
      GO TO 920
C ----------------------------------------------------------------------
C     MONTH CARD                                              ICARD=12
C ----------------------------------------------------------------------
  470 IF(IWCARD(12).le.0) go to 900
      READ(INPUT,1502) NYEAR, MONTHS
      IMONTH=1
      GO TO 90
C ----------------------------------------------------------------------
C     SUNSPOT CARD                                            ICARD=13
C ----------------------------------------------------------------------
  570 IF(IWCARD(13).le.0) go to 900
      READ(INPUT,1508) SSNS
      IWCARD(13)=-1
      ISSN=1
      GO TO 90
C ----------------------------------------------------------------------
C     LABEL CARD                                              ICARD=14
C ----------------------------------------------------------------------
  670 IF(IWCARD(14).le.0) go to 900
      READ(INPUT,1504) ITRANS, IRCVR
      IWCARD(14)=-1
      GO TO 90
C ----------------------------------------------------------------------
C     CIRCUIT CARD                                            ICARD=15
C ----------------------------------------------------------------------
  770 IF(IWCARD(15).le.0) go to 900
      READ(INPUT,1506)TLATD,ITLAT,TLONGD,ITLONG,RLATD,IRLAT,
     A RLONGD,IRLONG,NPSL,MIN
       LOC=1
       IWCARD(15)=-1
       IF(MIN.EQ.IMIN) then
C......ENTER WITH MINUTES AFTER DECIMAL PLACE
          TLATD=CMD(TLATD)
          TLONGD=CMD(TLONGD)
          RLATD=CMD(RLATD)
          RLONGD=CMD(RLONGD)
       end if
       if(ITLAT(1:1).eq.'S') TLATD=-TLATD
       if(ITLONG(1:1).eq.'W') TLONGD=-TLONGD
       if(IRLAT(1:1).eq.'S') RLATD=-RLATD
       if(IRLONG(1:1).eq.'W') RLONGD=-RLONGD
       GO TO 90
C ----------------------------------------------------------------------
C     ANTENNA CARD                                            ICARD =16
C ----------------------------------------------------------------------
780   READ(INPUT,781) IAT,iantr,minfreq,maxfreq,designfreq(iantr),
     +                antfile(iantr),beammain(iantr),pwrkw(iantr)
781   FORMAT(10X,4I5,f10.3,1x,a21,1x,f5.1,f10.4)
      iats(iantr)=iat
      if(iat.eq.1) then           !  transmitter
         if(pwrkw(iantr).le.0.) pwrkw(iantr)=1.
         pwrdba(iantr)=30. + 10.*alog10(pwrkw(iantr))     !  convert to dB
      end if
      numants=iantr
      write(gainfile,'(4hgain,i2.2,4h.dat)') iantr
      open(lu26,file=trim(run_directory)//PATH_SEPARATOR//gainfile,status='old',form='formatted',err=9900)
      rewind(lu26)
      read(lu26,782) anttype(iantr),antname(iantr)
782   format(a10,a70)
      read(lu26,787) xfqs(iantr),xfqe(iantr),
     +           beammain(iantr),offazim(iantr),cond(iantr),diel(iantr)
787   format(2f5.0,2f7.2,2f10.5)
      if(offazim(iantr).eq.-999.) then      !  area coverage
c          change made 8/31/95 for Windows version because gains are
c          stored in the correct arrays in antcalc
ccc         read(lu26,'(25x,f10.3)') aeff(1,iantr)
ccc         do 784 iazim=1,360
ccc         read(lu26,783) (array91(ielev),ielev=1,91)
ccc783      format(9x,10f7.3)
ccc         do 784 ielev=1,91              !  store in INTEGER*2 to save space
ccc         gain=array91(ielev)
ccc         if(gain.gt. 300.) gain= 300.
ccc         if(gain.lt.-300.) gain=-300.
ccc784      iarray360(iazim,ielev,iantr)=gain*100.
      else
         do 785 ifreq=1,30
785      read(lu26,786) AEFF(ifreq,iantr),(array(ifreq,j,iantr),j=1,91)
786      format(2x,f6.2,(t10,10f7.3))
      end if
      close(lu26)
      GO TO 90
C ----------------------------------------------------------------------
C     AREA    CARD                                            ICARD =17
C ----------------------------------------------------------------------
C.....AREA    card reads area coverage center and limits
790   nx=0
      ny=0
      READ(INPUT,791) platd,plond,xmin,xmax,yymin,ymax,nx,ny,itype
791   format(10x,6F10.0,3i5)
      if(nx.eq.0) nx=5
      if(ny.eq.0) ny=5
      iproj=7                   !  great circle projection
      if(itype.ne.0) iproj=8    !  lat/lon projection
      GO TO 90
C ----------------------------------------------------------------------
C     COEFFS  CARD                                            ICARD =18
C ----------------------------------------------------------------------
C.....COEFFS  card reads whether to use CCIR or URSI foF2 coefficients
795   coeff=INPUT(11:14)
      if(coeff.ne.'URSI') coeff='CCIR'
      GO TO 90
C ----------------------------------------------------------------------
C     FSLIMIT CARD                                            ICARD =19
C ----------------------------------------------------------------------
C.....Do not output records with FS < this value
c     For special ITU CIRAF method only
798   READ(INPUT,799) fs_limit
799   format(10x,F10.0)
      GO TO 90
C ----------------------------------------------------------------------
C     DIGITAL CARD                                           ICARD =20
C ----------------------------------------------------------------------
C.....Read digital modulation data
800   READ(INPUT,'(10x,3f10.0)') A_ratio,Tw,Fw
      idigital=1
      GO TO 90
C***********************************************************************
C ----------------------------------------------------------------------
C     (BLANK CARD)
C ----------------------------------------------------------------------
C.....A "(BLANK)" CARD ACTS AS A "DO-NOTHING" CARD AND IS IGNORED
  870 CONTINUE
ccc   if(input(1:4).eq.'COMM') write(lu16,'(1x,a)') input  !  comment card
      if(input(1:14).eq.'COMMENT   FREQ')
     +       write(lu16,'(1x,a)') input  !  freq comment card for perform2
      GO TO 90
C.....END OF NAMES
C.....EXECUTE PROGRAM IF TWO CONSECUTIVE CARDS WITH THE SAME NAME
  900 CONTINUE
      IRED=-1
      GO TO 415
C.....E R R O R IN THE INPUT-CARD NAME NOT FOUND IN THE CARD NAME LIST
  920 RETURN
9900  write(*,9901) gainfile
9901  format(' Could not OPEN file=',a)
      stop 'OPEN error in decred at 9900'
C.....FORMAT SPECIFICATIONS
 1500 FORMAT(10X,14I5)
 1501 FORMAT(20A4)
 1502 FORMAT(10X,1X,A4,12I5)
 1503 FORMAT(10X,2I5,2F6.1,I10,3I5)
 1504 FORMAT(10X,10A4)
 1505 FORMAT(10X,5A4,f7.2,a1,3x,f7.2,a1,2x,a1,5X,A1)
 1506 FORMAT(10X, 4(F5.2,A1,4X), I5, A1 )
 1508 FORMAT(10X,12F5.2)
 1509 FORMAT(10X,I5,2F5.1,5I5)
 1510 FORMAT(10X,2I5,A5,I5)
 1511 FORMAT(10X,10F7.3)
      END
c------------------------------------------------------------------

c# decred.f
      SUBROUTINE DECRED
C---------------------------------
C
C     THIS ROUTINE CALLS FUNCTION MONITR TO READ THE CONTROL CARDS.
C     THE CARD IMAGES ARE THEN DECODED , PROCESSED AND THE DATA STORED
C
      use voacapl_defs
      character cirafz*30,zones(100)*4
      common /cCIRAF_TP/ nTP,idx_TP(911)
      common /crun_directory/ run_directory
         character run_directory*50
      common /ccoeff/ coeff
      character coeff*4
      common /cdaily/ idaily(12)       !  day of the month modification
      common /carea/ iarea
      common /c_S_to_I/ i_S_to_I    !  = 1 = S/I calculation
      common /cmodel/ model
      character model*8
      common /cantenna/ numants,iats(20),anttype(20),antname(20),
     +                  xfqs(20),xfqe(20),designfreq(20),antfile(20),
     +                  beammain(20),offazim(20),cond(20),diel(20),
     +                  array(30,91,20),aeff(30,20)
      character anttype*10,antname*70,antfile*24,gainfile*10
      dimension array91(91)
      common /pantenna/ pwrkw(20),pwrdba(20)    !  antenna powers
      common /chours/ nhours,ihours(24)       !  which hours are active
      COMMON /RGRID/ IPROJ,PLAT,PLON,XMIN,XMAX,YYMIN,YMAX,NX,NY
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
      COMMON /CON/ D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON /ES /FS (3, 5), HS (5)
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON /GEOG /GYZ (5), RAT (5), GMDIP (5), CLCK (5), ABIY (5), ART
     1IC (5), SIGPAT (5), EPSPAT (5)
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON/LPATH/ GCDLNG,TXRGML(45,2),DELOPT,GMIN,YMIN,LTXRGM(2)
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
      COMMON / MFAC / F2M3(5),HPF2(5),ZENANG(5),ZENMAX(5),IEDP,FSECV(3)
      COMMON / MODES / GHOP, DELMOD(6,3), HPMOD(6,3), HTMOD(6,3),
     1 FVMOD(6,3), ITMOD(6,3), AFMOD(6,3)
      COMMON / NAMEX / NAMES(100), INPUT
      CHARACTER INPUT*85
      COMMON / OUTLAB / LABEL(11), LAYTYP(5), IEAST, INORTH, ISOUTH,
     A IWEST, LABLI, LABLJ, LABLK
      COMMON / OUTPRT / LINBOT(26), LINBD(14), LINTOP(15), LINTP(14),
     A GRPTYP, JOUT, LINBYP, LINES, LINMAX, LINTYP, LPAGES, NLINE
      COMMON / RAYS / ANG(40), IFOB(40,30,5), NANG
      COMMON/REFLX/DELFX(45,3),HPFLX(45,3),HTFLX(45,3),GDFLX(45,3),FVFLX
     A (45,3),DSKPKM(3),DELSKP(3),HPSKP(3),HTSKP(3),DMAXKM(3),FVSKP(3)
     B ,ISKP(3),IMODE(45,3),AFFLX(45,3),DELPEN(3,5),GML(45,3),FHP(45,3)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON /SSP /SUN (2, 12), MONTH
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      COMMON /TON /ADJ, ADS, GNOS, GOT, REL, SL, SLS
     1, SPR, SU, SUS, XNOISE, ZNOISE, NF
      COMMON / ZON / ABPS(7), CREL(7), EFF(7), FLDST(7), GRLOS(7),
     1 HN(7), HP(7), PROB(7), RELY(7), RGAIN(7), SIGPOW(7), SN(7),
     2 SPRO(7), TGAIN(7), TIMED(7), TLOSS(7), B(7), FSLOS(7), ADV(7),
     3 OBF(7),NMODE(7),TLLOW(7),TLHGH(7)
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON/INFORM/INFO,IHSHR,IHLNG
      COMMON / PSCA / PSC(4), PSCB(4), IPSC
      DIMENSION IWCRD(100),xmonth(12)
      CHARACTER INAME1*8, INAME2*8, JNAME1*8, JNAME2*8, IOFF*3,
     A IBLANK*1, ITEMP*10, ISLAT*1, ISLONG*1, ISGLAT*1
      CHARACTER INORTH*1,IEAST*1,ISOUTH*1,IWEST*1
      CHARACTER NAMES*10, LABEL*5,
     A LAYTYP*2, LABLI*5, LABLJ*5, LABLK*5
      DATA INAME1/'BOULDER,'/, INAME2/' COLO.  '/, JNAME1/'ST. LOUI'/,
     A JNAME2/'S, MO.  '/
      DATA IWCRD/100*1/,
     A IOFF/'OFF'/, IBLANK/' '/
C***********************************************************************
C
C     SUBROUTINE REDMAP MAY READ THE IONOSPHERIC LONG TERM DATA BASE
C     FILE (LU2) DEPENDENT ON TASK OPTION AND USER DEFINED INPUT
C     THE PROGRAM WILL TERMINATE IF AN ERROR OCCURS (SET ITRUN = 0)
C
C     ON THE FIRST CALL TO THIS SUBROUTINE CONTROL PASSES TO STATEMENT
C     LABEL  90 TO READ THE FIRST CONTROL CARD FROM LUI WHICH INITIALLY
C     IS SET TO LU5.  CONTROL THEN PASSES TO STATEMENT LABEL 105 TO
C     BRANCH AND PROCESS THE CONTROL CARD.
C
C***********************************************************************

      nch_run=lcount(run_directory,50)
C.....EXECUTE PROGRAM IF TWO CONSECUTIVE CARDS WITH SAME NAME
      IF(IRED.le.0) go to 105
   90 CONTINUE
C.....IF ISPROC = 0, CARDS ARE INPUT FROM THE USER DEFINED FILE (LUI)
C.....IF ISPROC = 1, CARDS ARE INPUT FROM THE PROCEDURE FILE (LU35)
      IF(ISPROC.gt.0) then
         ICARD = MONITR(LU35,MAXNAM)     !  read from procedure file
      else
         ICARD = MONITR(LUI,MAXNAM)      !  read from LUI
      end if
ccc      write(*,'(''  after monitr='',i3,''  maxnum,numnam='',2i5)')
ccc     +                icard,maxnum,numnam
      IF(LUI.ne.LU15) KCARD=KCARD+1
C.....ICARD IS AN INDEX OF THE CARD NAME
  105 IF(ICARD.GT.MAXNAM) GO TO 910
      IF(ICARD.GT.NUMNAM) GO TO 880
C.....ICARD = 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
C            16  17  18  19  20  21  22  23  24  25  26  27  28  29  30
C            31  32  33  34  35  36  37  38  39  40  41  42  43  44  45
C
C           MET MON MON SUN CIR SYS TIM CCI FRE LAB INT EXE SAM EFV ESV
C           EDP AUX AUX ANT OUT COM FRE PRO END NEX QUI FPR TOP BOT INF
C           ARE LIN COE ZON    
C
ccc      write(*,'('' before GO TO (..) icard='',i5)') icard
      GO TO(110,145,165,205,230,270,285,330,340,380,390,410,425,465,480,
     A      490,505,515,525,535,545,550,560,625,635,670,700,750,770,780,
     B      800,805,806,810,820), ICARD
C***********************************************************************
C     METHOD CARD                                             ICARD = 1
C***********************************************************************
C.....THE METHOD CARD IS USED FOR PROGRAM CONTROL
  110 IF(IWCRD(1).le.0) go to 900
      READ(INPUT,1500) METHOD, NPAGO, MSPEC
c          PCVOACAP cannot use MSPEC=125
      if(METHOD.gt.200) then
         i_S_to_I=1             !  S/I calculations
         METHOD=METHOD-200
      else if(METHOD.gt.100) then
         iarea=1                !  area coverage
         METHOD=METHOD-100
      end if
      if(MSPEC.eq.125 .and. model.eq.'PCVOACAP') MSPEC=0
      if(METHOD.eq.30) then            !  Short/Long Path smoothing
         MSPEC=121
         METHOD=20
      end if
C.....TERMINATE EXECUTION IF METHOD .LE. 0 OR .GT. MAXIMUM METHOD
C.....HOWEVER, A "QUIT" CARD IS REQUIRED AS THE LAST CONTROL CARD
      IF(METHOD.le.MAXMET) go to 125
      ITRUN = 0
      ITOUT = 0
      GO TO 920
C.....SET RUN AND OUTPUT INDICATORS DEPENDING ON METHOD
  125 ITRUN = JTRUN(METHOD)
      ITOUT = JTOUT(METHOD)
C.....SET STARTING PAGE NUMBER FOR OUTPUT FILE
      IF(NPAGO.gt.0) LPAGES = NPAGO-1
      IWCRD(1) = -1
      GO TO 90
C***********************************************************************
C     MONTH CARD                                              ICARD = 2
C***********************************************************************
  145 IF(IWCRD(2).le.0) go to 900
ccc      READ(INPUT,1502) NYEAR, (MONTHS(I),I=1,12)
      READ(INPUT,'(10x,a5,13f5.2)') NYEAR, (XMONTH(I),I=1,12)
      do 146 i=1,12
      if(xmonth(i).lt..005) then          !  no  month
         months(i)=0
         idaily(i)=0
      else if(xmonth(i).lt.1.) then       !  old month
         months(i)=nint(xmonth(i)*100.)
         idaily(i)=0
      else                                !  month.day
         monthx=xmonth(i)
         ida=nint((xmonth(i)-float(monthx))*100.)
         idaily(i)=ida                    !  if <>0, use daily foF2 coeff
         months(i)=monthx
         if(ida.gt.0) coeff='URSI'                !  daily MUST use URSI
      end if
146   continue
         
      IWCRD(2) = -1
      GO TO 90
C***********************************************************************
C     MONTHLOOP CARD                                          ICARD = 3
C***********************************************************************
  165 continue
      pause 'MONTHLOOP card no longer valid in VOACAP'
      stop  'MONTHLOOP card no longer valid in VOACAP'
C***********************************************************************
C     SUNSPOT CARD                                            ICARD = 4
C***********************************************************************
  205 IF(IWCRD(4).le.0) go to 900
      READ(INPUT,1508) (SUNSP(I),I=1,12)
      IWCRD(4) = -1
      go to 90
C***********************************************************************
C     CIRCUIT CARD                                            ICARD = 5
C***********************************************************************
  230 IF(IWCRD(5).le.0) go to 900
      READ(INPUT,1506) TLATD,ITLAT, TLONGD, ITLONG, RLATD, IRLAT,
     A RLONGD, IRLONG, NPSL
 1506 FORMAT(10X,F5.2,A1,3(F9.2,A1),4X,I5)
      IWCRD(5) = -1
      IF(ITLAT .EQ. IBLANK) GO TO 255
      IF(ITLONG .EQ. IBLANK) GO TO 255
      IF(IRLAT .EQ. IBLANK) GO TO 255
      IF(IRLONG .NE. IBLANK) GO TO 260
C.....CIRCUIT TRANSMITTER AND RECEIVER COORDINATES ARE BLANK
C.....SET COORDINATES TO THE FOLLOWING DEFAULT CIRCUIT
C.....BOULDER, COLORADO TO ST. LOUIS, MO.
  255 TLATD = 40.03
      ITLAT = INORTH
      TLONGD = 105.3
      ITLONG = IWEST
      RLATD = 38.67
      IRLAT = INORTH
      RLONGD = 90.25
      IRLONG= IWEST
      ITRAN(1) = INAME1
      ITRAN(2) = INAME2
      IRCVR(1) = JNAME1
      IRCVR(2) = JNAME2
C.....OUTPUT IN GEOM
  260 CONTINUE
      if(ITLAT.eq.ISOUTH) TLATD=-TLATD
      if(ITLONG.eq.IWEST) TLONGD=-TLONGD
      if(IRLAT.eq.ISOUTH) RLATD=-RLATD
      if(IRLONG.eq.IWEST) RLONGD=-RLONGD
      GO TO 90
C***********************************************************************
C     SYSTEM CARD                                             ICARD = 6
C***********************************************************************
  270 IF(IWCRD(6).le.0) go to 900
      READ(INPUT,1507) PWX, XNOISE, AMIND, XLUFP, RSNX, PMPX, DMPX
 1507 FORMAT(10X,F5.2,F5.0,F5.2,F5.0,3F5.2)
C.....SET VARIABLES TO DEFAULT VALUES IF NOT SET ON SYSTEM CARD
      IWCRD(6) = -1
      IF(XNOISE.NE.0.) NOISE = XNOISE
      LUFP = XLUFP
      IF(AMIND.LE.0.0) AMIND = 3.0
      AMIN = AMIND * D2R
      IF(LUFP.LE.0) LUFP = 90.
ccc      IF(RSNX.GT.0.) RSN = RSNX
ccc      IF(PMPX.NE.0.) PMP = PMPX
ccc      IF(DMPX.NE.0.) DMP = DMPX
      RSN = RSNX
      PMP = PMPX
      DMP = DMPX
      GO TO 90
C***********************************************************************
C     TIME CARD                                               ICARD = 7
C***********************************************************************
  285 IF(IWCRD(7).le.0) go to 900
      READ(INPUT,1500) IHRO, IHRE, IHRS, ITIM
      IWCRD(7) = -1
C.....SET HOUR INCREMENT TO THE DEFAULT VALUE OF ONE (IF .LE. 0)
      IF(IHRS.LE.0) IHRS = 1
c          New TIME concept by Greg Hand 5/17/93
c          Time may now run around midnight (e.g. 22 to 04)
      istart=IHRO
      if(istart.eq.0) istart=24
      iend=IHRE
      if(IHRE.lt.istart) iend=iend+24
      nhours=0
      do 287 ihr=istart,iend,IHRS
      jhr=ihr
      if(jhr.gt.24) jhr=jhr-24
      nhours=nhours+1
287   ihours(nhours)=jhr
      GO TO 90
C***********************************************************************
C     ANTENNA CARD                                            ICARD = 8
C***********************************************************************
  330 CONTINUE
      READ(INPUT,1510) IAT,iantr,minfreq,maxfreq,designfreq(iantr),
     +                 antfile(iantr),beammain(iantr),pwrkw(iantr)
 1510 FORMAT(10X,4I5,f10.3,1x,a21,1x,f5.1,f10.4)
      iats(iantr)=iat
      if(iat.eq.1) then             !  transmitter, save power
         if(pwrkw(iantr).le.0.) pwrkw(iantr)=1.
         pwrdba(iantr)=30. + 10.*alog10(pwrkw(iantr))    !  convert to dB
      end if
      if(iantr.gt.numants) numants=iantr
      write(gainfile,'(4hgain,i2.2,4h.dat)') iantr
ccc      write(*,'('' opening file='',a)') run_directory(1:nch_run)//
ccc     +                                         '\'//gainfile
      open(lu26,file=run_directory(1:nch_run)//PATH_SEPARATOR//gainfile,status='old',form='formatted')
      rewind(lu26)
      read(lu26,331) anttype(iantr),antname(iantr)
331   format(a10,a70)
      read(lu26,332) xfqs(iantr),xfqe(iantr),
     +           beammain(iantr),offazim(iantr),cond(iantr),diel(iantr)
332   format(2f5.0,2f7.2,2f10.5)
      if(offazim(iantr).eq.-999.) then      !  area coverage
c          change made 8/31/95 for Windows version because gains are
c          stored in the correct arrays in antcalc
ccc         read(lu26,'(25x,f10.3)') aeff(1,iantr)
ccc         do 334 iazim=1,360
ccc         read(lu26,333) (array91(ielev),ielev=1,91)
ccc333      format(9x,10f7.3)
ccc         do 334 ielev=1,91              !  store in INTEGER*2 to save space
ccc         gain=array91(ielev)
ccc         if(gain.gt. 300.) gain= 300.
ccc         if(gain.lt.-300.) gain=-300.
ccc334      iarray360(iazim,ielev,iantr)=gain*100.
      else
         do 335 ifreq=1,30
335      read(lu26,336) AEFF(ifreq,iantr),(array(ifreq,j,iantr),j=1,91)
336      format(2x,f6.2,(t10,10f7.3))
      end if
      close(lu26)
      GO TO 90
C***********************************************************************
C     FREQUENCY CARD                                          ICARD = 9
C***********************************************************************
  340 IF(IWCRD(9).le.0) go to 900
      if(INPUT(11:11).eq.'$') then           !  get 3 sig digits in frequency
         READ(INPUT,341) (FREL(IFK),IFK=1,11)
341      format(11x,11f6.3)
      else
         READ(INPUT,1508) (FREL(IFK),IFK=1,11)
      end if
      IWCRD(9) = -1
C.....SET REST OF FREQUENCY ARRAY
      DO 350 IFK = 12,29
  350 FREL(IFK) = 0.
C.....SET FREL(14) AS A FLAG
      IF(FREL(1).eq.0.) then
         IF(FREL(2).le.0.) then
C.....SET FLAG TO CALCULATE FREQUENCY COMPLEMENT IN FRQCOM EACH HOUR
            FREL(14) = -1.
         else
C.....SET FLAG TO USE FOT AS FIRST FREQUENCY FOR EACH HOUR
            FREL(14) = -10.
         end if
      else
C.....SET FLAG TO USE INPUT DATA
         FREL(14) = 1.
      end if
      GO TO 90
C***********************************************************************
C     LABEL CARD                                              ICARD = 10
C***********************************************************************
  380 IF(IWCRD(10)) 900, 900, 385
  385 READ(INPUT,1504) ITRAN(1), ITRAN(2), IRCVR(1), IRCVR(2)
      IWCRD(10) = -1
      GO TO 90
C***********************************************************************
C     INTEGRATE CARD                                          ICARD = 11
C***********************************************************************
C.....THE "INTEGRATE" CARD INDICATES WHAT KIND OF INTEGRATION IS USED
C.....INTEG = -1 FOR ALWAYS GAUSSIAN,
C.....INTEG .GE. 0 FOR MODEL SEGMENT WHEN NO F1 LAYER
  390 CONTINUE
      READ(INPUT,1504) ITEMP
      INTEG = 1
      IEDP = 1
      IF(ITEMP .EQ. IOFF) GO TO 90
      READ(INPUT,1500) INTEG
      IEDP = INTEG
      GO TO 90
C***********************************************************************
C     EXECUTE CARD                                            ICARD = 12
C***********************************************************************
C.....THE "EXECUTE" CARD CAUSES PROGRAM EXECUTION
  410 READ(INPUT,1500) KRUN
      IRED = 1
  415 DO 420 IC = 1,MAXNAM
  420 IWCRD(IC) = 1
      GO TO 920
C***********************************************************************
C     SAMPLE CARD                                             ICARD = 13
C***********************************************************************
C.....SAMPLE FOR GEOGRAPHICAL VARIABLES
  425 CONTINUE
C.....THE USER MAY RUN METHOD= 1 TO OBTAIN THESE VALUES FROM THE
C.....IONOSPHERIC LONG TERM DATA BASE FILE (LU2) . HE MAY THEN PUNCH THE
C.....VALUES ON DATA CARDS INSTEAD OF READING THE DATA BASE FILE.
  430 READ(INPUT,1514) I, SLAT, ISLAT, SLONG, ISLONG, SGLAT,
     A ISGLAT, RD(I), GYZ(I), CLCK(I), GMDIP(I), ARTIC(I), SIGPAT(I),
     B EPSPAT(I)
      IF(ISLAT .EQ. ISOUTH) SLAT = -SLAT
      CLAT(I) = SLAT * D2R
      IF(ISLONG .EQ. IWEST) SLONG = -SLONG
      CLONG(I) = SLONG * D2R
      IF(ISGLAT .EQ. ISOUTH) SGLAT = -SGLAT
      GLAT(I) = SGLAT * D2R
      GO TO 90
C***********************************************************************
C     EFVAR CARD                                              ICARD = 14
C***********************************************************************
C.....SAMPLE FOR E, F1, F2 LAYERS, FREQ, SEMI-THICKNESS, HEIGHT OF MAX
  465 CONTINUE
C.....THE USER MAY RUN METHOD= 1 TO OBTAIN THESE VALUES FROM THE
C.....IONOSPHERIC LONG TERM DATA BASE FILE (LU2) . HE MAY THEN PUNCH THE
C.....VALUES ON DATA CARDS INSTEAD OF READING THE DATA BASE FILE.
  470 READ(INPUT,1516) I,(FI(J,I), YI(J,I), HI(J,I),J=1,3)
      F2M3(I) = 1490. / (HI(3,I) + 176.)
      HPF2(I) = HI(3,I)
      GO TO 90
C***********************************************************************
C     ESVAR CARD                                              ICARD = 15
C***********************************************************************
C.....SAMPLE FOR ES LAYER, LOW, MIDDLE, HIGH, VIRTUAL HEIGHT
  480 CONTINUE
C.....THE USER MAY RUN METHOD= 1 TO OBTAIN THESE VALUES FROM THE
C.....IONOSPHERIC LONG TERM DATA BASE FILE (LU2) . HE MAY THEN PUNCH THE
C.....VALUES ON DATA CARDS INSTEAD OF READING THE DATA BASE FILE.
  485 READ(INPUT,1518) I, FS(1,I), FS(2,I), FS(3,I), HS(I)
      GO TO 90
C***********************************************************************
C     EDP CARD                                                ICARD = 16
C***********************************************************************
C.....THE "EDP" CARD ALLOWS THE USER TO READ IN AND USE AN EXTERNAL
C.....ELECTRON DENSITY PROFILE FOR A SPECIFIED SAMPLE AREA
C.....(FOR ONE HOP PATH ONLY)
  490 CONTINUE
      READ(INPUT,1530) JSAMP, ITEMP
C.....ASSUME THE DEFAULT SAMPLE AREA = 1
C.....IF THE SPECIFIED SAMPLE AREA ISN"T 1, 2 OR 3
      IF(JSAMP) 494, 494, 492
  492 IF(JSAMP - 3) 496, 496, 494
  494 JSAMP = 1
  496 IF(ITEMP .NE. IOFF) GO TO 500
C.....USE THE INTERNAL ELECTRON DENSITY PROFILE INSTEAD OF THE ONE READ
      IELECT(JSAMP) = 0
      GO TO 90
C.....READ THE EXTERNAL ELECTRON DENSITY PROFILE
  500 IELECT(JSAMP) = 1
C.....N O T E    CHANGED TO HTR(50,3) AND FNSQ(50,3)
      READ(LUI,1528) (HTR(I,1),I=1,50)
      READ(LUI,1528) (FNSQ(I,1),I=1,50)
      GO TO 90
C***********************************************************************
C     AUXIN CARD                                              ICARD = 17
C***********************************************************************
C.....THE "AUXIN" CARD CAUSES INPUT CARD IMAGES TO BE READ FROM THE
C.....AUXILLARY INPUT FILE
  505 CONTINUE
      READ(INPUT,1504) ITEMP
      LUI = LU15
      IF(ITEMP .NE. IOFF) GO TO 90
      LUI = LU5
      GO TO 90
C***********************************************************************
C     AUXOUT CARD                                             ICARD = 18
C***********************************************************************
C.....THE "AUXOUT" CARD CAUSES THE OUTPUT FILE TO BE CHANGED FROM THE
C.....STANDARD OUTPUT FILE TO THE ALTERNATE OUTPUT FILE
  515 CONTINUE
      READ(INPUT,1504) ITEMP
      LUO = LU16
      IF(ITEMP .NE. IOFF) GO TO 90
      LUO = LU6
      GO TO 90
C***********************************************************************
C     ANTOUT CARD                                             ICARD = 19
C***********************************************************************
C.....THE "ANTOUT" CARD INDICATES THE ANTENNA PATTERNS CREATED
C.....ARE TO BE WRITTEN ON A FILE FOR LATER USE
  525 CONTINUE
      READ(INPUT,1504) ITEMP
      IANTOU = 1
      IF(ITEMP .NE. IOFF) GO TO 90
      IANTOU = 0
      GO TO 90
C***********************************************************************
C     OUTGRAPH CARD                                           ICARD = 20
C***********************************************************************
C.....THE "OUTGRAPH" CARD ALLOWS THE USER TO REQUEST OUTPUT OF SEVERAL
C.....DIFFERENT METHODS THAT HAVE OUTPUT OPTIONS 3 OR 4
  535 CONTINUE
      READ(INPUT,1504) ITEMP
      ISOUT = 0
      IF(ITEMP .EQ. IOFF) GO TO 90
      ISOUT = 1
      READ(INPUT,1500) KTOUT
      GO TO 90
C***********************************************************************
C     COMMENT CARD                                            ICARD = 21
C***********************************************************************
C.....THE "COMMENT" CARD ALLOWS THE USER TO PUT COMMENTS IN THE INPUT
C.....FILE. IT HAS NO EFFECT ON PROGRAM EXECUTION
  545 CONTINUE
      if(input(11:14).eq.'GROU') write(LUO,'(11h COMMENT   ,a)') 
     +        input(11:)
      GO TO 90
C***********************************************************************
C     FREEFORM                                                ICARD = 22
C***********************************************************************
C.....THE "FREEFORM" CARD INDICATES THAT THE INPUT CONSISTES OF
C.....FREEFORM CARD IMAGES
C     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C     N O T E - THE FREEFORM INPUT ANALYSER IS NOT YET DEVELOPED
C     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  550 CONTINUE
      READ(INPUT,1504) ITEMP
      ITYPE = 1
      IF(ITEMP .NE. IOFF) GO TO 90
      ITYPE = 0
      GO TO 90
C***********************************************************************
C     PROCEDURE CARD                                          ICARD = 23
C***********************************************************************
C.....THE "PROCEDURE" CARD ALLOWS THE USER TO DEFINE A PROCEDURE
C.....THE PROCEDURE IS STORED ON LU35 AND WHEN A CALL TO THE PROCEDURE
C.....OCCURS (IE THE USER SPECIFIES THE NAME OF THE PROCEDURE ON AN
C.....INPUT CARD) THE PROGRAM READS THE PROCEDURE FILE AND REPLACES
C.....THE PROCEDURE CALL WITH THE PROCEDURE DEFINITION
  560 CONTINUE
C.....PROCEDURE FILE HAS ALREADY BEEN CREATED IN SUBROUTINE LISTIN
C.....IGNORE THE PROCEDURE DEFINITION ON THE INPUT FILE
  590 JCARD = MONITR(LUI,MAXNAM)
      KCARD = KCARD + 1
C.....SKIP PAST THE "END" CARD
      IF(JCARD - 24) 590, 90, 590
C***********************************************************************
C     END CARD                                                ICARD = 24
C***********************************************************************
C.....THE "END" CARD IS USED TO END A PROCEDURE DEFINITION
  625 CONTINUE
      ISPROC = 0
      GO TO 90
C***********************************************************************
C     NEXT CARD                                               ICARD = 25
C***********************************************************************
C.....THE "NEXT" CARD TERMINATES A MONTH SUNSPOT LOOP
  635 CONTINUE
      pause 'NEXT card no longer valid in VOACAP'
      stop  'NEXT card no longer valid in VOACAP'
ccc      GO TO 90
C***********************************************************************
C     QUIT CARD                                               ICARD = 26
C***********************************************************************
C.....THE "QUIT" CARD IS USED TO TERMINATE PROGRAM EXECUTION
  670 CONTINUE
      ITRUN = 0
      GO TO 920
C***********************************************************************
C     FPROB CARD                                              ICARD = 27
C***********************************************************************
C.....THE "FPROB" CARD ALLOWS THE USER TO ALTER THE PREDICTED CRITICAL
C.....FREQUENCIES. EACH CRITICAL FREQUENCY IS MULTIPLIED BY PSC(N),
C.....WHERE N = 1 IS E, N=2 IS F1, N=3 IS F2 AND N=4 IS ES.
C.....(N O T E -  E AND F2 ARE NECESSARY)
  700 CONTINUE
      READ(INPUT,1504) ITEMP
      IF(ITEMP .EQ. IOFF) GO TO 720
      IPSC = 1
      READ(INPUT,1508) PSC
      IF(PSC(1).LE.0.) PSC(1) = PSCB(1)
      IF(PSC(2).LT.0.) PSC(2) = PSCB(2)
      IF(PSC(3).LE.0.) PSC(3) = PSCB(3)
      IF(PSC(4).LT.0.) PSC(4) = PSCB(4)
      GO TO 90
  720 IPSC = 0
      DO 730 I = 1,4
  730 PSC(I) = PSCB(I)
      GO TO 90
C***********************************************************************
C     TOPLINES CARD                                           ICARD = 28
C***********************************************************************
C.....THE "TOPLINES" CARD ALLOWS THE USER TO SPECIFY THE LINES PRINTED
C     IN SUBROUTINE OUTTOP (WHENEVER METHOD = 23)
  750 CONTINUE
      READ(INPUT,1504) ITEMP
      LINTYP = 0
      IF(ITEMP .EQ. IOFF) GO TO 90
      LINTYP = 1
      READ(INPUT,1500) LINTP
      GO TO 90
C***********************************************************************
C     BOTLINES CARD                                           ICARD = 29
C***********************************************************************
C.....THE "BOTLINES" CARD ALLOWS THE USER TO SPECIFY THE LINES PRINTED
C     IN SUBROUTINE OUTBOD (WHENEVER METHOD = 23)
  770 CONTINUE
      READ(INPUT,1504) ITEMP
      LINBYP = 0
      IF(ITEMP .EQ. IOFF) GO TO 90
      LINBYP = 1
      READ(INPUT,1500) LINBD
      GO TO 90
C***********************************************************************
C     INFORMATION CARD                                        ICARD = 30
C***********************************************************************
C     N O T E - THIS OPTION IS NOT AVAILABLE TO ALL USERS AND IF NOT
C     AVAILABLE ACTS AS A "DO-NOTHING" CARD AND IS IGNORED
C
C     1.  MODE GENERATION SUBROUTINE.                     SHORT
C     2.  OVER-THE-MUF ABSORPTION RATES.                  BOTH
C     4.  SELTXR (0.05 to 0.5) CHANGE RESULTS.            LONG
C         or LUCAS' LETTER FOR HIGH ORDER MUFS ONLY       SHORT
C     8.  LUCAS' LETTER CHANGE RESULTS.                   BOTH
C    16.  LONG PATH/SHORT PATH SMOOTHING VALUES.          BOTH (MSPEC=121)
C    32.  RAYSET (PAGE 23)                                BOTH
C    64.  V. IONOGRAM AND REFLECTRIX (PAGE 21)            BOTH
C   128.  EXTRA LONG PATH PARAMETERS                      LONG
C
  780 READ(INPUT,1500)INFO
      IHSHR=IAND(INFO,1)+IAND(INFO,2)+IAND(INFO,32)+IAND(INFO,128)
      IHLNG=IAND(INFO,2)+IAND(INFO,8)+IAND(INFO,32)+IAND(INFO,128)
            GO TO 90
C***********************************************************************
C     AREA    CARD                                            ICARD = 31
C***********************************************************************
C.....AREA    card reads area coverage center and limits
800   nx=0
      ny=0
      READ(INPUT,801) plat,plon,xmin,xmax,yymin,ymax,nx,ny,itype
801   format(10x,6F10.0,3i5)
      if(nx.eq.0) nx=5
      if(ny.eq.0) ny=5
      iproj=7               !  great circle projection
      if(itype.ne.0) iproj=8         !  lat/lon for GRIB format
      iarea=1
      GO TO 90
C***********************************************************************
C     LINEMAX CARD                                            ICARD = 32
C***********************************************************************
C.....LINEMAX card reads maximum lines per page
805   READ(INPUT,'(10x,i5)') LINMAX
      if(LINMAX.lt.30) LINMAX=55
      GO TO 90
C***********************************************************************
C     COEFFS  CARD                                            ICARD = 33
C***********************************************************************
C.....COEFFS card allows either CCIR or URSI88 foF2 coefficients
806   coeff=INPUT(11:14)
      if(coeff.ne.'URSI') coeff='CCIR'
      GO TO 90
C***********************************************************************
C     (ZONES) CARD                                            ICARD = 34
C***********************************************************************
C.....CIRAF zone list to calculate values at CIRAF test points
  810 CONTINUE
      call rdCIRAF           !  read the 911 CIRAF test points
      cirafz=input(11:40)
      call parse_zones(cirafz,zones,nzones)
      call zones_to_TP(zones,nzones,idx_TP,nTP)  !  convert zones to Test Points
      GO TO 90
C***********************************************************************
C     (BLANK) CARD                                            ICARD = 35
C***********************************************************************
C.....A "(BLANK)" CARD ACTS AS A "DO-NOTHING" CARD AND IS IGNORED
  820 CONTINUE
      GO TO 90
C***********************************************************************
C     (USER DEFINED PROCEDURE NAME) CARD
C***********************************************************************
C.....THE "(USER DEFINED PROCEDURE NAME)" CARD PLACES THE CARD IMAGES
C     DEFINED BY THE USER PROCEDURE INTO THE INPUT STREAM
  880 CONTINUE
      REWIND LU35
      ISPROC = 1
C.....LOCATE THE DESIRED PROCEDURE ON THE PROCEDURE FILE
  890 JCARD = MONITR(LU35,MAXNAM)
      IF(JCARD - ICARD) 890, 90, 890
C.....END OF NAMES
C.....EXECUTE PROGRAM IF TWO CONSECUTIVE CARDS WITH THE SAME NAME
  900 CONTINUE
      IRED = -1
      GO TO 415
C.....E R R O R IN THE INPUT - CARD NAME NOT FOUND IN THE CARD NAME LIST
  910 WRITE(LUO,1520)
      ITRUN = 0
  920 continue
      do 930 i=1,12   !  if any daily predictions, must use URSI
         if(idaily(i).ne.0) then
            coeff='URSI'
         end if
930   continue
      RETURN
C.....FORMAT SPECIFICATIONS
 1401 FORMAT(A80)
 1402 FORMAT(' ','MONTH =',I5,5X,'MONTH INDEX =',I5)
 1403 FORMAT(' ',8A10,5X,'ICARD = ',I5)
 1406 FORMAT(' ','SSN = ',F7.2)
 1500 FORMAT(10X,14I5)
 1502 FORMAT(10X,A5,13I5)
 1504 FORMAT(10X,4A10)
 1508 FORMAT(10X,13F5.2)
 1514 FORMAT(10X,I5,3(F5.2,A1,4X),F5.0,6F5.2)
 1516 FORMAT(10X,I5,3(F5.2,2F5.1))
 1518 FORMAT(10X,I5,3F5.2,F5.1)
 1520 FORMAT(///,' NAME IDENTIFIER ERRORS DETECTED ON INPUT FILE')
 1528 FORMAT(16F5.2)
 1530 FORMAT(10X,I5,A10)
      END
C--------------------------------

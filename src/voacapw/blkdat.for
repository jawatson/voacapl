C--------------------------------
      BLOCK DATA    
C--------------------------------
      common /ccoeff/ coeff
      character coeff*4
      COMMON /A11/GAMMA (6)
C  OUTPUT OF NUMERICAL MAPS (SEE SUBROUTINE VERCY)
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
C  NOISE VALUES, SUBRS ANOIS1 AND GENOIS.
      COMMON/ANOIS/ATNU,ATNY,CC,TM,RCNSE,DU,DL,SIGM,SxGU,SxGL,KJ,JK
C CONSTANTS,SET BELOW.
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
C WEIGHTS AND ABSCISSCAE FOR 40 POINT GUASSIAN SET BELOW.
      COMMON / DATR / WT(20), XT(20), NT, NPL, XNPL, TWDIV
C GENERAL GEOGRAPHIC VARIABLES, SEE SUBR GEOM AND HFMUFES.
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON /FONE/ANEW(3),BNEW(3),ACHI(2),BCHI(2)
      COMMON / OUTLAB / LABEL(11), LAYTYP(5), IEAST, INORTH, ISOUTH,
     A IWEST, LABLI, LABLJ, LABLK
      CHARACTER IEAST*1, ISOUTH*1, INORTH*1, IWEST*1, LABEL*5, LAYTYP*2,
     A LABLI*5, LABLJ*5, LABLK*5
C SPORADIC E LAYER, SEE SUBR. ESIND.
      COMMON /ES /FS (3, 5), HS (5)
C  INPUT AND OUTPUT FILE NUMBERS
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
C FREQUENCY COMPLEMENT,SEE SUBR. HFMUFES.
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
C GEOGRAPHIC VARIABLES AT SAMPLE AREAS,SEE GEOM,TIMVAR,MAGVAR AND   LUFFY.
      COMMON /GEOG /GYZ (5), RAT (5), GMDIP (5), CLCK (5), ABIY (5), ART
     1IC (5), SIGPAT (5), EPSPAT (5)
      COMMON/INFORM/INFO,IHSHR,IHLNG
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON/LOSX/ANDVX(45,3),ADVX(45,3),AOFX(45,3),ARFX(45,3),GRLOSX(45
     A ,3),TGAINX(45,3),TLSKM(45,3),EFFlp(45),IAFTXR(3)
C  LONG PATH PARAMETERS,SEE SUBR LNGPAT.
      COMMON/LPATH/ GCDLNG,TXRGML(45,2),DELOPT,GMIN,YMIN,LTXRGM(2)
C  PROGRAM VERSION NUMBER, PROGRAM CONTROL VARIABLES
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
C  F2 LAYER PARAMETERS (SEE SUBROUTINE F2VAR)
      COMMON / MFAC / F2M3(5),HPF2(5),ZENANG(5),ZENMAX(5),IEDP,FSECV(3)
      COMMON /MODES/GHOP, DELMOD(6,3),HPMOD(6,3),HTMOD(6,3),FVMOD(6,3),
     1 ITMOD (6, 3), AFMOD (6, 3)
C  CIRCUIT MUFS
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     1,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMU
     2F(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     3 ,YMUF(4)
C  INPUT/OUTPUT CONTROL PARAMETERS
      COMMON / NAMEX / NAMES(100), INPUT
      CHARACTER NAMES*10,INPUT*85
      COMMON / OUTFMT / KLINE(26)
      CHARACTER KLINE*6
C  PARAMETERS FOR LINE PRINTER PLOTS
      COMMON / OUTGH / PLOTI(24), PLOTJ(24), PLOTK(24), XOUT(13), NPLOTS
C  LAT AND LONG DIRECTION INDICATORS, LINE NUMBER AND MAXIMUM LINES
      COMMON / OUTPRT / LINBOT(26), LINBD(14), LINTOP(15), LINTP(14),
     A GRPTYP, JOUT, LINBYP, LINES, LINMAX, LINTYP, LPAGES, NLINE
C  PREDICTED CRITICAL FREQUENCY MULTIPLIERS
      COMMON / PSCA / PSC(4), PSCB(4), IPSC
C OBLIQUE FREQUENCIES,SEE BELOW AND SUBR. GENION
      COMMON/RAYS/ANG(40),IFOB(40,30,5),NANG
      COMMON/REFLX/DELFX(45,3),HPFLX(45,3),HTFLX(45,3),GDFLX(45,3),FVFLX
     A (45,3),DSKPKM(3),DELSKP(3),HPSKP(3),HTSKP(3),DMAXKM(3),FVSKP(3)
     B ,ISKP(3),IMODE(45,3),AFFLX(45,3),DELPEN(3,5),GML(45,3),FHP(45,3)
C GEOGRAPHIC AND IONSPHERIC DATA AT SAMPLE AREAS,SEE GEOM AND GENION.
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON/SIGD/ DSL,ASM,DSU,AGLAT,DSLF,ASMF,DSUF,ACAV,FEAV,AFE,BFE
     A ,HNU,HTLOSS,XNUZ,XVE
C LATITUDES OF SUBSOLAR POINTS AND MONTH,SEE BELOW AND SUBR. HFMUFES
      COMMON / SSP / SUN(2,12), MONTH
C TIME VARIABLES, SEE HFMUFES.
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
C AURORAL LOSS, POWER AND NOISE DATA, SEE SYSSY, HFMUFES AND RELBIL.
      COMMON /TON /ADJ, ADS, GNOS, GOT, REL, SL, SLS
     1, SPR, SU, SUS, XNOISE, ZNOISE, NF
C  LONG TERM NOISE DATA
      COMMON / TWO / F2D(16,6,6), P(29,16,8), ABP(2,9), DUD(5,12,5),
     A FAM(14,12), SYS(9,16,6), PERR(9,4,6)
C  LOSSES ETC. FOR MODES.  SEE SUBR. REGMOD. SEE ALSO COMMON/ZON/.
      COMMON / allMODE /ABPS(20),CREL(20),FLDST(20),HN(20),HP(20),
     1PROB(20),RELY(20),RGAIN(20),SIGPOW(20),SN(20),
     2SPRO(20),TGAIN(20),TIMED(20),TLOSS(20),B(20),FSLOS(20),
     3grlos(20),adv(20),obf(20),
     CNMODE(20),TLLOW(20),TLHGH(20),EFF(20),NREL,NMMOD
C ALL MODES OUTPUT ARRAYS
CPC      COMMON/INDICEZ/ISP,ISZ,IFQN,IUR,NMS(11,24,4,2)
CPC      COMMON/SPALMOUT/RNZ(11,24,4,2),DUZ(11,24,4,2),DLZ(11,24,4,2),
CPC     -ZTL(20,11,24,4,2),BBZ(20,11,24,4,2), ZLO(20,11,24,4,2),
CPC     -ZHG(20,11,24,4,2)
CPC      COMMON/LPALMOT2/zACAV(24,4,2),zASM(24,4,2),zDSL(24,4,2),
CPC     +zDSU(24,4,2),zYMUF(4,24,4,2),zYFOT(4,24,4,2),zYHPF(4,24,4,2),
CPC     +zFVMUF(4,24,4,2),zSIGL(4,24,4,2),zSIGU(4,24,4,2)
CPC      common/lpalmcmp/imddl(45,2,11,24,4,2),igdfx(45,2,11,24,4,2),
CPC     +itlos(45,2,11,24,4,2),ihpfx(45,2,11,24,4,2),lNANG(2,11,24,4,2)
CPC      integer*2 imddl,igdfx,itlos,ihpfx,lNANG
C
C SET PREDEFINED CONSTANTS.
C
      data coeff/'CCIR'/        !  default coefficients
C     CONSTANTS
C125PC      DATA ISP,ISZ,IFQN,IUR /2,4,2*0/, INFO/0/
      DATA INFO/0/
      DATA XT      /0.0387724175,0.1160840707,0.1926975807,0.2681521850,
     1              0.3419940908,0.4137792043,0.4830758017,0.5494671251,
     2              0.6125538897,0.6719566846,0.7273182552,0.7783056514,
     3              0.8246122308,0.8659595032,0.9020988070,0.9328128083,
     4              0.9579168192,0.9772599500,0.9907262387,0.9982377097/
      DATA WT      /0.0775059480,0.0770398182,0.0761103619,0.0747231691,
     1              0.0728865824,0.0706116474,0.0679120458,0.0648040135,
     2              0.0613062425,0.0574397691,0.0532278470,0.0486958076,
     3              0.0438709082,0.0387821680,0.0334601953,0.0279370070,
     4              0.0222458492,0.0164210584,0.0104982845,0.0045212771/
      DATA JTRUN / 1,2,4*3,5*4,5,3*6,10*7,5*8,10*0/,
     A     JTOUT / 1,2,3,3*4,10,4*4,5,3*6,8*7,8,9,3,3*4,11,10*0/
      DATA ITRUN/0/, ITOUT/0/
      DATA RZ/6370./,PI/3.1415926/,VOFL/299.79246/, DCL/1.28/
      DATA GAMA/0.57721566/,PI2/6.283185307/,PIO2/1.570796326/
      DATA D2R/.01745329251/, R2D/57.295779513/
      DATA ANG/0., 0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0, 12.0
     1,14.,16.,18.,20.,22.,24.,26.,28.,30.,32.,34.,36.,38.,40.,42.,44.
     2 ,46.,48.,50.,52.,54.,56.,60.,65.,70.,75.,80.,85.,89.99/
      DATA IMON /'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     A 'Oct','Nov','Dec' /
      DATA NAMES / 'METHOD    ', 'MONTH     ', 'MONTHLOOP ',
     A 'SUNSPOT   ', 'CIRCUIT   ', 'SYSTEM    ', 'TIME      ',
     B 'ANTENNA   ', 'FREQUENCY ', 'LABEL     ', 'INTEGRATE ',
     C 'EXECUTE   ', 'SAMPLE    ', 'EFVAR     ', 'ESVAR     ',
     D 'EDP       ', 'AUXIN     ', 'AUXOUT    ', 'ANTOUT    ',
     E 'OUTGRAPH  ', 'COMMENT   ', 'FREEFORM  ', 'PROCEDURE ',
     F 'END       ', 'NEXT      ', 'QUIT      ', 'FPROB     ',
     G 'TOPLINES  ', 'BOTLINES  ', 'INFORM    ',
     H 'AREA      ', 'LINEMAX   ', 'COEFFS    ', 'ZONES     ',
     H 66*'          '/
      DATA LABEL / '  ANG', ' EMUF', 'ESMUF', '  FOT', 'F1MUF', 'F2MUF',
     A '  HPF', '  LUF', ' MODE', '  MUF', '     ' /
      DATA LAYTYP /' E', 'F1', 'F2', 'ES', ' N'/
c%lc:gsp, change 1b     12 Jan 1995,                          blockdata blkdat
c%lc      fdays basis changed to hop muf, not path. Name changed accordingly.
c%lc      DATA KLINE/'MODE  ','ANGLE ','DELAY ','V HITE','F DAYS',
      DATA KLINE/'MODE  ','TANGLE','DELAY ','V HITE','MUFday',
     A           'LOSS  ','DBU   ','S DBW ','N DBW ','SNR   ',
     B           'RPWRG ','REL   ','MPROB ','S PRB ','SIG LW',
     B           'SIG UP','SNR LW','SNR UP','TGAIN ','RGAIN ',
     c           'SNRxx ','DBM   ','RANGLE','N dl  ','N du  ','      '/
c%lc. end change 1b
      DATA PSCB/1.,1.,1.,0.7/, PSC/1.,1.,1.,0.7/, IPSC/0/
      DATA SUN/-23.05,-17.31,-17.30,-7.89,-7.88,4.21,4.26,
     1 14.83, 14.84, 21.93, 21.93, 23.45, 23.15, 18.23, 18.20, 8.68, 8.5
     2 5, -2.86, -2.90, -14.16, -14.20, -21.68, -21.66, -23.45/
      DATA GCDLNG/10000./,DELOPT/3./,GMIN/3./,YMIN/.1/
      DATA ADJ/1./,ADS/1./,GNOS/1./,GOT/1./
      DATA RCNSE/-204./,REL/.01/,SL/1./,SLS/1./,SPR/.01/,SU /1./,SUS/1./
      DATA XNOISE/1./
      data noise/-4/   !  default=remote
      DATA ZNOISE/-204./, NF/1/, FLUX/1./, DMP/0.85/
      DATA ERTR/1./, PMP/10./, PWR/1./, RSN/1./, SIGTR/1./
      DATA ATNU/-204./,ATNY/-204./,CC/1./,TM/1./
      DATA DU/9./,DL/4./,SIGM/1.5/,SXGL/1./,SXGU/1./,KJ/1/,JK/1/
      DATA IELECT/0,0,0/
      DATA IHRO,IHRE,IHRS/12,24,12/, ITIM/-1/, LINES/55/
      DATA LPAGES/0/, FREL(14)/-1./
      DATA JOUT/20/, INORTH/'N'/, IEAST/'E'/, ISOUTH/'S'/, IWEST/'W'/,
     A NLINE/55/, LINMAX/55/, ITYPE/0/, JLONG/-1/
      DATA MONTH/0/, NYEAR/'     '/, SSN/0./
      DATA ICARD/0/, KCARD/0/, MOREM/0/, MORES/0/, MAPIN/-1/, MONOLD/0/,
     A INTEG/1/, IANTOU/0/, ISOUT/0/, NUPROC/0/, ISPROC/0/, ISSN/0/,
     B NUMNAM/34/, MAXNAM/100/, MAXMET/30/, IRED/1/, MSPEC/0/,M100/75/
      DATA NMMOD/1/, NREL/1/, IEDP/-1/
      DATA AMIND/3./, AMIN/.05236/
      DATA IT/1/, JFREQ/0/, JTX/1/, KM/1/, KFX/1/, KRUN/0/, LTXRGM/1,1/,
     A METHOD/-1/, LINBYP/0/, LINTYP/0/, NANG/0/, NPLOTS/0/
C
C     PRESET TO BOULDER, COLO. TO ST. LOUIS, MO. SO THAT A CIRCUIT CARD
C     IS NOT NECESSARY.
      DATA ITRAN/'BOULDER, C','OLORADO   '/,
     1     IRCVR/'ST. LOUIS,',' MISSOURI '/
      DATA TLATD/40.03/, ITLAT/'N'/, TLONGD/105.3/, ITLONG/'W'/,
     A RLATD/38.67/, IRLAT/'N'/, RLONGD/90.25/, IRLONG/'W'/, NPSL/0/
      DATA TLAT/.6986553/,TLONG/-1.837308/,RLAT/.6749188/
      DATA RLONG/-1.57516/,GCDKM/1301.1/,GCD/.204254/,BTR/1.60291/
      DATA BTRD/91.84/, BRTD/281.42/
      DATA RD/.102127,.0,.0,.0,.0/,GLAT/.862245,.0,.0,.0,.0/
      DATA CLAT/.691011,.0,.0,.0,.0/,CLONG/-1.70495,.0,.0,.0,.0/
      DATA SIGPAT/.001,.001,.001,.001,.001/,EPSPAT/4.,4.,4.,4.,4./
      DATA ALATD/39.592/,GYZ/1.358,.0,.0,.0,.0/
      DATA GMDIP/.959931,.0,.0,.0,.0/
C.....ELIMINATE D-REGION ABSORPTION ON ES ONLY
C.....PRESET FOR EFFECTIVE ELIMINATION OF LAYERS USING "KRUN" OPTION
      DATA FS/15*0./, FI/10*0.,5*0.2/, HI/5*110.,5*0.,5*300./,
     A YI/5*20.,5*0.,5*100./
      DATA LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35/15,16,2,11,12,15,16,20,25,26,35/
C
C---------------------------------
C
C     PROGRAM VERSION NUMBER
C
C
CC---------------------------------
 
ccc      DATA VERSN /95.0512/         !  Changes external to propagation
ccc      DATA VERSN /95.0501/         !  Changes external to propagation
ccc      DATA VERSN /95.0428/         !  Fix METHOD 30 (SMOOTHING) for area calc
ccc      DATA VERSN /95.0411/         !  Changes external to propagation
ccc      DATA VERSN /95.0327/         !  Changes external to propagation
ccc      DATA VERSN /95.0314/         !  Changes external to propagation
ccc      DATA VERSN /95.0222/         !  Additional LUCAS change that was missed
ccc      DATA VERSN /95.0215/         !  changes to make DOS & UNIX same code
c                                      & allow maximum of frequency complement
ccc      DATA VERSN /95.0120/         !  LUCAS changes incorporated
ccc      DATA VERSN /95.0109/         !  Changed REDMAP for standard coeff files
ccc      DATA VERSN /95.0105/         !  Changed Man-Made Noise defaults
ccc      DATA VERSN /94.1114/         !  Changed SNR90 to SNRxx
ccc      DATA VERSN /94.1103/         !  fixed coeff error (115.2 > 11.52)
ccc      DATA VERSN /94.0902/         !  method 30 (smoothing) activated
ccc      DATA VERSN /94.0526/
 
C---------------------------------
C
C   VOACAP VERSION 92.0515 of   15 May 1992  is the initial interactive
C   version of VOACAP (EUIP) to be tested by VOA. 
C
C---------------------------------
C
C      ADD COMMENTS ON OTHER VERSIONS
C
C---------------------------------
C
C   VOACAP VERSION 91.0821 of   21 Aug 1991   contains two significant
C   modifications since Vers. 91.0510.
C
C          1)  This is the first version to contain an interactive entry
C     of data in lieu of the older card image file format.
C          2)  The calling sequency to find OVER-THE-MUF modes was changed
C     to assure at least an over-the-muf mode for each layer.  The old call
C     sequence was (starting in the hop loop in LUFFY) LUFFY called REGMOD
C     which in turn called FDIST then INMUF before RETURNing to LUFFY with
C     up to 6 modes for each hop. In LUFFY after exiting the hop loop, if
C     no modes were found for the three hop numbers, then INMOD was called
C     which found one over-the-muf mode AND no more. In the new sequence
C xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx continue here
C
C---------------------------------
C
C   VOACAP VERSION 91.0701 of    1 JUL 1991   contains three significant
C   modifications, a cosmetic change, a clerical change, AND one puristic.
C
C          1)  It is the iniTIAL version to provide an option to smooth
C     prediction results between the Short path model AND the Long path
C     model for distances between 7000 AND 10000 kilometers.  This is
C     implemented per VOA Memo of 15 January 1991 by selecting method
C     121 in columns 23 thru 25 on the method card.  All parameters needed
C     in IONANT for this smoothing are automatically output in the binary
C     file when the all modes option is selected by placing 125 in columns
C     23 thru 25.
C          2)  The VHF test output lines have been completely eliminated.
C          3)  Subroutines LNGOUT AND OUTLNG were incomplete AND were
C     never called so they were deleted.
C          4)  This version is the first to have its version number updated
C     with each modification AND to contain a comment section, namely this,
C     which briefly describes the current changes.
C          5)  All output pages generated using this version will automati-
C     cally bear this new version number for reference. The two most signif-
C     icant digits of the version number are the year of the modification,
C     the next two significant digits are the month AND the two least sig-
C     nificant are the day.
C          6)  An option to force an all modes method (METHOD=25) for paths
C     greater than 10,000 kilometers has been added. However when this is
C     selected the short path model is used AND NOT the long path model.
C
C---------------------------------
C
      END
C--------------------------------

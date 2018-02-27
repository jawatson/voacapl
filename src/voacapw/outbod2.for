c# outbod2.f
      SUBROUTINE OUTBOD2(lin,jfmt)
C--------------------------------
C
C     THIS ROUTINE OUTPUTS THE VARIABLES SET IN SUBROUTINE SETOUT
C     DEPENDENT ON METHOD BUT THE USER MAY SPECIFY HIS OWN CHOICE OF
C     VARIABLES TO OUTPUT BY INCLUDING A "BOTLINES" CONTROL CARD AND
C     RUNNING METHOD 23.  THE ROUTINE USES A VECTORED FORMAT SCHEME AND
C     CALLS SUBROUTINES FIXLIN AND FLOLIN TO OUTPUT THE LINE
C
      COMMON /DUDL_NOIS/ DU_NOIS(13),DL_NOIS(13)
      common /c_S_to_I/ i_S_to_I    !  = 1 = S/I calculation
      common /sncom/ snxx(13)
      common /cgains/ gaint(13),gainr(13)
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/LPATH/ GCDLNG,TXRGML(45,2),DELOPT,GMIN,YMIN,LTXRGM(2)
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     1,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMU
     2F(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     3 ,YMUF(4)
      COMMON / OUTFMT / KLINE(26)
      COMMON /OUTPRT/ LINBOT(26), LINBD(14), LINTOP(15), LINTP(14),
     A GRPTYP, JOUT, LINBYP, LINES, LINMAX, LINTYP, LPAGES, NLINE
      COMMON / SON / ANGLE(13), ANGLER(13), CPROB(13), DBLOS(13),
     A DBLOSL(13), DBLOSU(13), DBU(13), DELAY(13), DBW(13), NHP(13),
     B XNYNOIS(13), PROBMP(13), RELIAB(13), SNDB(13), SNPR(13),
     C SNRLW(13), SNRUP(13), SPROB(13), VHIGH(13), RNEFF(13),MDL(13)
       CHARACTER MDL*1
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      CHARACTER NDASH*4
      CHARACTER KLINE*6
      INTEGER ITMP(13)
      character jfmt*(*)
      DATA NDASH/'  - '/
      DATA IMUF/12/
C.....THIS PROGRAM WILL WRITE 2 PLUS LINBOT(26) LINES. IT IS
C     ASSUMED THAT THE LINE COUNT WAS CHECKED BY THE CALLING PROGRAM.

      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     +      21,22),lin
1     continue                       !  MODE
      IF(JLONG.lt.0) then            !  short path
         jfmt(8:15)='1x,i2,a2'
         WRITE(LUO,jfmt) NHP(IMUF),MODE(IMUF),(NHP(IFQ),MODE(IFQ),
     +                   IFQ =1,JFREQ),(NDASH,ID=1,JDASH),KLINE(1)
      else                            !  long path
         jfmt(8:15)='1x,a2,a2'
         WRITE(LUO,jfmt) MODE(IMUF),MODER(IMUF),(MODE(IFQ),MODER(IFQ),
     +                   IFQ=1,JFREQ),(NDASH,ID=1,JDASH),KLINE(1)
      end if
      go to 500
2     CALL FLOLIN(jfmt,1,ANGLE,2)     !  ANGLE
      IF(JLONG.gt.0) CALL FLOLIN(jfmt,1,ANGLER,23)   !  long path ANGLE
      go to 500
3     CALL FLOLIN(jfmt,1,DELAY,3)     !  DELAY
      go to 500
4     DO 104 II=1,13                  !  V HITE
  104 ITMP(II)=anint(vhigh(II))
      CALL FIXLIN(jfmt,ITMP,4)
      go to 500
5     CALL FLOLIN(jfmt,2,CPROB,5)     !  F DAYS
      go to 500
6     DO 106 II=1,13                  !  LOSS
  106 ITMP(II)=anint(dblos(II))
      CALL FIXLIN(jfmt,ITMP,6)
      go to 500
7     DO 107 II=1,13                  !  dBu
  107 ITMP(II)=anint(dbu(II))
      CALL FIXLIN(jfmt,ITMP,7)
      go to 500
8     DO 108 II=1,13                  !  S dBw
  108 ITMP(II)=anint(DBW(II))
      CALL FIXLIN(jfmt,ITMP,8)
      go to 500
9     DO 109 II=1,13                  !  N dBw
  109 ITMP(II)=anint(XNYNOIS(II)+RNEFF(II))
      CALL FIXLIN(jfmt,ITMP,9)
      go to 500
10    DO 110 II=1,13                  !  SNR median
  110 ITMP(II)=anint(sndb(II))
      CALL FIXLIN(jfmt,ITMP,10)
      go to 500
11    DO 111 II=1,13                  !  RPWRG
  111 ITMP(II)=anint(snpr(II))
      CALL FIXLIN(jfmt,ITMP,11)
      go to 500
12    CALL FLOLIN(jfmt,2,RELIAB,12)   !  REL
      go to 500
13    CALL FLOLIN(jfmt,2,PROBMP,13)   !  MPROB
      go to 500
14    CALL FLOLIN(jfmt,2,SPROB,14)    !  SPRB
      go to 500
15    CALL FLOLIN(jfmt,1,DBLOSL,15)   !  SIGLW
      go to 500
16    CALL FLOLIN(jfmt,1,DBLOSU,16)   !  SIGUP
      go to 500
17    if(i_S_to_I.eq.0) then          !  not S/I output
         CALL FLOLIN(jfmt,1,SNRLW,17)    !  SNRLW
      else
         CALL FLOLIN(jfmt,1,DL_NOIS,24)  !  Noise dl
      end if
      go to 500
18    if(i_S_to_I.eq.0) then          !  not S/I output
         CALL FLOLIN(jfmt,1,SNRUP,18)    !  SNRUP
      else
         CALL FLOLIN(jfmt,1,DU_NOIS,25)  !  Noise du
      end if
      go to 500
19    CALL FLOLIN(jfmt,1,gaint,19)    !  T gain
      go to 500
20    CALL FLOLIN(jfmt,1,gainr,20)    !  R gain
      go to 500
21    DO 121 II=1,13                  !  SNR xx
  121 ITMP(II)=anint(snxx(II))
      CALL FIXLIN(jfmt,ITMP,21)
      go to 500
22    DO 122 II=1,13                  !  dBm = S DBW + 30
  122 ITMP(II)=anint(DBW(II)+30.)
      CALL FIXLIN(jfmt,ITMP,22)
500   continue
      RETURN
      END
C--------------------------------

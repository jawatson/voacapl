c# outbod.f
      SUBROUTINE OUTBOD
C--------------------------------
C
C     THIS ROUTINE OUTPUTS THE VARIABLES SET IN SUBROUTINE SETOUT
C     DEPENDENT ON METHOD BUT THE USER MAY SPECIFY HIS OWN CHOICE OF
C     VARIABLES TO OUTPUT BY INCLUDING A "BOTLINES" CONTROL CARD AND
C     RUNNING METHOD 23.  THE ROUTINE USES A VECTORED FORMAT SCHEME AND
C     CALLS SUBROUTINES FIXLIN AND FLOLIN TO OUTPUT THE LINE
C
      common /cdistance/ idistance,ndistance,ihr       !  plot vs distance
      common /ctime/ ntime                             !  plot vs time
      common /c_S_to_I/ i_S_to_I    !  = 1 = S/I calculation
      common /Charris/ iharris    !  =1=harris99.exe exists
      common /sncom/ snxx(13)
      common /cgains/ gaint(13),gainr(13)
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
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
      COMMON /OUTPRT/ LINBOT(26), LINBD(14), LINTOP(15), LINTP(14),
     A GRPTYP, JOUT, LINBYP, LINES, LINMAX, LINTYP, LPAGES, NLINE
      COMMON / SON / ANGLE(13), ANGLER(13), CPROB(13), DBLOS(13),
     A DBLOSL(13), DBLOSU(13), DBU(13), DELAY(13), DBW(13), NHP(13),
     B XNYNOIS(13), PROBMP(13), RELIAB(13), SNDB(13), SNPR(13),
     C SNRLW(13), SNRUP(13), SPROB(13), VHIGH(13), RNEFF(13),MDL(13)
       CHARACTER MDL*1, slmdl*9
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      character jfmt*40
      DATA ND/11/
C.....THIS PROGRAM WILL WRITE 2 PLUS LINBOT(26) LINES. IT IS
C     ASSUMED THAT THE LINE COUNT WAS CHECKED BY THE CALLING PROGRAM.
      IFREQ=1
      if(ndistance.ne.1) then
         call outbod3           !  output for Plots vs DISTANCE
         return
      end if
      if(ntime.ne.0) then
         call outbod4           !  output for Plots vs TIME
         return
      end if
 
ccc      IF(LINES.gt.LINTOP(15)) go to 200
ccc      if(MSPEC.eq.121)then
ccc        WRITE(LUO,1501)
ccc 1501 FORMAT(/,' ',3X,'UT  MUF ',5x,'***MODEL  S=Short path  L=LONG Pa',
ccc     +'th  M=Smoothed(VOA)')
ccc      else
ccc        WRITE(LUO,1500)
ccc 1500 FORMAT(/,' ',3X,'UT  MUF ')
ccc      ENDIF
ccc      LINES=LINES+2
200   if(MSPEC.ne.121) then
         slmdl='         '
      else
         if(MDL(13).eq.'S')then
            slmdl='/SP-Model'
         else if(MDL(13).eq.'L')then
            slmdl='/LP-Model'
         else
            slmdl='/Model***'
         endif
      end if
ccc      WRITE(LUO,1502)GMT,ALLMUF(IT),MDL(12),(FREL(IQ),MDL(IQ),IQ=1,ND),
ccc     +slmdl
ccc 1502 FORMAT(/,' ',1X,F4.1,1X,12(F4.1,A1),'FREQ',a9)
ccc       changes made at VOA request 9/8/95
cc      WRITE(LUO,1502)GMT,ALLMUF(IT),(FREL(IQ),IQ=1,ND)
cc 1502 FORMAT(/,' ',1X,F4.1,12F5.1,' FREQ')
C*****Begin HARRIS RF Mod by CJK 8/1/96************************************
      if(iharris.eq.0) then
         WRITE(LUO,1502)GMT,ALLMUF(IT),(FREL(IQ),IQ=1,ND)
c1502  FORMAT(/,' ',1X,F4.1,12F5.1,' FREQ')
      else
         WRITE(LUO,1502) GMT,ALLMUF(IT),(FREL(IQ),IQ=1,ND),XLMT(IT),
     1                FOT(IT),XLUF(IT)
      end if
 1502 FORMAT(/,' ',1X,F4.1,12F5.1,' FREQ',4X,3(F4.1,1X))
C*****End HARRIS RF Mod****************************************************


      if(ALLMUF(IT).gt.30.) then        !  no antenna support
         nhp(12)=0
         mode(12)='NA'
         moder(12)='NA'
         angle(12)=99.9
         delay(12)=99.9
         vhigh(12)=1000
         dblos(12)=1000
         dbu(12)=-999
         dbw(12)=-999
         sndb(12)=-999
         snpr(12)=-999
         reliab(12)=0.
         probmp(12)=0.
         sprob(12)=0.
         snxx(12)=-999
      end if
      DO 210 IFQ = 2,ND
      IF(FREL(IFQ).gt.0) IFREQ=IFQ
  210 CONTINUE
      JFREQ = -1
      DO 225 IFQ = 1,IFREQ
      IF(NHP(IFQ).gt.0) JFREQ=IFQ
  225 CONTINUE
      JFREQ = MIN0(ND+1, JFREQ)
      IF(JFREQ.le.0) then         !  write all dashes
         LINES=LINES+3
         RETURN
      end if
      IF(JFREQ.ge.ND) then      !  write no dashes
         jfmt='(6x,12(1x,i2,a2),1x,a6)'
         JDASH = -1
      else                      !  write some dashes
         JDASH = ND - JFREQ
         write(jfmt,226) jfreq+1,jdash
226      format(4h(6x,,i2,11h(1x,i2,a2),,i2,14h(1x,a4),1x,a6))
      end if

      if(method.eq.23 .or. linbyp.ne.0) then   !  output controlled by BOTLINES
         linbot(26)=0
         do 510 i=1,14
         lin=LINBD(i)
         if(lin.gt.0) then
            call outbod2(lin,jfmt)
            linbot(26)=linbot(26)+1
         end if
510      continue
      else
         do 500 lin=1,22                !  output in order on OUTLINES card
         if(LINBOT(lin).gt.0) call outbod2(lin,jfmt)   !  output this parameter
500      continue
      end if
      LINES = LINES + LINBOT(26) + 2
      if(i_S_to_I.ne.0) call out_si        !  = 1 = S/I calculation
      RETURN
      END
C--------------------------------
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
ccc      write(luo,'(''cprob='',13f8.5)') (cprob(ii),ii=1,jfreq)
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
c# outbod3.f
      SUBROUTINE OUTBOD3
C--------------------------------
C
C     THIS ROUTINE OUTPUTS THE VARIABLES SET IN SUBROUTINE SETOUT
C     DEPENDENT ON METHOD BUT THE USER MAY SPECIFY HIS OWN CHOICE OF
C     VARIABLES TO OUTPUT BY INCLUDING A "BOTLINES" CONTROL CARD AND
C     RUNNING METHOD 23.  THE ROUTINE USES A VECTORED FORMAT SCHEME AND
C     CALLS SUBROUTINES FIXLIN AND FLOLIN TO OUTPUT THE LINE
C
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      common /cdistance/ idistance,ndistance,ihr       !  plot vs distance
      common /cnfreqs/ nfreqs
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

      character xmode*4
      dimension vals(21)
      data nvals/21/

      if(idistance*ihr.eq.1) write(LUO,'(''**********'')')
      do 100 ifreq=1,nfreqs
      irec=((ihr-1)*ndistance + idistance-1)*nfreqs + ifreq
ccc      irec=((ihr-1)*nfreqs + ifreq-1)*ndistance + idistance
      if(JLONG.lt.0) then       !  short path algorithm
         write(xmode,'(i2,a2)') iabs(nhp(ifreq)),mode(ifreq)
      else                      !  long path algorithm
         write(xmode,'(a2,a2)') mode(ifreq),moder(ifreq)
      end if
      xmuf=ALLMUF(IT)
      xfot=FOT(IT)
      vals( 1)=ANGLE(ifreq)     !  ANGLE
      vals( 2)=DELAY(ifreq)     !  DELAY
      vals( 3)=VHIGH(ifreq)     !  V HITE
      vals( 4)=CPROB(ifreq)     !  MUFday
      vals( 5)=DBLOS(ifreq)     !  LOSS
      vals( 6)=DBU(ifreq)       !  DBU
      vals( 7)=DBW(ifreq)       !  S DBW
      vals( 8)=XNYNOIS(ifreq)+RNEFF(ifreq)   !  N DBW
      vals( 9)=SNDB(ifreq)      !  SNR median
      vals(10)=SNPR(ifreq)      !  RPWRG
      vals(11)=RELIAB(ifreq)    !  REL
      vals(12)=PROBMP(ifreq)    !  MPROB
      vals(13)=SPROB(ifreq)     !  S PRB
      vals(14)=DBLOSL(ifreq)    !  SIG LW
      vals(15)=DBLOSU(ifreq)    !  SIG UP
      vals(16)=SNRLW(ifreq)     !  SNR LW
      vals(17)=SNRUP(ifreq)     !  SNR UP
      vals(18)=GAINT(ifreq)     !  T gain
      vals(19)=GAINR(ifreq)     !  R gain
      vals(20)=SNxx(ifreq)      !  SNRxx
      vals(21)=DBW(ifreq)+30.   !  DBM
      xlat=rlat*r2d
      xlon=rlong*r2d
      write(49,rec=irec) gcdkm,xlat,xlon,xmode,xmuf,xfot,
     +                   (vals(i),i=1,nvals)
ccc      write(LUO,99) ifreq,ihr,idistance,irec,xlat,xlon,
ccc     +            xmode,xmuf,(vals(i),i=1,6)
ccc 99   format(3i3,i5,1h=,f6.2,f7.3,1h=,a4,f8.3,2f5.1,f5.0,f5.3,2f6.1)
 100  continue
      RETURN
      END
C--------------------------------
c# outbod4.f
      SUBROUTINE OUTBOD4
C--------------------------------
C
C     THIS ROUTINE OUTPUTS THE VARIABLES SET IN SUBROUTINE SETOUT
C     DEPENDENT ON METHOD BUT THE USER MAY SPECIFY HIS OWN CHOICE OF
C     VARIABLES TO OUTPUT BY INCLUDING A "BOTLINES" CONTROL CARD AND
C     RUNNING METHOD 23.  THE ROUTINE USES A VECTORED FORMAT SCHEME AND
C     CALLS SUBROUTINES FIXLIN AND FLOLIN TO OUTPUT THE LINE
C
      COMMON /CON /D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      common /cdistance/ idistance,ndistance,ihr       !  plot vs distance
      common /cnfreqs/ nfreqs
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

      character xmode*4
      dimension vals(21)
      data nvals/21/

      if(ihr.eq.1) write(LUO,'(''**********'')')
      do 100 ifreq=1,nfreqs
      irec=(ihr-1)*nfreqs + ifreq
      if(JLONG.lt.0) then       !  short path algorithm
         write(xmode,'(i2,a2)') nhp(ifreq),mode(ifreq)
      else                      !  long path algorithm
         write(xmode,'(a2,a2)') mode(ifreq),moder(ifreq)
      end if
      xmuf=ALLMUF(IT)
      xfot=FOT(IT)
      vals( 1)=ANGLE(ifreq)     !  ANGLE
      vals( 2)=DELAY(ifreq)     !  DELAY
      vals( 3)=VHIGH(ifreq)     !  V HITE
      vals( 4)=CPROB(ifreq)     !  MUFday
      vals( 5)=DBLOS(ifreq)     !  LOSS
      vals( 6)=DBU(ifreq)       !  DBU
      vals( 7)=DBW(ifreq)       !  S DBW
      vals( 8)=XNYNOIS(ifreq)+RNEFF(ifreq)   !  N DBW
      vals( 9)=SNDB(ifreq)      !  SNR median
      vals(10)=SNPR(ifreq)      !  RPWRG
      vals(11)=RELIAB(ifreq)    !  REL
      vals(12)=PROBMP(ifreq)    !  MPROB
      vals(13)=SPROB(ifreq)     !  S PRB
      vals(14)=DBLOSL(ifreq)    !  SIG LW
      vals(15)=DBLOSU(ifreq)    !  SIG UP
      vals(16)=SNRLW(ifreq)     !  SNR LW
      vals(17)=SNRUP(ifreq)     !  SNR UP
      vals(18)=GAINT(ifreq)     !  T gain
      vals(19)=GAINR(ifreq)     !  R gain
      vals(20)=SNxx(ifreq)      !  SNRxx
      vals(21)=DBW(ifreq)+30.   !  DBM
      xlat=rlat*r2d
      xlon=rlong*r2d
      write(49,rec=irec) xmode,xmuf,xfot,(vals(i),i=1,nvals)
ccc      write(LUO,99) ifreq,ihr,irec,
ccc     +            xmode,xmuf,(vals(i),i=1,6)
ccc 99   format(3i3,1h=,a4,f8.3,2f5.1,f5.0,f5.3,2f6.1)
 100  continue
      RETURN
      END
C--------------------------------

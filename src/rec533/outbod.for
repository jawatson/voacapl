c###outbod.for
      SUBROUTINE OUTBOD(XDBU,SPWR,SNA,PSN,SNAxx)
      dimension XDBU(24,12),SPWR(24,12),SNA(24,12),PSN(24,12),
     +          SNAxx(24,12)
C.....VERSION 10.APRIL.92
      common /cdistance/ idistance,ndistance,ihr       !  plot vs distance
      common /ctime/ ntime                             !  plot vs time
      CHARACTER*4 NFREQ
      CHARACTER*4 NAMES,APW,KLINE(6),DASH
      CHARACTER*40 VERSN
      COMMON /CON/D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      LOGICAL YNOISE
      COMMON /TNOISE/BDWTH,JBW,JRSN,LUF,MAN,RLUF,RSN,XSN(2),YNOISE
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      COMMON /FRQ/ FREL(12),FREL5(11,7),FW5(11,7),MAXF,MF,KOF,FREQ
     A, JKF,FXEMAX
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      COMMON/MUFS/ EMUF(24,3),F2MUF(24,6),ALLMUF(24),EFMUF(24),FOT(24)
     A,FOT1(24),XLUF(24),ZLUF(24),OPMUF(24),OPMUF1(24),FU(24),FL(24)
     B, KFN,LUFC,NLOOP,ACT1,DXL,XMODE,FX,PR
      COMMON/NAMEX/NAMES(20),ISSN,IRED,LINES,LPAGES,MAPIN,KRUN
C RAY MODES FOR DISTANCE GHOP,SEE SUBROUTINE CURMUF
      COMMON/MODES/ DELMOD(9),UTMOD(9),ABPS(9),FLDST(9),FSLOS(9)
     1,GRLOS(9),TLOSS(9),XLOSS(9),DELMD1(9),UTMD1(9),KANGLE(12)
     2,DBLOS(12),DBU(12),MODE(12),NHP(12),KANGLA(24,12)
     3,MODEA(24,12),KANGL4(4,12),KDBU4(4,12),DBU4(4,12),MODE4(4,12)
     4,NHP4(4,12),KODEA(24,12)
       character*4 MODE,MODEA,MODE4
      DATA KLINE/'MODE','ANGL','DBU ','dBpW','S/N ','FS/N'/
      DATA DASH/'  - '/
      DATA NFREQ/'FREQ'/
C.....THIS PROGRAM WILL WRITE 2 PLUS LINBOD(30) LINES. IT IS
C.....ASSUMED THAT THE LINE COUNT WAS CHECKED BY THE CALLING PROGRAM.
C.....MAX. NO. OF FREQUENCIES,MAXF=10 OR 11 (IC=2,1 RESP.)
C.....NOTE IF MAXF=10 OUTPUTS CAN BE ACCOMODATED IN 80 COLS

      if(ndistance.ne.1) then   !  output for plots vs DISTANCE
         call outbod3(XDBU,SPWR,SNA,PSN,SNAxx)
         return
      end if
      if(ntime.ne.0) then       !  output for plots vs TIME
         call outbod4(XDBU,SPWR,SNA,PSN,SNAxx)
         return
      end if

      GMT=IT
C.....
      IF(METHOD.EQ.6) THEN
C.....SINGLE MODE OUTPUT (TYPE 6)
      IF(LINES.le.6) WRITE(LUO,1501)
1501  FORMAT(/,1H ,8HUT  MUF ,56X,14HLUF  FOT OPMUF)
      WRITE(LUO,1502) IT,ALLMUF(IT),(FREL(IFQ),IFQ=1,MAXF),NFREQ
     A,XLUF(IT),FOT(IT),OPMUF(IT)
1502  FORMAT(/,I3,11F5.1,1x,A4,2F5.1,F6.1)
      IF(GCDKM.GT.GCDFTZ) GO TO 101
      WRITE(LUO,1504) KODEA(IT,12), MODEA(IT,12),
     +              (KODEA(IT,IFQ), MODEA(IT,IFQ),IFQ=1,MAXF),KLINE(1)
1504  FORMAT(1H ,3X,11(I2,A2,1X),A4)
      WRITE(LUO,1506) KANGLA(IT,12), (KANGLA(IT,I),I=1,MAXF),KLINE(2)
1506  FORMAT(3X,11I5,1X,A4)
      GO TO 102
  101 WRITE(LUO,1510) (DASH,IC1=1,MF),KLINE(1)
      WRITE(LUO,1510) (DASH,IC1=1,MF),KLINE(2)
1510  FORMAT(4X,11(A4,1X),A4)
  102 CONTINUE
      WRITE(LUO,1506) NINT(XDBU(IT,12)),
     +                (NINT(XDBU(IT,I)),I=1,MAXF),KLINE(3)
      WRITE(LUO,1506) NINT(SPWR(IT,12)),
     +                (NINT(SPWR(IT,I)),I=1,MAXF),KLINE(4)
      WRITE(LUO,1506) NINT(SNA(IT,12)),
     +                (NINT(SNA(IT,I)),I=1,MAXF),KLINE(5)
      WRITE(LUO,1508) PSN(IT,12),(PSN(IT,I),I=1,MAXF),KLINE(6)
1508  format(3X,11F5.2,1X,A4)
      WRITE(LUO,1506) NINT(SNAxx(IT,12)),
     +                (NINT(SNAxx(IT,I)),I=1,MAXF),'SNxx'
      LINES=LINES+10
      ELSE
C..... MULTI-MODE OUTPUT -RSS+3 STRONGEST (TYPE 7)
C..... WRITE HEADER FOR EACH COLUMN AT EACH UT
      WRITE(LUO,1698)
      DO 1700 IFQ=1,KOF,2
      JFQ=IFQ
      IF(JFQ.EQ.KOF) THEN
C...... SINGLE COLUMN OUTPUT
       WRITE(LUO,1702) IT,ALLMUF(IT),FREL(JFQ),XLUF(IT),FOT(IT)
     1 ,OPMUF(IT)
       WRITE(LUO,1704) (NHP4(J4,JFQ),MODE4(J4,JFQ),J4=2,4),KLINE(1)
       WRITE(LUO,1706) (KANGL4(J4,JFQ),J4=1,4),KLINE(2)
       WRITE(LUO,1706) (KDBU4(J4,JFQ),J4=1,4),KLINE(3)
ccc*      WRITE(LUO,1708) (PSN4 (J4,JFQ),J4=1,4),KLINE(6)
      ELSE
C...... DOUBLE COLUMN OUTPUT
       JFQ1=JFQ+1
       WRITE(LUO,1712) IT,ALLMUF(IT),FREL(JFQ),FREL(JFQ1),XLUF(IT)
     1 ,FOT(IT),OPMUF(IT)
       WRITE(LUO,1714) ((NHP4(J4,JX),MODE4(J4,JX),J4=2,4),JX=JFQ,JFQ1)
     1 ,KLINE(1)
       WRITE(LUO,1716) ((KANGL4(J4,JX),J4=1,4),JX=JFQ,JFQ1),KLINE(2)
       WRITE(LUO,1716) ((KDBU4(J4,JX),J4=1,4),JX=JFQ,JFQ1),KLINE(3)
ccc*      WRITE(LUO,1718) ((PSN4 (J4,JX),J4=1,4),JX=JFQ,JFQ1),KLINE(6)
      END IF
 1700 CONTINUE
C.....ADD 50 TO LINE COUNT TO ENSURE NEW PAGE FOR EACH UT OUTPUT BLOCK
      LINES=LINES+50
 1698 FORMAT('0 UT  MUF FREQ',26X,'FREQ',22X,'LUF  FOT OPMUF')
 1702 FORMAT('0 ',I3,2F5.1,50X,2F5.1,F6.1)
 1704 FORMAT(8X,2X,'RSS',3(I3,A2),32X,A4)
 1706 FORMAT(8X,4(2X,I3),32X,A4)
ccc 1708 FORMAT(8X,4(2X,F3.2),32X,A4)
 1712 FORMAT('0 ',I3,2F5.1,25X,F5.1,20X,2F5.1,F6.1)
 1714 FORMAT(8X,2X,'RSS',3(I3,A2),10X,2X,'RSS',3(I3,A2),2X,A4)
 1716 FORMAT(8X,4(2X,I3),10X,4(2X,I3),2X,A4)
ccc 1718 FORMAT(8X,4(2X,F3.2),10X,4(2X,F3.2),2X,A4)
      END IF
      RETURN
      END
c------------------------------------------------------------------
c# outbod3.f
      SUBROUTINE OUTBOD3(XDBU,SPWR,SNA,PSN,SNAxx)
      dimension XDBU(24,12),SPWR(24,12),SNA(24,12),PSN(24,12),
     +          SNAxx(24,12)
C--------------------------------
C
C     THIS ROUTINE OUTPUTS THE VARIABLES SET IN SUBROUTINE SETOUT
C     DEPENDENT ON METHOD BUT THE USER MAY SPECIFY HIS OWN CHOICE OF
C     VARIABLES TO OUTPUT BY INCLUDING A "BOTLINES" CONTROL CARD AND
C     RUNNING METHOD 23.  THE ROUTINE USES A VECTORED FORMAT SCHEME AND
C     CALLS SUBROUTINES FIXLIN AND FLOLIN TO OUTPUT THE LINE
C
      common /cdistance/ idistance,ndistance,ihr       !  plot vs distance
      common /cnfreqs/ nfreqs
      COMMON /CON/D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      CHARACTER*40 VERSN
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      COMMON/MUFS/ EMUF(24,3),F2MUF(24,6),ALLMUF(24),EFMUF(24),FOT(24)
     A,FOT1(24),XLUF(24),ZLUF(24),OPMUF(24),OPMUF1(24),FU(24),FL(24)
     B, KFN,LUFC,NLOOP,ACT1,DXL,XMODE,FX,PR
C RAY MODES FOR DISTANCE GHOP,SEE SUBROUTINE CURMUF
      COMMON/MODES/ DELMOD(9),UTMOD(9),ABPS(9),FLDST(9),FSLOS(9)
     1,GRLOS(9),TLOSS(9),XLOSS(9),DELMD1(9),UTMD1(9),KANGLE(12)
     2,DBLOS(12),DBU(12),MODE(12),NHP(12),KANGLA(24,12)
     3,MODEA(24,12),KANGL4(4,12),KDBU4(4,12),DBU4(4,12),MODE4(4,12)
     4,NHP4(4,12),KODEA(24,12)
       character*4 MODE,MODEA,MODE4

      character xmode*4
      dimension vals(6)
      data nvals/6/

      if(idistance*ihr.eq.1) write(LUO,'(''**********'')')
      do 100 ifreq=1,nfreqs
      irec=((ihr-1)*ndistance + idistance-1)*nfreqs + ifreq
ccc      irec=((ihr-1)*nfreqs + ifreq-1)*ndistance + idistance
      if(GCDKM.le.GCDFTZ) then       !  short path algorithm
         write(xmode,'(i2,a2)') kodea(it,ifreq),modea(it,ifreq)
      else                      !  long path algorithm
         xmode='----'
      end if
      xmuf=ALLMUF(IT)
      xfot=FOT(IT)
      xopmuf=OPMUF(IT)
      vals( 1)=KANGLA(it,ifreq)     !  ANGLE
      vals( 2)=XDBU  (it,ifreq)     !  Field strength
      vals( 3)=SPWR  (it,ifreq)     !  Signal Power
      vals( 4)=SNA   (it,ifreq)     !  S/N
      vals( 5)=PSN   (it,ifreq)     !  Reliability
      vals( 6)=SNAxx (it,ifreq)     !  SNRxx
      xlat=rlatd
      xlon=rlongd
      write(49,rec=irec) gcdkm,xlat,xlon,xmode,xmuf,xfot,xopmuf,
     +                   (vals(i),i=1,nvals)
ccc      write(61,99) ifreq,ihr,idistance,irec,xlat,xlon,gcdkm,
ccc     +            xmode,xmuf,(vals(i),i=1,6)
ccc 99   format(3i3,i5,1h=,f7.2,f8.3,f7.0,1h=,
ccc     + a4,f8.3,4f5.1,f6.3,f5.1)
 100  continue
      RETURN
      END
C--------------------------------
c# outbod4.f
      SUBROUTINE OUTBOD4(XDBU,SPWR,SNA,PSN,SNAxx)
      dimension XDBU(24,12),SPWR(24,12),SNA(24,12),PSN(24,12),
     +          SNAxx(24,12)
C--------------------------------
C
C     THIS ROUTINE OUTPUTS THE VARIABLES SET IN SUBROUTINE SETOUT
C     DEPENDENT ON METHOD BUT THE USER MAY SPECIFY HIS OWN CHOICE OF
C     VARIABLES TO OUTPUT BY INCLUDING A "BOTLINES" CONTROL CARD AND
C     RUNNING METHOD 23.  THE ROUTINE USES A VECTORED FORMAT SCHEME AND
C     CALLS SUBROUTINES FIXLIN AND FLOLIN TO OUTPUT THE LINE
C
      common /cdistance/ idistance,ndistance,ihr       !  plot vs distance
      common /cnfreqs/ nfreqs
      COMMON /CON/D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      CHARACTER*40 VERSN
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      COMMON/MUFS/ EMUF(24,3),F2MUF(24,6),ALLMUF(24),EFMUF(24),FOT(24)
     A,FOT1(24),XLUF(24),ZLUF(24),OPMUF(24),OPMUF1(24),FU(24),FL(24)
     B, KFN,LUFC,NLOOP,ACT1,DXL,XMODE,FX,PR
C RAY MODES FOR DISTANCE GHOP,SEE SUBROUTINE CURMUF
      COMMON/MODES/ DELMOD(9),UTMOD(9),ABPS(9),FLDST(9),FSLOS(9)
     1,GRLOS(9),TLOSS(9),XLOSS(9),DELMD1(9),UTMD1(9),KANGLE(12)
     2,DBLOS(12),DBU(12),MODE(12),NHP(12),KANGLA(24,12)
     3,MODEA(24,12),KANGL4(4,12),KDBU4(4,12),DBU4(4,12),MODE4(4,12)
     4,NHP4(4,12),KODEA(24,12)
       character*4 MODE,MODEA,MODE4

      character xmode*4
      dimension vals(6)
      data nvals/6/

      if(ihr.eq.1) write(LUO,'(''**********'')')
      do 100 ifreq=1,nfreqs
      irec=(ihr-1)*nfreqs + ifreq
      if(GCDKM.le.GCDFTZ) then       !  short path algorithm
ccc         write(xmode,'(i2,a2)') nhp(ifreq),mode(ifreq)
         write(xmode,'(i2,a2)') kodea(it,ifreq),modea(it,ifreq)
      else                      !  long path algorithm
         xmode='----'
      end if
      xmuf=ALLMUF(IT)
      xfot=FOT(IT)
      xopmuf=OPMUF(IT)
      vals( 1)=KANGLA(it,ifreq)     !  ANGLE
      vals( 2)=XDBU  (it,ifreq)     !  Field strength
      vals( 3)=SPWR  (it,ifreq)     !  Signal Power
      vals( 4)=SNA   (it,ifreq)     !  S/N
      vals( 5)=PSN   (it,ifreq)     !  Reliability
      vals( 6)=SNAxx (it,ifreq)     !  SNRxx
      xlat=rlatd
      xlon=rlongd
      write(49,rec=irec) xmode,xmuf,xfot,xopmuf,
     +                  (vals(i),i=1,nvals)
ccc      write(9,99) ifreq,ihr,irec,
ccc     +            xmode,xmuf,(vals(i),i=1,6)
ccc 99   format(3i3,1h=,a4,f8.3,2f5.1,f5.0,f5.3,2f6.1)
 100  continue
      RETURN
      END
C--------------------------------

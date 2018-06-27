c###outtop.for
      SUBROUTINE OUTTOP
C.....VERSION 10.APRIL.92
      LOGICAL YNOIS1
      CHARACTER*7 QPATH(2)
      CHARACTER*4 NAMES,APW
      CHARACTER*40 VERSN
      common /cdistance/ idistance,ndistance,ihr       !  plot vs distance
      common /ctime/ ntime                             !  plot vs time
      common /ccoeff/ coeff
      character coeff*4
      common /cantenna/ numants,iats( 5),anttype( 6),antname( 6),
     +                  xfqs( 5),xfqe( 5),designfreq( 5),antfile( 5),
     +                  beammain( 5),offazim( 5),cond( 5),diel( 5)
      character anttype*10,antname*70,antfile*24
      common /pantenna/ pwrkw(5),pwrdba(5)
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
      COMMON /SOL/ DECL12(12),EQT12(12),DECL,EQT,MONTH,IMON(12)
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      COMMON/GEOG/ABIY(5),CLAT(5),CLONG(5),CLCK(5),EPSPAT(5),F2M3(5)
c>>(b)WP-6A SEPT. 93 MODS add 2 lines REMOVE 1 LINE
     A,FI(3,5),GLAT(5),GMDIP(5),GYZ100(5),GYR100,GYZ300(5),HPF2(5),RD(5)
     B,SIGPAT(5),CYCEN(5)
CX   A,FI(3,5),GLAT(5),GMDIP(5),GYZ(5),HPF2(5),RD(5),SIGPAT(5),CYCEN(5)
c>>(b)WP-6A SEPT. 93 MODS end of change
      LOGICAL YNOISE
      COMMON /TNOISE/BDWTH,JBW,JRSN,LUF,MAN,RLUF,RSN,XSN(2),YNOISE
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      COMMON/NAMEX/NAMES(20),ISSN,IRED,LINES,LPAGES,MAPIN,KRUN
      COMMON /HOG/ IANTT,BANTT,IANTR,BANTR
      common /FADE/ PERCENT
      integer PERCENT
      common /C_digital/ idigital,A_ratio,Tw,Fw
ccc      dimension nfound(2)
      character mform*1
ccc      character gainiso*10,antenna(2)*8
ccc      data antenna/'TRANSMIT','RECEIVE '/
      DATA QPATH/'<Short>','<Long >'/

c*********************************************************************
      if(ndistance.ne.1) then            !  only test if distance plot
         if(idistance.ne.1 .or. ihr.ne.1) return
      end if
      if(ntime.ne.0 .and. ihr.ne.1) return
c*********************************************************************

      YNOIS1=.NOT.YNOISE.OR.METHOD.EQ.1
C.....INCREMENT PAGE NUMBER
      LPAGES= LPAGES+1
      IPATH=NPSL+1
      GCDNMI=GCDKM*0.54
      mform=''                    !  top of page
      if(lpages.eq.1) mform=' '     !  do not page eject on 1st page
      WRITE(LUO,1500) mform,coeff,METHOD,VERSN(1:23)//'       ',LPAGES
 1500 FORMAT(a1,a4,' Coefficients',2x,'METHOD',I3,3X,A30,6X,'PAGE',I4)
ccc      write(luo,'(''rlongd='',f8.3)') rlongd
      if(idigital.eq.0) then
         write(luo,'(50x,''Analog modulation'')')
      else
         write(luo,'(50x,''Digital modulation'')')
      end if
      xrlongd=rlongd             !  this should be in the range 0-360.
      irlong='E   '
      if(xrlongd.gt.180.) then   !  make it -180 to 180
         xrlongd=360.-rlongd     !  west longitude
         irlong='W   '
      end if
      WRITE(LUO,1502) IMON(MONTH),NYEAR,SSN
 1502 FORMAT(1H ,A3,4X,A4,10X,6HSSN = ,F4.0,22x,'Path')
      WRITE(LUO,1504) ITRANS,IRCVR,QPATH(IPATH)
 1504 FORMAT(1H ,10A4,3X,8HAZIMUTHS,1X,A7,1X,6HN. MI. ,6X,2HKM)
      WRITE(LUO,1506) abs(TLATD),ITLAT,abs(TLONGD),ITLONG,
     +                abs(RLATD),IRLAT,abs(xRLONGD),IRLONG,
     +                BTRD,BRTD,GCDNMI,GCDKM
      WRITE(LUO,1508) AMIND
ccc      do 55 itr=1,2                  !  1=transmitter   2=receiver
ccc      nfound(itr)=0
ccc      do 50 i=1,numants
ccc      if(iats(i).ne.itr) go to 50    !  look for transmit/receive
ccc      if(iantdx(i).eq.0) then        !  add gain above ISOTROPE
ccc         write(gainiso,'(1h+,f5.1,4h dBi)') designfreq(i)
ccc         write(LUO,48) antenna(itr),nint(xfqs(i)),nint(xfqe(i)),
ccc     +                  iantdx(i),antname(i)(1:9)//gainiso
ccc48       format(1x,a8,i2,' to',i3,1x,'CCIR.[',i3,']=',a)
ccc      else
ccc         write(LUO,49) antenna(itr),nint(xfqs(i)),nint(xfqe(i)),
ccc     +     iantdx(i),antname(i),designfreq(i),beammain(i),offazim(i)
ccc49       format(1x,a8,i2,' to',i3,1x,'CCIR.[',i3,']=',a20,
ccc     +          ' DFreq=',f6.3,' Az=',f5.1,1h/,f5.1)
ccc      end if
ccc      KNT=KNT+1
ccc      nfound(itr)=nfound(itr)+1
ccc50    continue
ccc55    continue
      do 50 i=1,numants               !  TRANSMITTER (with power)
      if(iats(i).ne.1) go to 50
      write(LUO,1520) 'XMTR',nint(xfqs(i)),nint(xfqe(i)),
     +         anttype(i),antfile(i)(1:21),
     +         beammain(i),offazim(i),pwrkw(i)
1520  format(1x,a4,i3,'-',i2,1x,a10,1h[,a21,'] Az=',f5.1,
     +       ' OFFaz=',f5.1,f8.3,'kW')
ccc      KNTT=KNTT+1
50    continue
      do 55 i=1,numants                !  RECEIVER (no power)
      if(iats(i).ne.2) go to 55
      write(LUO,1520) 'RCVR',nint(xfqs(i)),nint(xfqe(i)),
     +         anttype(i),antfile(i)(1:21),beammain(i),offazim(i)
ccc      KNTT=KNTT+1
55    continue
c*****************************************************************
ccc      if(nfound(2).eq.0)
ccc     +write(LUO,49) 'RECEIVE ',2,30,0,'Isotropic           ',0.,0.,0.
      IF(YNOIS1.AND.MAN.LE.0) WRITE(LUO,1510) MAN,PERCENT,JRSN,JBW
      IF(YNOIS1.AND.MAN.GT.0) WRITE(LUO,1512) MAN,PERCENT,JRSN,JBW
      LINES=6
      RETURN
 1506 FORMAT(1H ,F5.2,1X,A1,2X,F6.2,1X,A1,3H   ,F5.2,1X,A1,2X,F6.2,
     A 1X,A1,4X,F6.2,2X,F6.2,3X,F7.1,2X,F7.1)
 1508 FORMAT(1H ,8HMIN ANG ,F4.1, 4H DEG)
 1510 FORMAT(1H ,'NOISE',i5,' dBW  ','         S/N',i3,
     + '% of Days @',i4,' dB  in',i5,' Hz RX Bandwidth')
 1512 FORMAT(1H ,'NOISE CATEGORY',i2,'         S/N',i3,
     + '% of Days @',i4,' dB  in',i5,' Hz RX Bandwidth')
      END
C--------------------------------

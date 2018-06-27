c# outarea.f
      SUBROUTINE OUTareah(luo,ix,iy)
C--------------------------------
C
C     THIS ROUTINE OUTPUTS all variables for area coverage contour plotting
C
      common /chours/ nhours,ihours(24)       !  which hours are active
      common /cantenna/ numants,iats( 5),anttype( 6),antname( 6),
     +                  xfqs( 5),xfqe( 5),designfreq( 5),antfile( 5),
     +                  beammain( 5),offazim( 5),cond( 5),diel( 5)
      character anttype*10,antname*70,antfile*24
      common /pantenna/ pwrkw(5),pwrdba(5)
      COMMON /FRQ/ FREL(12)
      COMMON /SOL/ DECL12(12),EQT12(12),DECL,EQT,MONTH,IMON(12)
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      CHARACTER*40 VERSN
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM

      character antbear*12,slew*4
      character alfs(19)*6,alfPwr*10,label*90,path*5
      equivalence (alfs,label)

      nch=lcount(VERSN,40)
      write(luo,'(a)') VERSN(1:nch)
      path='     '
      if(NPSL.ne.0) path='/Long'
      itr=1
      call TxPwr(pwrkw(itr),alfPwr,nchp)
c**********************************************************
c         get slew angle for HFCC antennas
      islew=0
      ncha=lcount(antfile(itr),24)
      slew=antfile(itr)(ncha-3:ncha)
      if(slew(1:2).eq.'.m') then    !  -slew angle
         read(slew,'(2x,i2)',err=110) islew
         islew=-islew
      else if(slew(1:2).eq.'.p') then    !  +slew angle
         read(slew,'(2x,i2)',err=110) islew
      end if
110   if(islew.eq.0) then
         write(antbear,'(i3,3hdeg)') nint(beammain(itr))
      else
         mainbeam=nint(beammain(itr))+islew       !  add slew back in
         if(mainbeam.lt.0.) mainbeam=mainbeam+360
         write(antbear,'(1h(,i3,1h,,i3,4h)deg)') mainbeam,islew
      end if
c**********************************************************
      write(label,101) ITRANS,antname(itr)(1:10),alfPwr(1:nchp),
     +                 antbear,ihours(1),Frel(1),
     +                 IMON(month),nint(ssn),path
101   format(5a4,1h[,a10,2h] ,a,1x,a,i3.2,'ut',f7.3,'MHz ',a3,
     +       i4,'ssn',a)
      call squeez(label,90)       !  squeeze out blanks
      write(*,'(1x,a)') label
      call progress_label(label)
      write(luo,'(a)') label
      write(luo,102) ix,iy
102   format(2i3,'  Latitude Longitude',
     +               '   MUF  MODE  ANGL   DBU   S/N  FS/N  SNxx')
      RETURN
      END
C--------------------------------
      subroutine progress_label(label)
c      include <windows.ins>
      character label*(*)
      common /Cprogress/ iarea_batch,alf_label
         character alf_label*80
      if(iarea_batch.eq.0) return
      alf_label=label
c      call window_update@(alf_label)
      RETURN
      END
C---------------------------------

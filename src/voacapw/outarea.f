c# outarea.f
      SUBROUTINE OUTarea(ix,iy)
C--------------------------------
C
C     THIS ROUTINE OUTPUTS all variables for area coverage contour plotting
C
      use verbose_mod
      use version_mod
      common /Careach/ areach
         character areach*1
      COMMON /RGRID/ IPROJ,PLAT,PLON,XMIN,XMAX,YMIN,YMAX,NX,NY
      common /chours/ nhours,ihours(24)       !  which hours are active
      common /cdaily/ idaily(12)       !  day of the month modification
      common /cantenna/ numants,iats(20),anttype(20),antname(20),
     +                  xfqs(20),xfqe(20),designfreq(20),antfile(20),
     +                  beammain(20),offazim(20),cond(20),diel(20),
     +                  array(30,91,20),aeff(30,20)
      character anttype*10,antname*70,antfile*24
      common /pantenna/ pwrkw(20),pwrdba(20)
      common /sncom/ snxx(13)
      common /cgains/ gaint(13),gainr(13)
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS,SPR,SU,SUS
     A ,XNOISE,ZNOISE,NF
      COMMON /DUDL_NOIS/ DU_NOIS(13),DL_NOIS(13)
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
      COMMON/LPATH/ GCDLNG,TXRGML(45,2),DELOPT,GMIN,YMINx,LTXRGM(2)
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
      CHARACTER KLINE*6
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
c      common /CVERSN/ VERSN
c         character VERSN*8

      character antbear*12,slew*4
      character alfs(24)*6,alfPwr*10,label*90,path*5,daily*3
      equivalence (alfs,label)
      data SNR88,SNR91/88.,91./

      if(ix.lt.0) go to 100                  !  output header

      write(alfs(1),'(f6.2)') frel(12)            !  MUF
      if(JLONG.lt.0) then
        write(alfs(2),'(2x,i2,a2)') NHP(1),MODE(1)  !  MODE
      else
        write(alfs(2),'(2x,a2,a2)') MODE(1),MODER(1)  !  T_MODE,R_MODE
      end if
      do 50 i=2,nf
        if(dbu(i).gt.dbu(1)) dbu(1)=dbu(i)
        if(dbw(i).gt.dbw(1)) dbw(1)=dbw(i)
        if(sndb(i).gt.sndb(1)) sndb(1)=sndb(i)
        if(reliab(i).gt.reliab(1)) reliab(1)=reliab(i)
        if(sprob(i).gt.sprob(1)) sprob(1)=sprob(i)
        if(snxx(i).gt.snxx(1)) snxx(1)=snxx(i)
50    continue
c          calculate the George Lane % Power Cut
      SNR_LW=SNRLW(1)
      SNR_UP=SNRUP(1)
      SNR50=sndb(1)
      call pwrcut(SNR50,SNR_LW,SNR_UP,SNR88,SNR91,power_cut)
      write(alfs(3),'(f6.2)') xlimit6(angle(1),2)            !  ANGLE
      write(alfs(4),'(f6.2)') xlimit6(delay(1),2)            !  DELAY
      write(alfs(5),'(f6.1)') xlimit6(vhigh(1),1)            !  V HITE
      write(alfs(6),'(f6.3)') xlimit6(cprob(1),3)            !  MUF days
      write(alfs(7),'(f6.1)') xlimit6(dblos(1),1)            !  LOSS
      write(alfs(8),'(f6.1)') xlimit6(dbu(1)  ,1)            !  DBU
      write(alfs(9),'(f6.1)') xlimit6(dbw(1),1)              !  S DBW
      write(alfs(10),'(f6.1)') xlimit6(XNYNOIS(1)+RNEFF(1),1)      !  N DBW
      write(alfs(11),'(f6.1)') xlimit6(sndb(1),1)            !  SNR median
      write(alfs(12),'(f6.1)') xlimit6(snpr(1),1)            !  RPWRG
      write(alfs(13),'(f6.3)') xlimit6(reliab(1),3)          !  REL
      write(alfs(14),'(f6.3)') xlimit6(probmp(1),3)          !  MPROB
      write(alfs(15),'(f6.3)') xlimit6(sprob(1),3)           !  S PRB
      write(alfs(16),'(f6.2)') xlimit6(gaint(1),2)           !  TGAIN
      write(alfs(17),'(f6.2)') xlimit6(gainr(1),2)           !  RGAIN
      write(alfs(18),'(f6.1)') xlimit6(snxx(1),1)            !  SNRxx
ccc      write(alfs(19),'(f6.1)') xlimit6(dbw(1)+30.,1)         !  DBM
      write(alfs(19),'(f6.2)') xlimit6(DU_NOIS(1),2)         !  DU NOISE
      write(alfs(20),'(f6.2)') xlimit6(DL_NOIS(1),2)         !  DL NOISE
      write(alfs(21),'(f6.2)') xlimit6(DBLOSL(1),2)          !  SIG LW
      write(alfs(22),'(f6.2)') xlimit6(DBLOSU(1),2)          !  SIG UP
      write(alfs(23),'(f6.3)') xlimit6(power_cut,3)          !  PWRCT
      angr=angler(1)
      if(angr.le.0.) angr=angle(1)
      write(alfs(24),'(f6.2)') xlimit6(angr,2)               !  ANGLER

      if(areach.eq.'A') then             !  normal Area Coverage
         xlatd=rlatd
         xlongd=rlongd
      else                             !  inverse Area Coverage
         xlatd=tlatd
         xlongd=tlongd
      end if
      if(iproj.ne.7) then    !  Lat/Lon projection, see if Longitude needs adjustment
         if(xmin.lt.0.) then
            if(ix.eq.1 .or. xlongd.gt.180.) xlongd=xlongd-360.
         end if
      end if
      if(xlongd.lt.-359.) xlongd=0.   !  probably North or south pole caused problem

      if(nf.eq.1) then
      write(LUO,'(2i3,2f10.4,24a6)') ix,iy,xlatd,xlongd,alfs
      else
      write(LUO,'(2i3,2f10.4,23a6)') ix,iy,xlatd,xlongd,alfs(1),alfs(8),
     +            alfs(9),alfs(11),alfs(13),alfs(15),alfs(18)
      end if
      return

100   write(LUO,101) VERSN
101   format('VOACAP Version',1x,a8)
      path='     '
      if(NPSL.ne.0) path='/Long'     !  using long path
      itr=1
      call TxPwr(pwrkw(itr),alfPwr,nchp)
      daily='   '
      if(idaily(1).ne.0) write(daily,'(1h.,i2.2)') idaily(1)
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
         write(antbear,'(i3,3hdeg)') iabs(nint(beammain(itr)))
      else
         mainbeam=nint(beammain(itr))+islew       !  add slew back in
         if(mainbeam.lt.0.) mainbeam=mainbeam+360
         write(antbear,'(1h(,i3,1h,,i3,4h)deg)') mainbeam,islew 
      end if
c**********************************************************
      if(nf.eq.1) then
      write(label,102) ITRAN,antname(itr)(1:10),alfPwr(1:nchp),
     +                 antbear,ihours(1),Frel(1),
     +                 IMON(months(1)),daily,nint(sunsp(1)),path
102   format(2a10,1h[,a10,2h] ,a,1x,a,i3.2,'ut',f7.3,'MHz ',2a3,
     +       i4,'ssn',a)
      else
      write(label,202) ITRAN,antname(itr)(1:10),alfPwr(1:nchp),
     +                 antbear,ihours(1),nf,
     +                 IMON(months(1)),daily,nint(sunsp(1)),path
202   format(2a10,1h[,a10,2h] ,a,1x,a,i3.2,'ut',i3,'Freqs ',2a3,
     +       i4,'ssn',a)
      end if
      call squeez(label,90)      !  squeeze blanks out
      nch=lcount(label,90)
      if(iquiet.eq.0) write(*,'(1x,a)') label(1:nch)
c      call progress_label(label)
      write(LUO,'(a)') label(1:nch)
      if(nf.eq.1) then
      write(LUO,103) -ix,-iy
103   format(2i3,'  Latitude Longitude',
     +               '   MUF  MODE ANGLE DELAY VHITE MUFda  LOSS',
     +               '   DBU  SDBW  NDBW   SNR RPWRG   REL MPROB',
     +         ' SPROB TGAIN RGAIN SNRxx    DU    DL SIGLW SIGUP PWRCT',
     +         'ANGLER')
      else
      write(LUO,104) -ix,-iy
104   format(2i3,'  Latitude Longitude',
     +               '   MUF   DBU  SDBW   SNR   REL SPROB SNRxx')
      end if
      RETURN
      END
C---------------------------------
c      subroutine progress_label(label)
c      include <windows.ins>
c      character label*(*)
c      common /Cprogress/ iarea_batch,alf_label
c         character alf_label*80
c      if(iarea_batch.eq.0) return
c      alf_label=label
c      call window_update@(alf_label)
c      RETURN
c      END
C---------------------------------

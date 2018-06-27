      subroutine antcalc_ciraf(tlat,tlon,rlat,rlon,frel)
      dimension frel(*)
c****************************************************************
c                         =   = point-to-point method CIRAF case
c****************************************************************
      common /cantenna/ numants,iats( 5),anttype( 6),antname( 6),
     +                  xfqs( 5),xfqe( 5),designfreq( 5),antfile( 5),

     +                  beammain( 5),offazim_z( 5),cond( 5),diel( 5),
     +                  array(30,91,12),aeff(30, 5)
      character anttype*10,antname*70,antfile*24
      common /crun_directory/ run_directory
         character run_directory*50
      common /designfreq/ freqdesign
      common /ccirparm/ parm(20),z6,umax,vmax,giso,
     +                  modegain,gainmax(3,2),gainmaxb(30),antnam
      character antnam*20
ccc      common /antnamec/ antname
ccc         character antname*20
      common /Cant_dat/ num_ants,ant_alf(5)
         character ant_alf*80
c****************************************************************
      CHARACTER antfilex*21,description*70
      character antfile40*40
c****************************************************************
      character fileant*10,alf*80,anttypex*10
c****************************************************************
      dimension antgains(30,91),efficiency(30),ztaz(2),aex(4)
      dimension gains1(91),gains2(91)
      dimension freq_calc(10)
      equivalence (freq_calc,aeff)
c****************************************************************
      nch_run=lcount(run_directory,50)
      do 400 i=1,30
      efficiency(i)=0.
      do 400 j=1,91
400   antgains(i,j)=0.
      do 5 if=1,10
      if(frel(if).gt.1.) nfreq=if
5     continue
c****************************************************************
      do 500 iant=1,1        !  num_ants
      alf=ant_alf(iant)
      design_freq=designfreq(iant)
      beam_main=beammain(iant)
      antfilex=antfile(iant)
      itr=1
      idx=1
         call dazel0(tlat,tlon,rlat,rlon,ztaz(1),zdgc)    !  calc azimuth T-R
         call dazel0(rlat,rlon,tlat,tlon,ztaz(2),zdgc)    !  calc azimuth R-T
c*****************************************************************
c          CIRAF test points antenna calculations
c          nFreq , 1 azimuth, 91 elevation angles
c*****************************************************************
         do 15 if=1,nfreq
         do 15 ielev=0,90
15       antgains(if,ielev+1)=0.
         antfile40=antfilex//'                   '
         call readant(37,antfile40,description)
         jant=nint(parm(2))                !  antenna type
         freqdesign=design_freq
         if(jant.eq.0) parm(1)=freqdesign   !  set isotrope gain
ccc         call antmodel(jant,freqdesign,anttypex)
         if(jant.le.12) then                            !  CCIR antenna
            do 20 ifreq=1,nfreq
c            call yieldit       !  yield for windows operations
            freq=frel(ifreq)
            parm(5)=freq
            call antinit2
ccc            call setmaxgain(freq)
            do 20 ielev=0,90
            delev=ielev
20          call antcal(beam_main,ztaz(itr),delev,
     +                  antgains(ifreq,ielev+1))
         else if(jant.eq.13) then                       !  Area gain table
            offazim=ztaz(itr)-beam_main    !  off azimuth
            if(offazim.lt.0.) offazim=offazim+360.
            iazim=offazim
            iazim2=iazim+1
            if(iazim2.eq.360) iazim2=0
            read(13,rec=iazim+1) gains1
            read(13,rec=iazim2+1) gains2
            fract=offazim-float(iazim)
            do 22 ielev=0,90
            g1=gains1(ielev+1)
            g2=gains2(ielev+1)
            g=g1 + (g2-g1)*fract + parm(1)
            do 22 ifreq=1,nfreq
22          antgains(ifreq,ielev+1)=g
         else if(jant.eq.14) then                   !  Point-to-Point gain table
c               NOT VALID
         else if(jant.ge.21 .and. jant.le.30) then      !  IONCAP antenna
            indx=jant-20
            call ioninit(indx,parm,asig,aeps,and,anl,anh,aex)
            offazim=ztaz(itr)-beam_main    !  off azimuth
            if(offazim.lt.0.) offazim=offazim+360.
            do 30 ifreq=1,nfreq
c            call yieldit       !  yield for windows operations
            freq=frel(ifreq)
            parm(5)=freq
            do 30 ielev=0,90
            delev=float(ielev)*.01745329          !  elevation angle in radians
30          call iongain(indx,offazim,asig,aeps,and,anl,anh,aex,
     +            delev,freq,antgains(ifreq,ielev+1),efficiency(ifreq))
         else if(jant.ge.31 .and. jant.le.47) then      !  HFMUFES antenna
            indx=jant-30
            call mufesint(indx,parm,asig,aeps,and,anl,anh,aex)
            offazim=ztaz(itr)-beam_main    !  off azimuth
            if(offazim.lt.0.) offazim=offazim+360.
            do 40 ifreq=1,nfreq
c            call yieldit       !  yield for windows operations
            freq=frel(ifreq)
            parm(5)=freq
            kas=0
            do 40 ielev=0,90
            delev=float(ielev)*.01745329          !  elevation angle in radians
40         call mufesgan(indx,kas+ielev,offazim,asig,aeps,and,anl,anh,
     +       aex,delev,freq,antgains(ifreq,ielev+1),efficiency(ifreq))
         else if(jant.eq.48) then      !  NOSC antenna models
            offazim=ztaz(itr)-beam_main    !  off azimuth
            if(offazim.lt.0.) offazim=offazim+360.
            do 45 ifreq=1,nfreq
c            call yieldit       !  yield for windows operations
            freq=frel(ifreq)
            do 45 ielev=0,90
            elev=float(ielev)          !  elevation angle in degrees
45         call invcon(30,freq,elev,antgains(ifreq,ielev+1))
         end if
         offazim=ztaz(itr)-beam_main    !  off azimuth
         if(offazim.lt.0.) offazim=offazim+360.
ccc         offazim_z(iant)=offazim
ccc         open(17,file='antciraf.dmp')
ccc         rewind(17)
         do 95 ifreq=1,nfreq
         freq_calc(ifreq)=frel(ifreq)
ccc         write(17,'('' in antciraf, freq='',f9.3)') frel(ifreq)
ccc         write(17,'(10f6.1)') (antgains(ifreq,j),j=1,91)
         do 95 j=1,91
95       array(ifreq,j,iant)=antgains(ifreq,j)
ccc         close(17)
c***************************************************************
500   continue
      offazim_z(1)=-2000.        !  CIRAF flag
      return
      END
c--------------------------------------------------------------------- *

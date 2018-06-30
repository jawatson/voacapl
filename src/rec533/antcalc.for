      subroutine antcalc(filename,method)
      use crun_directory
      use voacapl_defs
c****************************************************************
c          Execute with:
c             antcalc filename method
c          where:
c                filename = propagation input data file to process
c                           (e.g. voacapx.dat)
c                method   = a = area coverage method
c                         =   = point-to-point method
c****************************************************************
      common /cQUIET/ iquiet     !  =1=SILENT
c      common /crun_directory/ run_directory
c         character run_directory*50
      common /designfreq/ freqdesign
      common /ccirparm/ parm(20),z6,umax,vmax,giso,
     +                  modegain,gainmax(3,2),gainmaxb(30),antnam
      character antnam*20,antname*20
      common /antnamec/ antname
      common /Cant_dat/ numants,ant_alf(5)
         character ant_alf*80
c****************************************************************
      CHARACTER xmtrecv(2)*8,antfile*21,description*70
      character antfile40*40
c****************************************************************
      character tlatns*1,tlonew*1,rlatns*1,rlonew*1,path*1
      character filename*20,fileant*10,alf*80,anttype*10,method*1
c****************************************************************
      dimension antgains(30,91),efficiency(30),ztaz(2),aex(4)
      dimension areagain(91),gains1(91),gains2(91),freqarea(11)
ccc      data antgains/2730*0./
ccc      data efficiency/30*0./
      data xmtrecv/'TRANSMIT','RECEIVE'/
      data tlat,tlon,rlat,rlon/0.,0.,0.,0./
c****************************************************************
      do 400 i=1,30
      efficiency(i)=0.
      do 400 j=1,91
400   antgains(i,j)=0.
      numants=0
c****************************************************************
ccc      if(method.eq.'A') method='a'
c****************************************************************
ccc      write(*,'('' OPENing file='',a)') filename
c      nch_run=lcount(run_directory,50)
      open(21,file=trim(run_directory)//PATH_SEPARATOR//filename,status='old',err=900)
      rewind(21)
c          read once to fill freqarea array
      do 505 i=1,11
505   freqarea(i)=0.
510   read(21,'(a)',end=520) alf
      if(alf(1:9).eq.'FREQUENCY') then  !read frequency for area coverage
         read(alf,'(10x,11f5.2)') freqarea   !  ICEPAC & VOACAP
      else if(alf(1:9).eq.'FREQ     ') then  !read frequency for area coverage
         read(alf,'(10x,10f7.3)') (freqarea(i),i=1,10)   !  REC533
      end if
      go to 510
520   rewind(21)

10    read(21,'(a)',end=100) alf
      if(alf(1:10).eq.'CIRCUIT   ') then
         read(alf,1) tlat,tlatns,tlon,tlonew,
     +               rlat,rlatns,rlon,rlonew,path
1        format(10x,f5.2,a1,3(f9.2,a1),2x,a)
         if(tlatns.eq.'S') tlat=-tlat
         if(tlonew.eq.'W') tlon=-tlon
         if(rlatns.eq.'S') rlat=-rlat
         if(rlonew.eq.'W') rlon=-rlon
         if(path.ne.'L') path='S'       !  default short path

      else if(alf(1:9).eq.'FREQUENCY') then  !read frequency for area coverage
         read(alf,'(10x,11f5.2)') freqarea   !  ICEPAC & VOACAP
      else if(alf(1:9).eq.'FREQ     ') then  !read frequency for area coverage
         read(alf,'(10x,10f7.3)') (freqarea(i),i=1,10)   !  REC533

      else if(alf(1:10).eq.'TRANS     ') then
         read(alf,2) tlat,tlatns,tlon,tlonew
2        format(30x,f7.2,a1,f10.2,a1,2x,a)
         if(tlatns.eq.'S') tlat=-tlat
         if(tlonew.eq.'W') tlon=-tlon

      else if(alf(1:10).eq.'RECVR     ') then
         read(alf,2) rlat,rlatns,rlon,rlonew,path
         if(rlatns.eq.'S') rlat=-rlat
         if(rlonew.eq.'W') rlon=-rlon
         if(path.ne.'L') path='S'       !  default short path

      else if(alf(1:10).eq.'ANTENNA   ') then
         numants=numants+1
         ant_alf(numants)=alf        !  save antenna for special CIRAF case
         call dazel0(tlat,tlon,rlat,rlon,ztaz(1),zdgc)    !  calc azimuth T-R
         call dazel0(rlat,rlon,tlat,tlon,ztaz(2),zdgc)    !  calc azimuth R-T
         if(path.eq.'L') then        !  other way around earth
            ztaz(1)=ztaz(1)+180.
            if(ztaz(1).ge.360.) ztaz(1)=ztaz(1)-360.
            ztaz(2)=ztaz(2)+180.
            if(ztaz(2).ge.360.) ztaz(2)=ztaz(2)-360.
         end if
         read(alf,3) itr,idx,minfreq,maxfreq,design_freq,
     +               antfile,beam_main,rgain
3        format(10x,4i5,f10.3,1x,a21,1x,f5.1,f10.4)
         if(itr.eq.2 .and. rgain.ne.0.) design_freq=rgain   ! fix isotrope gain
         write(fileant,'(4hgain,i2.2,4h.dat)') idx
         open(22,file=trim(run_directory)//PATH_SEPARATOR//fileant,form='formatted')
         rewind(22)
         fs=minfreq
         fe=maxfreq
         if(freqarea(2).ne.0.) go to 200      !  point-to-point
         if(method.ne.'A') go to 200          !  point-to-point
c*****************************************************************
c          Area coverage  antenna calculations
c          1 Freq, 360 azimuths, 91 elevation angles
c*****************************************************************
         antfile40=antfile//'                   '
         call readant(37,antfile40,description)
         jant=nint(parm(2))                !  antenna type
         freqdesign=design_freq
         if(jant.eq.0) parm(1)=freqdesign   !  set isotrope gain
         call antmodel(jant,freqdesign,anttype)
         write(*,118) xmtrecv(itr),anttype,antfile,description(1:15),
     +                beam_main
118      format(4x,'    ',a8,'=',a10,1h[,a21,1h],'=',a,'  beam=',f6.1)
         write(22,'(a)') anttype//description
         write(22,119) fs,fe,beam_main,-999.,parm(4),parm(3)
119      format(2f5.0,2f7.2,2f10.5)
         parm(5)=freqarea(1)
         if(jant.le.12) then                            !  CCIR antenna
            call antinit2
            eff=0.
            if(jant.eq.11) eff=parm(3) !  efficiency
            if(jant.eq.7 .and. beam_main.lt.-1.) then   ! NON-terminated Rhombic
               do 125 iazim=0,359
c               call yieldit       !  yield for windows operations
               azimuth=iazim
               if(iazim.ge.91 .and. iazim.le.180) then
                  azimuth=180-iazim
               else if(iazim.ge.181 .and. iazim.le.269) then
                  azimuth=180-iazim+360
               end if
               do 120 ielev=0,90
               delev=ielev
120            call antcal(0.,azimuth,delev,areagain(ielev+1))
               call antsave(idx,iazim,freqarea(1),eff,areagain)
125            continue
            else
               do 135 iazim=0,359
c               call yieldit       !  yield for windows operations
               azimuth=iazim
               do 130 ielev=0,90
               delev=ielev
130            call antcal(0.,azimuth,delev,areagain(ielev+1))
               call antsave(idx,iazim,freqarea(1),eff,areagain)
135            continue
            end if
         else if(jant.eq.13) then      !  360 gain table
            eff=parm(3)         !  efficiency
            do 150 iazim=0,359
c            call yieldit       !  yield for windows operations
            read(13,rec=iazim+1) areagain
            do 140 ielev=1,91
140         areagain(ielev)=areagain(ielev)+parm(1)     !  additional gain
150         call antsave(idx,iazim,freqarea(1),eff,areagain)
         else if(jant.eq.14) then      !  Point-to-Point gain table
            ifreq=freqarea(1)
            read(14,rec=ifreq) eff1,gains1
            ifreq2=ifreq+1
            if(ifreq2.gt.30) ifreq2=30
            read(14,rec=ifreq2) eff2,gains2
            fract=freqarea(1)-ifreq
            eff=eff1 + (eff2-eff1)*fract
            do 160 iazim=0,359
c            call yieldit       !  yield for windows operations
            do 155 ielev=1,91
155         areagain(ielev)=parm(1) + gains1(ielev) +
     +                      (gains2(ielev)-gains1(ielev))*fract
160         call antsave(idx,iazim,freqarea(1),eff,areagain)
         else if(jant.ge.21 .and. jant.le.30) then      !  IONCAP antenna
            indx=jant-20
            call ioninit(indx,parm,asig,aeps,and,anl,anh,aex)
            azimuth=0.
            delev=6.*.0174533     !  get eff at 6 degrees
            call iongain(indx,azimuth,asig,aeps,and,anl,anh,aex,
     +            delev,freqarea(1),areagain,eff)
            do 190 iazim=0,359
c            call yieldit       !  yield for windows operations
            azimuth=iazim
            do 180 ielev=0,90
            delev=float(ielev)*.0174533
180         call iongain(indx,azimuth,asig,aeps,and,anl,anh,aex,
     +            delev,freqarea(1),areagain(ielev+1),eff)
190         call antsave(idx,iazim,freqarea(1),eff,areagain)
         else if(jant.ge.31 .and. jant.le.47) then      !  HFMUFES antenna
            indx=jant-30
            call mufesint(indx,parm,asig,aeps,and,anl,anh,aex)
            azimuth=0.
            delev=6.*.0174533     !  get eff at 6 degrees
            call mufesgan(indx,1,azimuth,asig,aeps,and,anl,anh,aex,
     +            delev,freqarea(1),areagain,eff)
            do 199 iazim=0,359
c            call yieldit       !  yield for windows operations
            azimuth=iazim
            do 195 ielev=0,90
            delev=float(ielev)*.0174533
195         call mufesgan(indx,2,azimuth,asig,aeps,and,anl,anh,aex,
     +            delev,freqarea(1),areagain(ielev+1),eff)
199         call antsave(idx,iazim,freqarea(1),eff,areagain)
         else if(jant.eq.48) then      !  NOSC antenna models
            eff=0.
            do 210 iazim=0,359
c            call yieldit       !  yield for windows operations
            azimuth=iazim
            do 205 ielev=0,90
            delev=float(ielev)
205         call invcon(30,freqarea(1),elev,areagain(ielev+1))
210         call antsave(idx,iazim,freqarea(1),eff,areagain)
         else if(jant.ge.90) then      !  External antenna calculator program
            close(22)
            call harris(jant,fileant,'a',idx,antfile,freqarea(1),30.,
     +                  beam_main,-999.)
            go to 301
         end if
         go to 300
c*****************************************************************
c          Point-to-point antenna calculations
c          Freqs 2-30, 1 azimuth, 91 elevation angles
c*****************************************************************
200      do 15 if=1,30
         efficiency(if)=0.
         do 15 ielev=0,90
15       antgains(if,ielev+1)=0.
         antfile40=antfile//'                   '
         call readant(37,antfile40,description)
         jant=nint(parm(2))                !  antenna type
         freqdesign=design_freq
         if(jant.eq.0) parm(1)=freqdesign   !  set isotrope gain
         call antmodel(jant,freqdesign,anttype)
         if(jant.le.12) then                            !  CCIR antenna
            do 20 ifreq=minfreq,maxfreq
c            call yieldit       !  yield for windows operations
            freq=ifreq
            parm(5)=freq
            call antinit2
ccc            call setmaxgain(freq)
            if(jant.eq.11) efficiency(ifreq)=parm(3) !  efficiency
            do 20 ielev=0,90
            delev=ielev
20          call antcal(beam_main,ztaz(itr),delev,
     +                  antgains(ifreq,ielev+1))
         else if(jant.eq.13) then                       !  Area gain table
            do 21 ifreq=minfreq,maxfreq
21          efficiency(ifreq)=parm(3)      !  efficiency
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
            do 22 ifreq=minfreq,maxfreq
22          antgains(ifreq,ielev+1)=g
         else if(jant.eq.14) then                   !  Point-to-Point gain table
            do 25 ifreq=minfreq,maxfreq
c            call yieldit       !  yield for windows operations
            read(14,rec=ifreq) efficiency(ifreq),
     +                         (antgains(ifreq,ielev+1),ielev=0,90)
            do 25 ielev=0,90
25          antgains(ifreq,ielev+1)=antgains(ifreq,ielev+1)+parm(1)
         else if(jant.ge.21 .and. jant.le.30) then      !  IONCAP antenna
            indx=jant-20
            call ioninit(indx,parm,asig,aeps,and,anl,anh,aex)
            offazim=ztaz(itr)-beam_main    !  off azimuth
            if(offazim.lt.0.) offazim=offazim+360.
            do 30 ifreq=minfreq,maxfreq
c            call yieldit       !  yield for windows operations
            freq=ifreq
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
            do 40 ifreq=minfreq,maxfreq
c            call yieldit       !  yield for windows operations
            freq=ifreq
            parm(5)=freq
            kas=0
            do 40 ielev=0,90
            delev=float(ielev)*.01745329          !  elevation angle in radians
40         call mufesgan(indx,kas+ielev,offazim,asig,aeps,and,anl,anh,
     +       aex,delev,freq,antgains(ifreq,ielev+1),efficiency(ifreq))
         else if(jant.eq.48) then      !  NOSC antenna models
            offazim=ztaz(itr)-beam_main    !  off azimuth
            if(offazim.lt.0.) offazim=offazim+360.
            do 45 ifreq=minfreq,maxfreq
c            call yieldit       !  yield for windows operations
            freq=ifreq
            do 45 ielev=0,90
            elev=float(ielev)          !  elevation angle in degrees
45         call invcon(30,freq,elev,antgains(ifreq,ielev+1))
         else if(jant.ge.90) then      !  External antenna calculator program
            offazim=ztaz(itr)-beam_main    !  off azimuth
            if(offazim.lt.0.) offazim=offazim+360.
            close(22)
            call harris(jant,fileant,' ',idx,antfile,fs,fe,
     +                  beam_main,offazim)
            go to 301
         end if
         offazim=ztaz(itr)-beam_main    !  off azimuth
         if(offazim.lt.0.) offazim=offazim+360.
         write(22,'(a)') anttype//description
         write(22,94) fs,fe,beam_main,offazim,parm(4),parm(3)
94       format(2f5.0,2f7.2,2f10.5)
         do 95 ifreq=1,30
95       write(22,96) ifreq,efficiency(ifreq),
     +                (antgains(ifreq,j),j=1,91)
96       format(i2,f6.2,(t10,10f7.3))
         if(iquiet.eq.0)      !  not SILENT
     +   write(*,101) xmtrecv(itr),anttype,antfile,description(1:10),
     +                beam_main,ztaz(itr)
101      format(1x,a8,'=',a10,1h[,a21,1h],'=',a10,'  beam=',f6.1,
     +          '  az=',f6.1)
ccc         write(*,102) (antgains(6,j),j=1,91)
ccc102      format(10x,10f6.1)
c***************************************************************
300      close(22)
301      continue

      end if
      go to 10
100   close(21)
      go to 999
c****************************************************************
900   write(*,901) filename
901   format(' In ANTCALC, could not OPEN file=',a)
999   continue
      return
      END
c--------------------------------------------------------------------- *
      subroutine antsave(iantr,iazim,freq,eff,areagain)
c        save into /cantenna/ array to save DECRED the effort
c        for area coverage, we can remove the write & read since antcalc
c        is back inside the propagation program.
      dimension areagain(91)
      common /cantenna/ numants,iats( 5),anttype( 6),antname( 6),
     +                  xfqs( 5),xfqe( 5),designfreq( 5),antfile( 5),
     +                  beammain( 5),offazim( 5),cond( 5),diel( 5),
     +                  array(30,91,12),aeff(30, 5)
      character anttype*10,antname*70,antfile*24
      integer*2 iarray360(360,91,2)
      equivalence (array,iarray360)        !  for area coverage
      aeff(1,iantr)=eff
      do 10 ielev=1,91
      gain=areagain(ielev)
      if(gain.gt. 300.) gain= 300.
      if(gain.lt.-300.) gain=-300.
10    iarray360(iazim+1,ielev,iantr)=nint(gain*100.)
ccc            if(iazim.eq.0) write(22,14) freq,eff
ccc14          format(10x,f7.3,'MHz eff=',f10.3)
ccc            write(22,121) iazim,areagain
ccc121         format(i5,(t10,10f7.3))
      return
      end
c--------------------------------------------------------------------- *

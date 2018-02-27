      subroutine readant(lu,filename,description)
      use voacapl_defs
      common /crun_directory/ run_directory
         character run_directory*50
      common /designfreq/ freqdesign
      common /ctype11g/ gain_type11(91)
      common /gain_10/ gain10(90,29)
      common /ccirparm/ parm(20),z6,umax,vmax,giso,
     +                  modegain,gainmax(3,2),gainmaxb(30)
      character filename*(*),description*70,alf*80

      dimension gains(91)
      equivalence (gains,gain_type11)
      nc1=len_trim(filename)
      alf=filename
      nch=80
      call rblankc(alf,nch)
ccc      nch=len_trim(alf)
      if(nch.le.2 .or. alf(nch:nch).eq.PATH_SEPARATOR) then  ! no file name = 0 dB gain
         description='0 dB gain'
         n=2
         parm(1)=0
         parm(2)=0
         modegain=0               !  use giso as max gain
         freqdesign=0.      !  default design freq=operating freq
         return
      end if
      nch_run=lcount(run_directory,50)
      open(lu,file=run_directory(1:nch_run-3)//'antennas'//PATH_SEPARATOR//alf(1:nch),status='old',iostat=ios,err=900)
      rewind(lu)
      read(lu,'(a)',err=910) description
      read(lu,*,err=910) n
      do 10 i=1,n
      read(lu,*,err=910) parm(i)
10    continue
      do 110 i=n+1,20
110   parm(i)=0.
      jant=nint(parm(2))       !  CCIR antenna type
      modegain=0               !  use giso as max gain
      if((jant.ge.1 .and.jant.le.4) .or. jant.eq.8 .or. jant.eq.9)
     +then
          modegain=1     !  interpolate on operating & design freq
          read(lu,*,err=910) (gainmax(i,1),i=1,3)
          read(lu,*,err=910) (gainmax(i,2),i=1,3)
      else if(jant.ge.5 .and.jant.le.7) then
          modegain=2     !  interpolate on operating freq
          read(lu,11,err=910) (gainmaxb(i),i=1,30)
11        format(10x,10f6.2)
      else if(jant.eq.10) then        !  vertical monopole
          modegain=2     !  interpolate on operating freq
          do 15 ifreq=2,30
15        read(lu,16,err=910) gainmaxb(ifreq),
     +                        (gain10(ielev,ifreq-1),ielev=1,90)
16        format(10x,f6.2,(t19,10f6.2))
      else if(jant.eq.11) then    !  type 11 = gain table at 91 elevation angles
         read(lu,*) gain_type11
      else if(jant.eq.12) then    !  read gain normalizing for Non-standards
          modegain=3     !  gain normalizing factor
          read(lu,17,err=920,end=920) (gainmaxb(i),i=1,30)
17        format(10x,5f10.3)
      else if(jant.eq.13) then    !  type 13 = gain table at 360x91 values
ccc         open(13,status='scratch',access='direct',form='unformatted',
ccc     +                recl=91*4)
c          Windows-95 cannot open SCRATCH files
         open(13,file=run_directory(1:nch_run)//PATH_SEPARATOR//'type13.tmp',
     +        access='direct',form='unformatted',recl=91*4)
         do 170 iang=0,359
         read(lu,'(9x,10f7.3)') gains
170      write(13,rec=iang+1) gains
      else if(jant.eq.14) then    !  type 14 = gain table at 30x91 values
ccc         open(14,status='scratch',
ccc     +        access='direct',form='unformatted',recl=92*4)
c          Windows-95 cannot open SCRATCH files
         open(14,file=run_directory(1:nch_run)//PATH_SEPARATOR//'type14.tmp',
     +        access='direct',form='unformatted',recl=92*4)
         do 180 ifreq=1,30
         read(lu,'(2x,f6.1,(t10,10f7.3))') eff,gains
180      write(14,rec=ifreq) eff,gains
      end if
20    freqdesign=0.      !  default design freq=operating freq
      close(lu)
ccc      call antinit2      !  initialize CCIR antenna routines
      return
900   write(*,901) ios,run_directory(1:nch_run-3)//'antennas'//PATH_SEPARATOR//alf(1:nch)
901   format(' Error=',i5,' OPENing antenna file=',a,1h.)
      write(*,'('' nch='',i5)') nch
      write(*,'('' nc1='',i5,''  filename='',a)') nc1,filename(1:nc1)
      pause 'error in readant'
      stop 'error in readant'
910   write(*,911) filename
911   format(' Error reading antenna file=',a)
      pause 'error in readant'
      stop 'error in readant'
920   do 925 i=1,30           !  initailize gain normalizing values
925   gainmaxb(i)=-99999.
      go to 20
      end
c------------------------------------------------------------------

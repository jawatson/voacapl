c# ioncap.f
      subroutine voacap(igraph)
c******************************************************************
c            this routine executes VOACAP 
c******************************************************************
      INCLUDE 'ficepac.hdr'
      common /cbotlines/ nbotlines,linesbot(14)
      common /ctoplines/ ntoplines,linestop( 7)
      common /crun_directory/ run_directory
         character run_directory*50
c******************************************************************
      character tns*1,tew*1,rns*1,rew*1,alf*80,antfile_name*21
      character path(2)*1,coefflist(2)*4
      data path/'S','L'/
      data coefflist/'CCIR','URSI'/
C******************************************************************
      character(len=1), parameter :: PATH_SEPARATOR ='/'
C******************************************************************
      nch_run=lcount(run_directory,50)
      if(igraph.eq.0) then
         open(31,file=run_directory(1:nch_run)//PATH_SEPARATOR//'voacapx.dat')
      else
         open(31,file=run_directory(1:nch_run)//PATH_SEPARATOR//'voacapg.dat')
      end if
      rewind(31)
c***************************************************************
c          Copy defaults from VOACAP.DEF
      open(32,file=run_directory(1:nch_run-3)//'database'//PATH_SEPARATOR//'voacap.def',
     +     status='old',err=100)
90    read(32,'(a)',end=99) alf
      nch=lcount(alf,80)
      write(31,'(a)') alf(1:nch)
      go to 90
99    close(32)
c***************************************************************
100   write(31,101) coefflist(icoeffs)(1:4)
101   format('COEFFS    ',a4)
c***************************************************************
      if(igraph.eq.0) then               !  circuit
         if(method.eq.11 .or. method.eq.28) then
            write(31,3) 1,24,1,1         !  MUF-LUF plots
         else
            iflag=-1                     !  LMT
            if(itimecode.eq.1) iflag=1   !  UT
            write(31,3) ihr1,ihr2,ihrinc,iflag
3           format('TIME      ',4i5)
         end if
      else                               !  graph, force 24 hours UT
         write(31,3) 1,24,1,1
      end if

      do 120 n=1,10
      if(montha(n).eq.0.) go to 125
120   continue
      n=11
125   n=n-1
ccc      write(31,4) year,(int(montha(i)),i=1,n)
ccc4     format('MONTH     ',11i5)
      write(31,4) year,(montha(i),i=1,n)
4     format('MONTH     ',i5,10f5.2)
      write(31,2) (float(ssna(i)),i=1,n)
2     format('SUNSPOT   ',12f5.0)

      call getTR(tlat,tlon,rlat,rlon)
      call benchfix(rname,rlat,rlon)      !  fix coordinates for benchmark
      write(31,5) tname,rname
5     format('LABEL     ',2a20)
      tlatd=tlat
      tns='N'
      if(tlatd.lt.0.) tns='S'
      tlond=tlon
      if(tlond.gt.180.) tlond=tlond-360.
      tew='E'
      if(tlond.lt.0.) tew='W'
      rlatd=rlat
      rns='N'
      if(rlatd.lt.0.) rns='S'
      rlond=rlon
      if(rlond.gt.180.) rlond=rlond-360.
      rew='E'
      if(rlond.lt.0.) rew='W'
      write(31,6) abs(tlatd),tns,abs(tlond),tew,
     +            abs(rlatd),rns,abs(rlond),rew,path(ipath),ipath-1
6     format('CIRCUIT   ',f5.2,a1,3(f9.2,a1),2x,a1,1x,i5)
      xnoise=noise
      if(noise.eq.1) xnoise=-1.       !  -140.4
      if(noise.eq.2) xnoise=-2.       !  -144.7
      if(noise.eq.3) xnoise=-3.       !  -150.0
      if(noise.eq.4) xnoise=-4.       !  -164.1
      write(31,111) 1.0,xnoise,amind,float(xlufp),float(rsn),pmp,dmpx
111   format('SYSTEM    ',f5.0,f5.0,f5.2,f5.0,3f5.2)
      write(31,13) fprob
13    format('FPROB     ',4f5.2)
      do 10 i=1,numants
      call antfile(xmtr_dir(i),xmtr_file(i),antfile_name)
10    write(31,11) 1,i,minfreq(i),maxfreq(i),design_freq(i),
     +             antfile_name,beam_main(i),TxPower(i)
11    format('ANTENNA   ',4i5,f10.3,1h[,a21,1h],f5.1,f10.4)
      call antfile(rec_dir,rec_file,antfile_name)
      write(31,11) 2,numants+1,2,30,0.,antfile_name,rec_bear,rec_gain

      if(igraph.eq.0) then
         write(31,7) (float(freq(i))/1000.,i=1,11)
7        format('FREQUENCY ',11f5.2)
      else
            write(31,17) (freq(i),i=1,11)
17          format('COMMENT   FREQ ',11i5)
ccc         write(31,8) 26,0                 !  method 26 is LUF-MUF table
ccc         write(31,'(7HEXECUTE)')
         write(31,7) -1.                  !  do all frequencies for graph
      end if

      npage0=0
      if(igraph.eq.0) then
         write(31,8) method,npage0
8        format('METHOD    ',2i5)
      else
         met=method
         if(met.lt.16 .or. met.gt.23) met=16
         if(method.eq.30) met=30      !  method 30 is allowed for graphs
         write(31,8) met,npage0
      end if
      if(method.eq.23) then
         if(nbotlines.eq.0) then
            nbotlines=7
            linesbot(1)=1         !  MODE
            linesbot(2)=2         !  ANGLE
            linesbot(3)=19        !  Tgain
            linesbot(4)=7         !  DBU
            linesbot(5)=10        !  SNR
            linesbot(6)=21        !  SNR90
            linesbot(7)=12        !  REL
         end if
         write(31,23) (linesbot(i),i=1,nbotlines)
23       format('BOTLINES  ',14i5)
         if(ntoplines.eq.0) then
            ntoplines=7
            do 123 i=1,7
123         linestop(i)=i        !  turn on all
         end if
         write(31,24) (linestop(i),i=1,ntoplines)
24       format('TOPLINES  ',7i5)

      end if

      write(31,'(7HEXECUTE)')

      write(31,'(4HQUIT)')
      close(31)
ccc      if(igraph.eq.0) then
ccc         if(method.eq.11 .or.method.eq.28) stop 25   !  graphics plots
ccc         stop 5
ccc      else
ccc         stop 15
ccc      end if
      end
c------------------------------------------------------------------

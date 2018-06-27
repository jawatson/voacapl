c# ioncap.f
      subroutine rec533(igraph)
c******************************************************************
c            this routine executes REC533 
c******************************************************************
      INCLUDE 'FICEPAC.HDR'
      common /crun_directory/ run_directory
         character run_directory*50
c******************************************************************
      character tns*1,tew*1,rns*1,rew*1,antfile_name*21
      character path(2)*1,coefflist(2)*4
      data path/'S','L'/
      data coefflist/'CCIR','URSI'/
C******************************************************************
      nch_run=lcount(run_directory,50)
      meth=6     !  force method 6 for all calculations
      if(igraph.eq.0) then
         open(31,file=run_directory(1:nch_run)//'\'//'rec533x.dat')
      else
         open(31,file=run_directory(1:nch_run)//'\'//'rec533g.dat')
      end if
      rewind(31)
      write(31,8) meth,ipath-1
8     format('OUTPUT    ',2i5)
c***************************************************************
      write(31,101) coefflist(icoeffs)(1:4)
101   format('COEFFS    ',a4)
c***************************************************************
      do 110 n=1,12
      if(ssna(n).eq.0.) go to 115
110   continue
      n=13
115   write(31,2) (float(ssna(i)),i=1,n-1)
2     format('SUNSPOT   ',12f5.0)
      if(igraph.eq.0) then
         write(31,3) ihr1,ihr2,ihrinc
      else
         write(31,3) 1,24,1
      end if
3     format('TIME      ',3i5)

      do 120 n=1,10
      if(montha(n).eq.0.) go to 125
120   continue
      n=11
125   write(31,4) year,(int(montha(i)),i=1,n-1)
4     format('MONTH     ',11i5)
      call getTR(tlat,tlon,rlat,rlon)
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
      write(31,6) 'TRANS     ',tname,abs(tlatd),tns,abs(tlond),tew,
     +                       path(ipath)
      write(31,6) 'RECVR     ',rname,abs(rlatd),rns,abs(rlond),rew,
     +                       path(ipath)
6     format(a10,a20,f7.2,a1,f10.2,a1,2x,a1,5x,1hD)
      write(31,1) 1.0,amind,0,0.
1     format('SYST      ',f10.4,f5.2,i5,f5.1)
      do 10 i=1,numants
      call antfile(xmtr_dir(i),xmtr_file(i),antfile_name)
10    write(31,11) 1,i,minfreq(i),maxfreq(i),design_freq(i),
     +             antfile_name,beam_main(i),TxPower(i)
11    format('ANTENNA   ',4i5,f10.3,1h[,a21,1h],f5.1,f10.4)
      call antfile(rec_dir,rec_file,antfile_name)
      write(31,11) 2,numants+1,2,30,0.,antfile_name,rec_bear,rec_gain
      nnoise=noise
      if(nnoise.ge.1 .and. nnoise.le.6) nnoise=-nnoise  !  noise category
      ibandw=pmp
      write(31,12) -nnoise,ibandw,rsn,xlufp
12    format('NOIS      ',4i5)

      if(igraph.eq.0) then
         write(31,7) (float(freq(i))/1000.,i=1,10)
7        format('FREQ      ',10f7.3)
      else
         write(31,'(''COMMENT   FREQ '',10i5)') (freq(i),i=1,10)
         write(31,7) -1.      !  Freq=-1= do all frequencies for graph
      end if

      write(31,'(7HEXEC   )')

      write(31,'(4HQUIT)')
      close(31)
ccc      if(igraph.eq.0) then
ccc         stop 5
ccc      else
ccc         stop 15
ccc      end if
      return
      end
c------------------------------------------------------------------

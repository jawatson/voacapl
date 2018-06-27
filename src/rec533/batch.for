      subroutine batch(lu,model,filein,icircuit,*)  ! Point-to-Point BATCH
c**************************************************************************
c          lu = file unit open of model.CIR
c          model = VOACAP, ICEPAC, or REC533
c          filein = file to create the input data on
c          icircuit = circuit number to be processed
c          * = alternate return when done or if error occurs
c**************************************************************************
         character model*6,filein*(*)

      INCLUDE 'ficepac.hdr'
      character path*5,antenna(5)*63
      character tlatns*1,tlonew*1,rlatns*1,rlonew*1
c**********************************************************************
      icircuit=icircuit+1
      read(lu,1,end=998) tname,itlatdeg,tlatns,itlatmin,
     +                         itlondeg,tlonew,itlonmin,
     +                   rname,irlatdeg,rlatns,irlatmin,
     +                         irlondeg,rlonew,irlonmin,path(1:1)
1     format(1x,a20,1x,i2,a1,i2,1x,i3,a1,i2,2x,
     +          a20,1x,i2,a1,i2,1x,i3,a1,i2,a)
      if(path(1:1).ne.'L') path='SHORT'
      if(path(1:1).eq.'L') path='LONG '
      tlat=float(itlatdeg) + float(itlatmin)/60.
      tlon=float(itlondeg) + float(itlonmin)/60.
      if(tlatns.eq.'S') tlat=-tlat
      if(tlonew.eq.'W') tlon=-tlon
      rlat=float(irlatdeg) + float(irlatmin)/60.
      rlon=float(irlondeg) + float(irlonmin)/60.
      if(rlatns.eq.'S') rlat=-rlat
      if(rlonew.eq.'W') rlon=-rlon
ccc      call benchfix(rname,rlat,rlon)
      call deg2chr(tlat,2,tlatdeg)
      call deg2chr(tlon,3,tlondeg)
      call deg2chr(rlat,2,rlatdeg)
      call deg2chr(rlon,3,rlondeg)
      call circant(lu,nant,antenna)
      numants=nant
      do 30 i=1,nant
      read(antenna(i),2) minfreq(i),maxfreq(i),xmtr_dir(i),xmtr_file(i),
     +                   design_freq(i),beam_main(i),TxPower(i)
2     format(2i5,2x,a8,1x,a12,3f10.3)
      if(beam_main(i).lt.0.) beam_main(i)=0.
30    continue
c************************
      igraph=0
      ipath=1
      if(path(1:1).eq.'L') ipath=2       !  long path
      write(*,51) icircuit,tname,rname,path
51    format(' circuit #',i3,5x,a,' <to> ',a,2x,a,' Path')
ccc      call icepac(igraph)
ccc      call voacap(igraph)
      call rec533(igraph)
      return
c********************************************************
998   write(*,'(i5,'' Batch circuits processed'')') icircuit-1
      write(*,997) model
997   format(' Batch processing for ',a,' is complete.',/
     +       ' You may close this output window now.')
      close(lu)
c          file does not exist or end of file encountered
      return 1
      end
c----------------------------------------------------------------

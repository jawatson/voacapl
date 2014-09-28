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
      call benchfix(rname,rlat,rlon)
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
ccc      if(model.eq.'ICEPAC') call icepac(igraph)
      call voacap(igraph)
ccc      if(model.eq.'REC533') call rec533(igraph)
      return
c********************************************************
998   write(*,'(i5,'' Batch circuits processed'')') icircuit-1
      write(*,997) model
997   format(' Batch processing for ',a,' is complete.') !,/
c     +       ' You may close this output window now.')
      close(lu)
c          file does not exist or end of file encountered
      return 1
      end
c----------------------------------------------------------------
      subroutine benchfix(rname,rlat,rlon)    !  fix coordinates for benchmark
      character rname*20
      dimension rlatb(27),rlonb(27)
      data rlatb/  50.50,  55.15,  59.83,  66.01,  63.10,  55.16,
     +             44.78,   4.99,  42.83,  44.30,  45.17,  43.44,
     +             38.82,  32.11,  24.01,  -7.11,  33.67,  32.09,
     +             29.81,  22.26,  15.37,   7.85,    .04, -24.30,
     +            -35.59,  37.72,  41.94/
      data rlonb/   8.13,  15.18,  25.38,  63.27,  94.28, 115.79,
     +            129.31, 153.26,  15.47,  23.92,  33.98,  59.11,
     +             75.98,  90.38, 102.63, 135.91,  15.86,  23.12,
     +             31.08,  49.29,  61.55,  73.00,  84.03, 123.09,
     +            173.09,  -1.04,  11.90/
      if(rname(1:1).ne.'#') return            !  not benchmark
      if(rname(2:2).lt.'0' .or. rname(2:2).gt.'9') return
      if(rname(3:3).lt.'0' .or. rname(3:3).gt.'9') return
      read(rname,'(1x,i2)') idx
      if(idx.lt.1 .or.idx.gt.27) return
      if(abs(rlat-rlatb(idx)).gt..02 .or.
     +   abs(rlon-rlonb(idx)).gt..02) return
      rlat=rlatb(idx)
      rlon=rlonb(idx)
      return
      end
c------------------------------------------------------------

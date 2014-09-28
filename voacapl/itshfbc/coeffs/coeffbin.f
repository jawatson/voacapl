      program coeffbin    !  make coeffxx.bin files from coeffxx.asc files
c**********************************************************************
c          This was created Jan 1995 by Greg Hand so that there would be
c          a common set of coefficients between ICEPAC, VOACAP, & REC533.
c          Also, text records were added to the ASCII files so that they
c          made some sense.
c**********************************************************************
      common /ccir/ 
     +ANEW(3),BNEW(3),ACHI(2),BCHI(2),FAKMAP(29,16),ABMAP(2,3),
     +F2D(16,6,6),DUD(5,12,5),FAM(14,12),SYS(9,16,6),PERR(9,4,6),
     +FAKP(29,16,6),FAKABP(2,6),
     +IKIM(10,6), XF2COF(13,76,2),
     +XESMCF(7,61,2),XPMAP(29,16,2),
     +XESLCF(5,55,2), XESUCF(5,55,2),
     +XFM3CF(9,49,2),
     +XERCOF(9,22,2)
      character filein*12,fileout*12,name*40
c**********************************************************************
c**********************************************************************
c     Modified to accept an (optional) arguement specifying 
c     the directory containing the .asc files.
      character (len=128) :: working_dir=''
      if (COMMAND_ARGUMENT_COUNT() == 1) then
         call GET_COMMAND_ARGUMENT(1, working_dir)
         working_dir = trim(working_dir)//"/"
      endif
c**********************************************************************
      do 100 month=1,12
      write(filein ,'(5hcoeff,i2.2,4h.asc)') month
      write(fileout,'(5hcoeff,i2.2,4h.bin)') month
      open(19,file=trim(working_dir)//filein,status='old')
      rewind(19)
      open(20,file=trim(working_dir)//fileout,form='unformatted')
      rewind(20)
      write(*,1) filein,fileout
1     format(' Using:',a,' to create:',a)
      read (19,2) mon
2     format(6x,i2)
      if(month.ne.mon) stop 'month does not match'
      read (19,98) name
      if(name(1:4).ne.'IKIM') stop 'name does not match IKIM'
      read (19,97) IKIM
97    format(10I5)
      read (19,98) name
      if(name(1:3).ne.'DUD') stop 'name does not match DUD'
      read (19,99) DUD
      read (19,98) name
      if(name(1:3).ne.'FAM') stop 'name does not match FAM'
      read (19,99) FAM
      read (19,98) name
      if(name(1:3).ne.'SYS') stop 'name does not match SYS'
      read (19,99) SYS
      read (19,98) name
      if(name(1:4).ne.'FAKP') stop 'name does not match FAKP'
      read (19,99) FAKP
      read (19,98) name
      if(name(1:6).ne.'FAKABP') stop 'name does not match FAKABP'
      read (19,99) FAKABP
      read (19,98) name
      if(name(1:6).ne.'XFM3CF') stop 'name does not match XFM3CF'
      read (19,99) XFM3CF
      read (19,98) name
      if(name(1:31).ne.'ANEW(3),BNEW(3),ACHI(2),BCHI(2)') 
     +   stop 'name does not match ANEW,BNEW,ACHI,BCHI'
      read (19,99) ANEW,BNEW,ACHI,BCHI
      read (19,98) name
      if(name(1:6).ne.'FAKMAP') stop 'name does not match FAKMAP'
      read (19,99) FAKMAP
      read (19,98) name
      if(name(1:5).ne.'ABMAP') stop 'name does not match ABMAP'
      read (19,99) ABMAP
      read (19,98) name
      if(name(1:3).ne.'F2D') stop 'name does not match F2D'
      read (19,99) F2D
      read (19,98) name
      if(name(1:4).ne.'PERR') stop 'name does not match PERR'
      read (19,99) PERR
      read (19,98) name
      if(name(1:6).ne.'XESMCF') stop 'name does not match XESMCF'
      read (19,99) XESMCF
      read (19,98) name
      if(name(1:5).ne.'XPMAP') stop 'name does not match XPMAP'
      read (19,99) XPMAP
      read (19,98) name
      if(name(1:6).ne.'XESLCF') stop 'name does not match XESLCF'
      read (19,99) XESLCF
      read (19,98) name
      if(name(1:6).ne.'XESUCF') stop 'name does not match XESUCF'
      read (19,99) XESUCF
      read (19,98) name
      if(name(1:6).ne.'XERCOF') stop 'name does not match XERCOF'
      read (19,99) XERCOF
98    format(a)
99    format(6e15.8)
      close(19)
c          these are common to all (icepac,voacap,rec533)
      write(20) IKIM
      write(20) FAKP,FAKABP
      write(20) DUD,FAM,SYS
      write(20) XFM3CF
c          the rest are for ICEPAC & VOACAP only
      write(20) F2D,PERR
      write(20) ANEW,BNEW,ACHI,BCHI,FAKMAP,ABMAP
      write(20) XESMCF,XPMAP
      write(20) XESLCF, XESUCF
      write(20) XERCOF
      close(20)
100   continue
      end
c--------------------------------------------------------------------

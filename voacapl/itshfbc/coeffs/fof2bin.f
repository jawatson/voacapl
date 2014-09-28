      program foF2bin
c          convert foF2CCIR.asc to direct access file:foF2CCIR.da
c          convert foF2URSI.asc to direct access file:foF2URSI.da
c          convert foF2dail.asc to binary        file:foF2dail.bin
c**********************************************************************
      dimension XF2COF(13,76,2)
      character coeff(2)*4,title*80
      character (len=128) :: filein, fileout = ''
      data coeff/'CCIR','URSI'/
c**********************************************************************
c**********************************************************************
c     Modified to accept an (optional) argument specifying 
c     the directory containing the .asc files.
      character (len=128) :: working_dir=''
      if (COMMAND_ARGUMENT_COUNT() == 1) then
         call GET_COMMAND_ARGUMENT(1, working_dir)
         working_dir = trim(working_dir)//"/"
      endif
c**********************************************************************
      do 200 icoeff=1,2
      filein=trim(working_dir)//'fof2'//coeff(icoeff)//'.asc'
      fileout=trim(working_dir)//'fof2'//coeff(icoeff)//'.da'
      open(19,file=filein,status='old')
      rewind(19)
      read(19,'(a)') title
      open(20,file=fileout,form='unformatted',access='direct',recl=7904)
      write(*,101) trim(fileout),trim(filein),trim(title)
101   format(' Creating:',a,' from:',a,' title=',a)
      do 100 month=1,12
      read(19,1) mon
1     format(6x,i2)
      if(mon.ne.month) stop 'month record does not match'
      read(19,3) XF2COF
3     format(6e15.8)
      write(20,rec=month) XF2COF
100   continue
      close(20)
200   close(19)
c********************************************************************
c*****now create the daily foF2 binary coefficient file
      write(*,'('' Creating:fof2daly.bin  from:fof2daly.asc'')')
      OPEN (21,FILE=trim(working_dir)//"fof2daly.asc",STATUS='OLD')
      REWIND(21)
      OPEN (22,FILE=trim(working_dir)//"fof2daly.bin",form='unformatted')
      REWIND(22)
      read(21,'(a)') title
      write(*,'(1x,a)') title(1:70)
      DO 10 KFOF2R=1,9
      read(21,'(a)') title
      write(*,'(1x,a)') title(1:70)
c*****Modified to an unformatted read to avoid eof problems in with 
c*****some compliers.
      READ (21,*) XF2COF
10    write(22) xf2cof
c11    format(6(e12.6,1x))
      close(22)
      close(21)
c********************************************************************
      STOP 'foF2 BINARY COEFFICIENTS created'
      END


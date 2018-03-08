      subroutine batch_S(lu,model,filein,icircuit,*)  ! Point-to-Point BATCH
c**************************************************************************
c          New special BATCH method that takes a standard IONCAP input
c          deck and runs each circuit separately.
c**************************************************************************
c          lu = file unit open of IONCAP full input deck
c          model = VOACAP or ICEPAC
c          filein = file to create the input data on
c          icircuit = circuit number to be processed
c          * = alternate return when done or if error occurs
c**************************************************************************
      use voacapl_defs
      use crun_directory
      character model*6,filein*(*)
c jw      common /crun_directory/ run_directory
c jw         character run_directory*50
      common /card_deck/ ncards,cards(50)
         character cards*80
      common /Cantenna_deck/ nantennas,antennas(22)
         character antennas*80
      character alf*80

      new=0
      icircuit=icircuit+1
      if(icircuit.ne.1) go to 100  !  process up to the next execute card
c          read up to the 1st execute card
      ncards=0
      nantennas=0
10    read(lu,'(a)',end=998) alf
      if(alf(1:1).eq.' ') go to 10     !  skip blank cards
      if(alf(1:10).ne.'ANTENNA   ') go to 20
         nantennas=nantennas+1
         antennas(nantennas)=alf
      go to 10
20    if(alf(1:10).eq.'EXECUTE') go to 200   !  process the deck
      ncards=ncards+1
      cards(ncards)=alf
      if(ncards.lt.49) go to 10
      write(*,'('' Batch input deck contains too many cards'')')
      go to 998
c          read new cards and replace
100   read(lu,'(a)',end=998) alf
      if(alf(1:10).eq.'EXECUTE   ') go to 200     !  EXECUTE
      if(alf(1:10).eq.'QUIT      ') go to 998     !  QUIT
      if(alf(1:1).eq.' ') go to 100               !  skip blank cards
      if(alf(1:10).eq.'ANTENNA   ') then          !  ANTENNA card match test
         if(new.eq.0) then
            nantennas=0
            new=1
         end if
         nantennas=nantennas+1
         antennas(nantennas)=alf
         go to 100
      end if
      do 110 i=1,ncards
      if(cards(i)(1:10).eq.alf(1:10)) go to 150   !  Match
110   continue
      go to 100         !  match not found
150   cards(i)=alf      !  substitute new card
      go to 100
c**************************************************************
c          save deck and process
200   nch_run=lenchar(run_directory)
      open(31,file=run_directory(1:nch_run)//PATH_SEPARATOR//model//'x.dat')
      rewind(31)
      do 210 i=1,ncards
      nch=lenchar(cards(i))
210   write(31,'(a)') cards(i)(1:nch)
      do 220 i=1,nantennas
      nch=lenchar(antennas(i))
220   write(31,'(a)') antennas(i)(1:nch)
      write(31,221)
221   format('EXECUTE',/,'QUIT')
      close(31)
      write(*,'('' Processing #'',i5)') icircuit
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

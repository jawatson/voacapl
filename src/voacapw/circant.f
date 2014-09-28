      subroutine circant(lu,nant,antenna)
      use error_codes
      character antenna(5)*63
c***********************************************************
      nant=0
10    nant=nant+1
      if(nant.gt.5) go to 910
      read(lu,'(a)') antenna(nant)
      read(antenna(nant),'(5x,i5)',err=900) maxfreq
      if(maxfreq.ne.30) go to 10
      return
c***********************************************************
900   write(*,901)
901   format(' Error READing CIRCUITS.??? data file.',/
     +       ' Maxfreq does not = 30.',/
     +       ' Correct problem and try again.')
      write(*,902) nant,antenna(nant)
902   format(' nant=',i2,'  antenna=',a)
      call exit(EC_EXEC_ERROR)
910   write(*,911)
911   format(' Error READing CIRCUITS.??? data file.',/
     +       ' More than 5 antennas defined for 1 circuit.',/
     +       ' Correct problem and try again.')
      call exit(EC_EXEC_ERROR)
      end
c------------------------------------------------------------

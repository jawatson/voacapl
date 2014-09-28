
      subroutine TxPwr(P,alf,nch)
      character alf*10,alft*10
      if(P.ge.10.) write(alf,'(f8.2,2hkW)') P
      if(P.ge.1. .and. P.lt.10.) write(alf,'(f8.4,2hkW)') P
      if(P.lt.1.) write(alf,'(f8.4,1hW)') P*1000.
c          remode leading blanks
      nch=10
      j=0
      do 10 i=1,nch
      if(alf(i:i).eq.' ') go to 10
      j=j+1
      alf(j:j)=alf(i:i)
 10   continue
      nch=j
      if(nch.lt.10) alf(nch+1:10)=' '
c          remode trailing 0s
15    alft=alf
      if(alf(nch-1:nch).eq.'0W') alf(nch-1:nch)='W '
      if(alf(nch-2:nch).eq.'0kW') alf(nch-2:nch)='kW '
      if(alft.eq.alf) go to 20
      nch=nch-1
      go to 15
20    if(alf(nch-1:nch).eq.'.W') then
         alf(nch-1:nch)='W '
         nch=nch-1
      else if(alf(nch-2:nch).eq.'.kW') then
         alf(nch-2:nch)='kW '
         nch=nch-1
      end if
      if(alf(1:1).eq.'W') then     !  Power=0 should really not occur
         alf='0W'
         nch=2
      end if
      return
      end

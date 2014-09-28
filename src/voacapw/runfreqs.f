      subroutine runfreqs(ifr,frel)
      dimension frel(11)
      dimension if1(3),if2(3)
      data if1/ 2,11,21/
      data if2/10,20,30/
      data mode/1/
      do 5 i=1,11
5     frel(i)=0.
      i=0
      if(mode.eq.2) go to 100
      do 10 if=if1(ifr),if2(ifr)
      i=i+1
10    frel(i)=if
      return
100   if(ifr.eq.1) then
         jf1=2
         jf2=20
      else
         jf1=22
         jf2=30
      end if
      do 110 if=jf1,jf2,2
      i=i+1
110   frel(i)=if
      end

c# pwrdb.for
      function pwrdb(freq)
c          return the power in dBw for the transmitter at frequency
      common /cantenna/ numants,iats( 5),anttype( 6),antname( 6),
     +                  xfqs( 5),xfqe( 5),designfreq( 5),antfile( 5),
     +                  beammain( 5),offazim( 5),cond( 5),diel( 5),
     +                  array(30,91,12),aeff(30, 5)
      character anttype*10,antname*70,antfile*24
      common /pantenna/ pwrkw(5),pwrdba(5)

      pwrdb=0.
      f=freq
      if(f.lt.2.) f=2.
      if(f.gt.30.) f=30.
      do 50 i=1,numants
      if(iats(i).ne.1) go to 50     !  not transmitter
      if(xfqs(i).le.f .and. f.le.xfqe(i)) then
         pwrdb=pwrdba(i)-30.
         return
      end if
50    continue
      return
      end

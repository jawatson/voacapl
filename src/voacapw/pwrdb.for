c# pwrdb.for
      function pwrdb(freq)
c          return the power in dBw for the transmitter at frequency
      common /cantenna/ numants,iats(20),anttype(20),antname(20),
     +                  xfqs(20),xfqe(20),designfreq(20),antfile(20),
     +                  beammain(20),offazim(20),cond(20),diel(20),
     +                  array(30,91,20),aeff(30,20)
      character anttype*10,antname*70,antfile*24
      common /pantenna/ pwrkw(20),pwrdba(20)

      pwrdb=30.
      f=freq
      if(f.lt.2.) f=2.
      if(f.gt.30.) f=30.
      do 50 i=1,numants
      if(iats(i).ne.1) go to 50     !  not transmitter
      if(xfqs(i).le.f .and. f.le.xfqe(i)) then
         pwrdb=pwrdba(i)
         return
      end if
50    continue
      return
      end

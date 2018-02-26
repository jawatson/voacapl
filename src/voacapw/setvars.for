      SUBROUTINE setvars     !  set variables we have found are undefined
C--------------------------------
      common /cantenna/ numants,iats(20),anttype(20),antname(20),
     +                  xfqs(20),xfqe(20),designfreq(20),antfile(20),
     +                  beammain(20),offazim(20),cond(20),diel(20),
     +                  array(30,91,22),aeff(30,20)
      character anttype*10,antname*70,antfile*24
      COMMON/GEOG/GYZ(5),RAT(5),GMDIP(5),CLCK(5),ABIY(5),ARTIC(5),SIGPAT
     A(5), EPSPAT(5)
      COMMON /RON /CLAT(5), CLONG(5), GLAT(5), RD(5), FI(3,5), YI(3,5),
     1HI(3,5), HPRIM(30,5), HTRUE(30,5), FVERT(30,5),KM,KFX, AFAC(30,5),
     2HTR(50,3), FNSQ(50,3)
      COMMON /RAYS/ ANG(40), IFOB(40,30,5), NANG
      COMMON/INFORM/INFO,IHSHR,IHLNG
      COMMON / ZON / ABPS(7), CREL(7), EFF(7), FLDST(7), GRLOS(7),
     1 HN(7), HP(7), PROB(7), RELY(7), RGAIN(7), SIGPOW(7), SN(7),
     2 SPRO(7), TGAIN(7), TIMED(7), TLOSS(7), B(7), FSLOS(7), ADV(7),
     3 OBF(7),NMODE(7),TLLOW(7),TLHGH(7)
c------------------------------------------------------------------------
      zero=0.
      numants=0
      do 10 i=1,5
10    ARTIC(i)=zero
      do 20 j=1,5
      do 20 i=1,30
      HTRUE(i,j)=zero
      FVERT(i,j)=zero
      AFAC(i,j)=zero
20    HPRIM(i,j)=zero
      do 30 k=1,5
      do 30 j=1,30
      do 30 i=1,40
30    IFOB(i,j,k)=zero
      IHSHR=0
      IHLNG=0
      do 40 i=1,7
      HP(i)=zero
      rely(i)=zero
      hn(i)=zero
      nmode(i)=0
      sn(i)=zero
      fldst(i)=zero
      sigpow(i)=zero
      b(i)=zero
      timed(i)=zero
      abps(i)=zero
      prob(i)=zero
      rgain(i)=zero
      tgain(i)=zero
      fslos(i)=zero
      spro(i)=zero
      EFF(i)=zero
      grlos(i)=zero
      adv(i)=zero
      obf(i)=zero
40    CREL(i)=zero
      return
      end
C--------------------------------

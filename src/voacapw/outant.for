C------------------------
c# outant.f
      SUBROUTINE OUTANT
C--------------------------------
C
C     THIS ROUTINE OUTPUTS THE ANTENNA PATTERNS GENERATED IN GETANT
C     IF METHOD IS 13,14 OR 15 AND CREATES AN ANTENNA FILE IF USER
C     REQUEST WAS MADE VIA AN "ANTOUT" CONTROL CARD.
C
      common /cmodel/model
      character model*8
      common /cantenna/ numants,iats(20),anttype(20),antname(20),
     +                  xfqs(20),xfqe(20),designfreq(20),antfile(20),
     +                  beammain(20),offazim(20),cond(20),diel(20),
     +                  array(30,91,20),aeff(30,20)
      character anttype*10,antname*70,antfile*24
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
      common /CVERSN/ VERSN
         character VERSN*8
      COMMON / OUTPRT / LINBOT(26), LINBD(14), LINTOP(15), LINTP(14),
     A GRPTYP, JOUT, LINBYP, LINES, LINMAX, LINTYP, LPAGES, NLINE
      dimension iff(21)
      CHARACTER ITL*46,xmtrecv(2)*11
      DATA ITL/'          ELEVATION ANGLE IN DEGREES'/
      data xmtrecv/'TRANSMITTER','RECEIVER'/
c          METHOD=13 = Transmitter output only (ITR=1)
c          METHOD=14 = Receiver    output only (ITR=2)
c          METHOD=15 = Both Transmitter (ITR=1) & Receiver (ITR=2) output
      itr1=1
      itr2=2
      if(method.eq.13) itr2=1
      if(method.eq.14) itr1=2
      do 200 itr=itr1,itr2

C.... PRINT THE PATTERN
      do 150 ian=1,numants
      if(iats(ian).ne.itr) go to 150
      
      LPAGES = LPAGES + 1
      WRITE(LUO,500) METHOD, model,VERSN, LPAGES
  500 FORMAT(1H,32X,'METHOD',I3,1X,a8,1x,a8,2X,'PAGE',I4,/)
      WRITE(LUO,502) anttype(ian),xmtrecv(itr),antfile(ian),antname(ian)
  502 FORMAT(1x,a10,' ANTENNA PACKAGE',26X,'ANTENNA PATTERN',10x,a,/
     + 2h [,a21,2h] ,a70,/
     A ' Frequency Range  ',
     B 'Design Freq  Bearing   Off Azim  Conduct.  Dielect.')
      WRITE(LUO,504) xfqs(ian),xfqe(ian),designfreq(ian),
     +               beammain(ian),offazim(ian),
     +               cond(ian),diel(ian)
  504 FORMAT(1X,F5.1,' to ',F5.1,2X,f10.3,2f10.1,2f10.3)
      if1=xfqs(ian)
      if2=xfqe(ian)
      if(if2-if1.le.20) then
         nf=if2-if1+1
         do 10 if=1,nf
10       iff(if)=if1 + if-1
      else
         nf=21
         do 20 if=1,13
20       iff(if)=if+1
         do 25 if=14,21
25       iff(if)=16 + (if-14)*2
      end if
      WRITE(LUO,506) (iff(if),if=1,nf)
      DO 140 K =1,91,2
      L = 92-K
      M = L-1
      N = (K+1)/2
      WRITE(LUO,508) ITL(N:N),M,(ARRAY(iff(i),L,ian),I=1,nf)
  140 CONTINUE
      WRITE(LUO,506) (iff(if),if=1,nf)
      WRITE(LUO,510)
      WRITE(LUO,512) (AEFF(iff(i),ian),I=1,nf)
      WRITE(LUO,506) (iff(if),if=1,nf)
      WRITE(LUO,510)
150   CONTINUE
200   CONTINUE
      RETURN
  506 FORMAT(4X,21(4X,I2) )
  508 FORMAT(1X,A1,1X,I2,21F6.1)
  510 FORMAT(/,/,48X,'FREQUENCY IN MEGAHERTZ')
  512 FORMAT(/,/,48X,'ANTENNA EFFICIENCY',/,' ',4X,21F6.1)
      END
C--------------------------------

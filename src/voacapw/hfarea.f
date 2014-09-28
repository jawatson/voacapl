c# hfarea.f
      SUBROUTINE hfarea(*)
      use verbose_mod
      common /Careach/ areach      !  =I=inverse area
         character areach*1
      common /cantenna/ numants,iats(20),anttype(20),antname(20),
     +                  xfqs(20),xfqe(20),designfreq(20),antfile(20),
     +                  beammain(20),offazim(20),cond(20),diel(20),
     +                  array(30,91,22),aeff(30,20)
      character anttype*10,antname*70,antfile*24
C--------------------------------
      common /Cround/ iround
C
C     THIS IS THE PROGRAM CONTROL SUBROUTINE
C     ITRUN CONTROLS PROGRAM TASK, ITOUT CONTROLS PROGRAM OUTPUT.
C
      COMMON / ALPHA / IMON(12), ITRAN(2), IRCVR(2), LBMAP(2), MODE(13),
     A MODER(13), ITLAT, ITLONG, IRLAT, IRLONG, IRLATU, IRLONGU, NYEAR
      CHARACTER IMON*3, NYEAR*5, ITRAN*10, IRCVR*10, LBMAP*10, ITLAT*1,
     A ITLONG*1, IRLAT*1, IRLONG*1, IRLATU*1, IRLONGU*1, MODE*2, MODER*2
      COMMON / CONTRL / IELECT(3), KTOUT(12), MONTHS(12), SUNSP(12),
     A IANTOU, ICARD, INTEG, IRED, ISOUT, ISPROC, ISSN, ITYPE, JDASH,
     B JFREQ, JLONG, KCARD, KRUN, MAPIN, MAXNAM, MONOLD, MOREM, MORES,
     C NUMNAM, NUPROC, MAXMET, MSPEC, M100
      COMMON / FILES / LUI, LUO, LU2, LU5, LU6, LU15, LU16, LU20, LU25,
     A LU26, LU35
      COMMON/LPATH/ GCDLNG,TXRGML(45,2),DELOPT,GMIN,YMIN,LTXRGM(2)
      COMMON / TIME / IT, GMT, UTIME(24), GMTR, XLMT(24), ITIM, JTX
      COMMON /DON /ALATD, AMIN, AMIND, BTR, BTRD, DLONG, DMP, ERTR, GCD,
     1 GCDKM, PMP, PWR, TLAT, TLATD, TLONG, TLONGD, RSN, SIGTR, RLAT,
     2 RLATD,RLONG,RLONGD,BRTD,FLUX,ULAT,ULATD,ULONG,ULONGD,SSN,D90R,
     3 D50R,D10R,D90S,D50S,D10S
      COMMON/FRQ/FREA(13),FREL(29),FREQ,JMODE,ITXRCP(2)
      COMMON / ION / IHRE, IHRO, IHRS, LUFP, METHOD, NOISE, NPSL
      COMMON / METSET / ITRUN, ITOUT, JTRUN(40), JTOUT(40)
      COMMON/MUFS/EMUF(24),F1MUF(24),F2MUF(24),ESMUF(24),ALLMUF(24)
     1,FOT(24),XLUF(24),HPF(24),ANGMUF(24),MODMUF,SIGL(4),SIGU(4),DELMU
     2F(4),HPMUF(4),HTMUF(4),FVMUF(4),AFMUF(4),NHOPMF(4),YFOT(4),YHPF(4)
     3 ,YMUF(4)
      COMMON /RTANT /XETA, XSIG, XEPS, XND, XNL, XNH, TEX (4), ITANT,
     1 IRANT, RETA, RSIG, REPS, RND, RNL, RNH, REX(4), TEFF, REFF,
     2 KASANT(2), ITSANT, JTSANT
      COMMON / SSP / SUN(2,12), MONTH
      COMMON/TON/ADJ,ADS,GNOS,GOT,REL,SL,SLS,SPR,SU,SUS
     A ,XNOISE,ZNOISE,NF
*D     COMMON/INDICEZ/ISP,ISZ,IFQN,IUR,NMS(11,24,4,2)
*D     common/lpalmcmp/imddl(45,2,11,24,4,2),igdfx(45,2,11,24,4,2),
*D    +itlos(45,2,11,24,4,2),ihpfx(45,2,11,24,4,2),lNANG(2,11,24,4,2)
*D     integer*2 imddl,igdfx,itlos,ihpfx,lNANG
      COMMON /RGRID/ IPROJ,PLAT,PLON,XMIN,XMAX,YYMIN,YMAX,NX,NY
      common /crun_directory/ run_directory
      character run_directory*50
      common /cdaily/ idaily(12)
      common /Cday/ iday
      character alf*20
      logical*1 doesit
      logical*4 fexists
      integer*4 error_code4
      integer :: error_code = 0
      data D2R/.01745329/
c------------------------------------------------------------------------
C.....START OF PROGRAM
C.....SET THE LOGICAL UNITS FOR SYSTEM INPUT AND OUTPUT FILES.
C     THE USER MUST INCLUDE AN "AUXIN" AND/OR AN "AUXOUT" CARD IF THE
C.....AUXILIARY INPUT AND/OR OUTPUT FILES ARE TO BE USED.
      LUI = LU5
      LUO = LU6
c***********************************************************************
      nch_run=lcount(run_directory,50)
      MONTH=MONTHS(1)
      SSN=SUNSP(1)
      call REDMAP(SSN,MONTH)
      iday=idaily(1)            !  day of the month (if used)
      if(iday.ne.0) call redaily(ssn,month,iday)
      nf=1
      do 5 nf=1,11
      if(frel(nf).eq.0.) go to 6
5     continue
6     nf=nf-1
      call outarea(-nx,-ny)                !  output headers
      meth=METHOD
      if(MSPEC.eq.121) meth=30             !  smoothing method
      if(nf.eq.1) then
         if(iquiet.eq.0) 
     +   write(*,21) meth,IMON(MONTH),IHRO,nint(SSN),frel(1)
21       format(20x,' Method',i3,1x,a3,i3.2,'ut',i4,'ssn  Freqs=',f6.2)
      else
         if(iquiet.eq.0) 
     +   write(*,22) meth,IMON(MONTH),IHRO,nint(SSN),nf
22       format(20x,' Method',i3,1x,a3,i3.2,'ut',i4,'ssn  max of',
     +          i3,' Freqs')
      end if
      tlat=tlatd*d2r
      tlong=tlongd*d2r
      rlat=rlatd*d2r
      rlong=rlongd*d2r
      if(areach.eq.'I') then            !  inverse area coverage
         rlat=tlat
         rlong=tlong
         rlatd=tlatd
         rlongd=tlongd
         if(iquiet.eq.0) write(*,'(''Area Inverse'')')
      end if
ccc      write(72,'(''Tx, Rx='',4f12.4)') tlatd,tlongd,rlatd,rlongd
c*****************************************************************
      DO 900 IY=1,NY
      if(iquiet.eq.0) then
         if(iy.eq.1) then
             write(unit=*,fmt='(i3'' rows '')') ny !jw
             write(unit=*,fmt='(''[''i4)', advance='no') iy !jw
         else
             write(unit=*,fmt='(i4)', advance='no') iy !jw
         end if

         if(mod(iy,15).eq.0) then !jw
            write(*,*) ' '            !  cause a <new line>
            write(unit=*,fmt='(A)', advance='no') ' '
         end if
      end if
c          check to see if we should abort processing
c      inquire(file="/voaarea.abt",exist=doesit) !jw
c      if(doesit) then      !  file exists, abort area calculations
c         call unlink(run_directory(1:nch_run)//'/voaarea.abt',
c     +                  error_code)   !  delete file first
c         return 1
c      end if
c      if(icancel_batch.ne.0) return     !  other cancel button picked
      DO 900 IX=1,NX
      if(areach.ne.'I') then            !  normal area coverage
         CALL GRIDXY(IX,IY,RLONGD,RLATD)      !  GET LON/LAT of RECEIVER
ccc         if(iround.ne.0) then
ccc            rlongd=roundit(rlongd)   !  round to nearest .01 deg
ccc            rlatd=roundit(rlatd)    !  round to nearest .01 deg
ccc         end if
         if(abs(RLATD-TLATD).lt..05 .and. 
     +      abs(RLONGD-TLONGD).le..05) then
            RLONGD=TLONGD+.05
            if(RLONGD.ge.360.) RLONGD=RLONGD-360.
         end if
         if(abs(RLATD).gt.89.9) RLONGD=0.   !  at poles, force longitude=0
         RLONG=RLONGD*D2R
         RLAT=RLATD*D2R
      else                              !  Inverse Area Coverage
         CALL GRIDXY(IX,IY,TLONGD,TLATD)      !  GET LON/LAT of RECEIVER
         if(abs(TLATD-RLAT/D2R).lt..05 .and. 
     +      abs(TLONGD-RLONG/D2R).le..05) then
            TLONGD=RLONG/D2R+.05
            if(TLONGD.ge.360.) TLONGD=TLONGD-360.
         end if
         if(abs(TLATD).gt.89.9) TLONGD=0.   !  at poles, force longitude=0
         TLONG=TLONGD*D2R
         TLAT=TLATD*D2R
         call dazel0(tlatd,tlongd,rlatd,rlongd,ztaz,dist)
         beammain(1)=ztaz       !  set main beam antenna direction
      end if
      do 10 i=1,24
10    allmuf(i)=-1.
ccc      NPSL=0            !  shortest path  (get NPSL from CIRCUIT card)
ccc      NPSL=1            !  thru antipode
ccc      write( *,711) ix,iy,tlong/d2r,tlat/d2r,rlong/d2r,rlat/d2r
ccc      write(72,711) ix,iy,tlong/d2r,tlat/d2r,rlong/d2r,rlat/d2r
ccc711   format(2i5,4f12.4)
C==================================================================
      CALL GEOM
      CALL SANG         !  reduce angle scan for this distance
C.....SET OUTPUT ARRAYS
      CALL SETOUT
C.....SET CONTROL PARAMETER FOR FREQUENCY COMPLEMENTS
C.....IFREQ is never used..
C     IFREQ = 1
C.....BEGIN HOUR LOOP
      JT = IHRO
      IUR=jt
      JTX = JT
      CALL GEOTIM(JT)   !  convert UT to LMT, etc
C.....WANT TO USE LONG TERM COEFFICIENTS
C.....TIME VARIATION OF MAPS
      CALL VIRTIM       !  UT variation of critical frequencies, F2M(3000)
      CALL TIMVAR       !  LMT dependency and E and F1 parameters
      CALL F2VAR        !  F2 parameters (F1 may be changed)
      CALL ESIND        !  Sporatic E parameters
      CALL IONSET
      CALL CURMUF       !  MUF by full ionosphere, and FOT calculation
      FREL(12) = ALLMUF(IT)
      IF(METHOD.EQ.22.OR.METHOD.EQ.25.OR.MSPEC.EQ.125)THEN
C.......FORCE SHORT PATH MODEL
        IPFG=100
      ELSE IF(METHOD.EQ.21)THEN
        IF(MSPEC.EQ.121)THEN
C.........SET FOR SHORT PATH AND USE LONGPATH/SHORTPATH SMOOTHING
          IPFG=100
        ELSE
C.........FORCE LONG PATH MODEL
          IPFG=200
        ENDIF
      ELSE
C.......FORCE LONG PATH MODEL IF PATH LENGTH .GT. SPECIFIED LIMIT
        IPFG=100
        IF(MSPEC.ne.121 .and. GCDKM.GE.GCDLNG)IPFG=200
      ENDIF
ccc      write(luo,111) ix,iy,method,npsl,ipfg,gcdkm,frel(12)
ccc111   format(5i5,f10.1,f10.3)
      CALL LUFFY(IPFG)
      CALL SETLUF
      call outarea(ix,iy)
C==================================================================
900   CONTINUE
c      if(iquiet.eq.0) call sou@(']')
      if(iquiet.eq.0) print *, ']' !jw
C******************************************************************
C.....END OF RUN
      RETURN
      END
C--------------------------------
      function roundit(x)    !  round to nearest .01 deg
      ix=abs(x)*100. +.5
      roundit=float(ix)/100.
      if(x.lt.0.) roundit=-roundit
      return
      end
C--------------------------------

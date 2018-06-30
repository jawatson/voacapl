      function p1240(ssn,month,gmt,plat,plong)
c          This is Recommendation ITU-R P.1240 Table 2
c          Ratio of F1 of FOT to operational MUF when determined by an F2-mode
      use voacapl_defs
      use crun_directory
      COMMON / CON / D2R, DCL, GAMA, PI, PI2, PIO2, R2D, RZ, VOFL
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      common /Cp1240/ ionce_p1240,table2(6,8,3,3)  !  (time,latitude,season,ssn)
c      common /crun_directory/ run_directory
c         character run_directory*50
      dimension kseason(12)
      data kseason/1,1,2,2,3,3,3,3,2,2,1,1/
      data lu/71/

      if(ionce_p1240.eq.2) then      !  table does not exist
         p1240=.85                   !  use old way
         return
      else if(ionce_p1240.ne.1) then !  read table in once
         ionce_p1240=1
         nch_run=lenchar(run_directory)
         open(lu,file=trim(root_directory)//PATH_SEPARATOR//'database'//PATH_SEPARATOR//'p1240.dat',status='old',err=900)
         rewind(lu)
         read(lu,1) alf     !  skip record
1        format(a)
         read(lu,1) alf     !  skip record
         do 40 issn=1,3
         read(lu,1) alf     !  skip record
         read(lu,1) alf     !  skip record
         read(lu,1) alf     !  skip record
         do 30 iseason=1,3
         do 20 lat=1,8
20       read(lu,21) (table2(it,lat,iseason,issn),it=1,6)
21       format(8x,6f8.2)
         read(lu,1) alf     !  skip record
30       continue
40       continue
         close(lu)

      end if

      call get_mid_point(elat,elong)   !  get path midpoint
      tl=amod(gmt+elong/15.,24.)     !  local time
      if(tl.lt.0.) tl=tl+24.
c          SunSpot block
      if(ssn.lt.50.) then
         issn=1
      else if(ssn.gt.100.) then
         issn=3
      else
         issn=2
      end if
c          Local time block
      it=(tl+2.)/4. + 1.
      if(it.gt.6) it=1
c          Season block
      iseason=kseason(month)
      if(elat.lt.0.) iseason=4-iseason     !  flip Winter-Summer
c          Latitude block
      alat=abs(elat)
      lat=(alat-5.)/10.
      if(lat.gt.7) lat=7
      lat=8-lat


      p1240=table2(it,lat,iseason,issn)
ccc      write(luo,11) ssn,month,gmt,elat,elong,tl
ccc11    format('p1240=',f7.0,i5,f8.2,2f10.4,f8.2)
ccc      write(luo,12) it,lat,iseason,issn,p1240
ccc12    format(10x,4i3,f6.2)
      return
900   ionce_p1240=2           !  flag that table does not exist
      p1240=.85               !  use old way
      return
      end
c----------------------------------------------------------------
      subroutine get_mid_point(xlat_mid,xlon_mid)
      COMMON / FILES / LUI,LUO,LU2,LU5,LU6,LU8,LU16,LU61,LU7,LU15
      COMMON/AZEL/ ZTLAT,ZTLON,ZTHT,ZRLAT,ZRLON,ZRHT,ZTAZ,ZRAZ,
     * ZTELV,ZRELV,ZD,ZDGC,ZTAKOF,ZRAKOF
      COMMON /DON/AMIN,AMIND,BTR,BTRD,BRTD,GCD,GCDKM,PWR,XPW,APW
     A ,RLAT,RLATD,RLONG,RLONGD,SSN,TLAT,TLATD,TLONG,TLONGD,PLAT,PLONG
     B ,PGLAT,ACAV,ASM,FEAV,GMT,VERSN,XLZ,GCDFTZ,GCDEND,XINTS,XINTL
     C ,FLUX,CY12(5),DMXA(24),DMX0,CYRAD(5)
      character*4 IRLAT,IRLONG,ITLAT,ITLONG,IRCVR,ITRANS
      COMMON /ION/ IRLAT,IRLONG,ITLAT,ITLONG,IRCVR(5),ITRANS(5),IT,ITRUN
     A,IT1,IT2,ITSTEP,KM,IA,METHOD,ICON,NPSL,NYEAR,NPAGO,ICGM
      ztlat=tlatd
      ztlon=tlongd
      if(ztlon.lt.0.) ztlon=ztlon+360.
      zrlat=rlatd
      zrlon=rlongd
      if(zrlon.lt.0.) zrlon=zrlon+360.
      ztht=0.
      zrht=0.

ccc      open(73,file='\itshfbc\win32\rec533w\p1240.out')
ccc      rewind(73)
ccc      write(73,'(''get_mid_point='',4f10.4)') ztlat,ztlon,zrlat,zrlon
ccc      close(73)

      call dazel0(ztlat,ztlon,zrlat,zrlon,ztaz,zdgc)     !  get distance
      if(npsl.eq.0) then         !  short path distance
         zdgc=zdgc/2.
      else                       !  long path distance
         zdgc=gcdkm/2.
         ztaz=ztaz+180.          !  go the other way (long way)
         if(ztaz.ge.360.) ztaz=ztaz-360.
      end if

ccc      open(73,file='\itshfbc\win32\rec533w\p1240.out2')
ccc      rewind(73)
ccc      write(73,'(''before dazel1='',4f10.4)') ztlat,ztlon,ztaz,zdgc
ccc      close(73)

      call dazel1     !  find midpoint
      xlat_mid=zrlat
      xlon_mid=zrlon
ccc      write(luo,1) tlatd,tlongd,rlatd,rlongd,xlat_mid,xlon_mid,zdgc,
ccc     +              gcdkm,ztaz
ccc1     format('get_mid_point=',6f10.4,3f10.2)
      return
      end
c----------------------------------------------------------------

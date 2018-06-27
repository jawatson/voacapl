      subroutine distxy(idistance,ndistance,
     +    tlat,tlon,rlat_dist,rlon_dist,npsl,rlatd,rlongd)
      COMMON /AZEL/ ZTLAT,ZTLON,ZTHT,ZRLAT,ZRLON,ZRHT,ZTAZ,ZRAZ,
     +              ZTELV,ZRELV,ZD,ZDGC,ZTAKOF,ZRAKOF
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
c**********************************************************************
      ZTLAT=tlat
      ZTLON=tlon
      ZRLAT=rlat_dist
      ZRLON=rlon_dist
      if(idistance.eq.1) then       !  Set Rx to Tx
         rlatd=ZRLAT
         rlongd=ZRLON
      else if(idistance.eq.ndistance) then    !  Set Rx to original Rx
         rlatd=ZTLAT
         rlongd=ZTLON
      else                               !  interpolate in between
         call dazel0(ztlat,ztlon,zrlat,zrlon,ztaz,zdgc)    !  distance Tx to Rx
         zdgc=zdgc*float(ndistance-idistance)/float(ndistance-1)
         call dazel1             !  calc new Rx
         rlatd=ZRLAT
         rlongd=ZRLON
      end if
c          fix up RLONG so it is between 0 & 360.
      if(rlongd.lt.0.) rlongd=rlongd+360.
      if(rlongd.gt.360.) rlongd=rlongd-360.
      RETURN
      END
C------------------------------------------------------------------
c      ZTLAT=tlat*R2D
c      ZTLON=tlon*R2D
c      ZRLAT=rlat_dist*R2D
c      ZRLON=rlon_dist*R2D

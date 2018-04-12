      subroutine distxy(idistance,ndistance,
     +    tlat,tlon,rlat_dist,rlon_dist,npsl,rlatd,rlongd)
      use distance_defs
      COMMON /AZEL/ ZTLAT,ZTLON,ZTHT,ZRLAT,ZRLON,ZRHT,ZTAZ,ZRAZ,
     +              ZTELV,ZRELV,ZD,ZDGC,ZTAKOF,ZRAKOF
      COMMON/CON/D2R,DCL,GAMA,PI,PI2,PIO2,R2D,RZ,VOFL
c**********************************************************************
      real, parameter :: RERTH = 6370.0D0
      ! lp fix
      if (long_path) then
        print *, 'resetting...'
        npsl = 1.
      else
        npsl = 0.
      end if
      ! end of lp fix
      ZTLAT=tlat*R2D
      ZTLON=tlon*R2D
      ZRLAT=rlat_dist*R2D
      ZRLON=rlon_dist*R2D
      if(idistance.eq.1) then       !  Set Rx to Tx
         rlatd=ZRLAT
         rlongd=ZRLON
      else if(idistance.eq.ndistance) then    !  Set Rx to original Rx
         rlatd=ZTLAT
         rlongd=ZTLON
         npsl = 0.
      else                         !  interpolate in between
         call dazel(0)             !  distance Tx to Rx
         ! Start of long Path fix
         if (long_path) then ! Long path
             print '(A)', "Applying Long path correction..."
             ztaz = ztaz - 180.
             if(ztaz.lt.0.) ztaz=ztaz+360.
             if(ztaz.gt.360.) ztaz=ztaz-360.
             zdgc = (2. * PI * RERTH) - zdgc
             zdgc=zdgc*float(ndistance-idistance)/float(ndistance-1) ! zdgc GC path length
             if (zdgc .lt. (PI * RERTH)) then
               npsl = 0.
             else
               npsl = 1.
             end if
         else
             zdgc=zdgc*float(ndistance-idistance)/float(ndistance-1) ! zdgc GC path length
         end if
         ! End of long path fix
         print '(A I1 A I2 A F10.3)', 'npsl:', npsl, ', idistance=', idistance, ', zdgc=', zdgc
         call dazel(1)             !  calc new Rx
         rlatd=ZRLAT
         rlongd=ZRLON
      end if
c          fix up RLONG so it is between 0 & 360.
      if(rlongd.lt.0.) rlongd=rlongd+360.
      if(rlongd.gt.360.) rlongd=rlongd-360.
      RETURN
      END
C------------------------------------------------------------------

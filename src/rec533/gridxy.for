c# gridxy.f
      SUBROUTINE GRIDXY(IX,IY,RLONG,RLAT)
      COMMON /AZEL/ ZTLAT,ZTLON,ZTHT,ZRLAT,ZRLON,ZRHT,ZTAZ,ZRAZ,
     +              ZTELV,ZRELV,ZD,ZDGC,ZTAKOF,ZRAKOF
      COMMON /RGRID/ IPROJ,PLAT,PLON,XMIN,XMAX,YMIN,YMAX,NX,NY
      X=XMIN + FLOAT(IX-1)*(XMAX-XMIN)/FLOAT(NX-1)
      Y=YMIN + FLOAT(IY-1)*(YMAX-YMIN)/FLOAT(NY-1)
      IF(IPROJ.EQ.7) GO TO 70              !  GREAT CIRCLE PROJECTION
      RLONG=X                   !  LAT LONG PROJECTION
      RLAT=Y
      go to 100
70    ZTLAT=PLAT                !  CENTER OF GREAT CIRCLE PROJECTION
      ZTLON=PLON
      if(x.ne.0. .or. y.ne.0.) then
         ZTAZ=90.-ATAN2(Y,X)/.0174533         !  AZIMUTH
         IF(ZTAZ.LT.0.) ZTAZ=ZTAZ+360.
         ZDGC=SQRT(X*X+Y*Y)                   !  GREAT CIRCLE DISTANCE
ccc      write(*,1) ztlat,ztlon,ztaz,zdgc,x,y,ix,iy
ccc1     format(' ztlat,ztlon,ztaz,zdgc=',6f10.2,2i5)
         ZTHT=0.           !  height of transmitter
         ZRHT=0.           !  height of receiver
         CALL DAZEL1
         RLONG=ZRLON
         RLAT=ZRLAT
      else
ccc      write(*,'("Center of projection  ix,iy=",2i5)') ix,iy
	 rlat=plat                         !  point is center of projection
	 rlong=plon
      end if
c          fix up RLONG so it is between 0 & 360.
100   if(rlong.lt.0.) rlong=rlong+360.
ccc      if(ix.eq.1) WRITE(6,2) Iy,nY,RLONG,RLAT,X,Y,ZTAZ,ZDGC
ccc 2    FORMAT(' GRIDXY=',I5,1h/,I5,2F10.4,2F10.1,F8.1,F8.0)
ccc      if(ix.eq.1) call lineup
      RETURN
      END
C------------------------------------------------------------------

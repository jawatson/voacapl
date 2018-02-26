       subroutine DAZEL0(ZTLAT,ZTLON,ZRLAT,ZRLON,ZTAZ,ZDGC)
C#  SUB DAZEL0             Great circle calculations. (same as DAZEL(0))
       IMPLICIT DOUBLE PRECISION(A-L,N-Y)
C
C     TWO MODES--   0   INPUT LAT AND LON OF END POINT
C                       RETURN DISTANCE AND AZIMUTH TO END PT WITH ELEVATIONS
C                   1   INPUT BEARING (AZIMUTH) OF END POINT
C                       RETURN LAT AND LON OF END POINT WITH ELEVATIONS
C
C   MODE 0
C   INPUT PARAMETERS (THESE DEFINE LOCATION OF POINTS T (TRANSMITTER)
C     AND R (RECEIVER) RELATIVE TO A SPHERICAL EARTH.
C     ZTLAT - LATITUDE (DECIMAL DEGREES NORTH OF EQUATOR) OF POINT T
C     ZTLON - LONGITUDE (DECIMAL DEGREES EAST OF PRIME (GREENWICH)
C            MERIDIAN) OF POINT T
C     ZTHT  - HEIGHT (METERS ABOVE MEAN SEA LEVEL) OF POINT T
C     ZRLAT - LATITUDE (DECIMAL DEGREES NORTH OF EQUATOR) OF POINT R
C     ZRLON - LONGITUDE (DECIMAL DEGREES EAST OF PRIME MERIDIAN OF POINT R
C     ZRHT  - HEIGHT (METERS ABOVE MEAN SEA LEVEL) OF POINT R
C
C   OUTPUT PARAMETERS
C     ZTAZ  - AZUMUTH (DECIMAL DEGREES CLOCKWISE FROM NORTH) AT T OF R
C     ZRAZ  - AZIMUTH (DECIMAL DEGREES CLOCKWISE FROM NORTH) AT R OF T
C     ZTELV - ELEVATION ANGLE (DECIMAL DEGREES ABOVE HORIZONTAL AT T
C            OF STRAIGHT LINE BETWEEN T AND R
C     ZRELV - ELEVATION ANGLE (DECIMAL DEGREES ABOVE HORIZONTAL AT R)
C            OF STRAIGHT LINE BETWEEN T AND R
C     ZTAKOF - TAKE-OFF ANGLE (DECIMAL DEGREES ABOVE HORIZONTAL AT T)
C            OF REFRACTED RAY BETWEEN T AND R (ASSUMED 4/3 EARTH RADIUS)
C     ZRAKOF - TAKE-OFF ANGLE (DECIMAL DEGREES ABOVE HORIZONTAL AT R)
C            OF REFRACTED RAY BETWEEN T AND R (ASSUMED 4/3 EARTH RADIUS)
C     ZD    - STRAIGHT LINE DISTANCE (KILOMETERS) BETWEEN T AND R
C     ZDGC  - GREAT CIRCLE DISTANCE (KILOMETERS) BETWEEN T AND R
C
C   MODE 1
C   INPUT PARAMETERS                    OUTPUT PARAMETERS
C     ZTLAT                                ZRLAT
C     ZTLON                                ZRLON
C     ZTAZ                                 RELEV,ZRAKOF
C     ZDGC                                 TELEV,ZTAKOF
C
C
C     ALL OF THE ABOVE PARAMETERS START WITH THE LETTER Z AND ARE SINGLE
C     PRECISION.  ALL PROGRAM VARIABLES ARE DOUBLE PRECISION.
C     PROGRAM IS UNPREDICTABLE FOR SEPARATIONS LESS THAN 0.00005 DEGREES,
C     ABOUT 5 METERS.
C
C   WRITTEN BY KEN SPIES 5/79
C   REFRACTION AND ST. LINE ELEVATIONS BY EJH
C
c                          RERTH changed (from 6368.) 2/26/93 by Greg hand
      DATA PI/3.141592653589793238462643D0/,RERTH/6370.0D0/
      DATA DTOR/0.01745329252D0/,RTOD/57.29577951D0/
      DATA ZTHT,ZRHT/0.,0./
       TLATS=ZTLAT
       TLONS=ZTLON
       THTS=ZTHT*1.0E-3
       RLATS=ZRLAT
       RLONS=ZRLON
       RHTS=ZRHT*1.0E-3
       IF(TLATS.LE.-90.) TLATS=-89.999
       IF(TLATS.GE. 90.) TLATS=89.999
       IF(RLATS.LE.-90.) RLATS=-89.999
       IF(RLATS.GE. 90.) RLATS=89.999
c          bad things happen when points are directly opposite
c          the angle is undefined!!!  (move receiver a little)
       if(abs(abs(tlons-rlons)-180.).le..001 .and.
     +    abs(tlats+rlats).le..002) then
          rlats=rlats+.1
          if(rlats.ge.90.) rlats=89.9
       end if
       DELAT=RLATS-TLATS
       ADLAT=DABS(DELAT)
       DELON=RLONS-TLONS
       IF(DELON-(-180.0))12,16,16
12     DELON=DELON+360.0
       IF(DELON-(-180.0))12,20,20
16     IF(DELON-180.0)20,20,18
18     DELON=DELON-360.0
       IF(DELON-180.0)20,20,18
20     ADLON=DABS(DELON)
       DELHT=RHTS-THTS
       IF(ADLON-1.0E-5)22,22,55
22     IF(ADLAT-1.0E-5)24,24,40
C
C   POINTS T AND R HAVE THE SAME COORDINATES
C
24     ZTAZ=0.0
       ZRAZ=0.0
       IF(DELHT)25,30,35
25     ZTELV=-90.0
       ZRELV=90.0
       ZD=-DELHT
      ZDGC=0.0
       RETURN
30     ZTELV=0.0
       ZRELV=0.0
       ZD=0.0
      ZDGC=0.0
       RETURN
35     ZTELV=90.0
       ZRELV=-90.0
       ZD=DELHT
      ZDGC=0.0
       RETURN
C
C   POINTS T AND R HAVE SAME LONGITUDE, DISTINCT LATITUDES
C
40     IF(DELAT-0.0)42,42,45
42     ZTAZ=180.0
       ZRAZ=0.0
       GO TO 50
45     ZTAZ=0.0
       ZRAZ=180.0
50     GC=ADLAT*DTOR
       SGC=DSIN(0.5*GC)
       D=DSQRT(DELHT*DELHT+4.0*(RERTH+THTS)*(RERTH+RHTS)*SGC*SGC)
      ZD=D
       GO TO 140
C
C   POINTS TAND R HAVE DISTINCT LONGITUDES
C
55     IF(DELON-0.0)56,56,60
56     WLAT=RLATS*DTOR
       ELAT=TLATS*DTOR
       GO TO 65
60     WLAT=TLATS*DTOR
       ELAT=RLATS*DTOR
C
C   CALCULATE AZIMUTHS AT POINTS W AND E
C
65     SDLAT=DSIN(0.5*ADLAT*DTOR)
       SDLON=DSIN(0.5*ADLON*DTOR)
       SADLN=DSIN(ADLON*DTOR)
       CWLAT=DCOS(WLAT)
       CELAT=DCOS(ELAT)
       P=2.0*(SDLAT*SDLAT+SDLON*SDLON*CWLAT*CELAT)
       SGC=DSQRT(P*(2.0-P))
       SDLAT=DSIN(ELAT-WLAT)
       CWAZ=(2.0*CELAT*DSIN(WLAT)*SDLON*SDLON+SDLAT)/SGC
      SWAZ=SADLN*CELAT/SGC
      WAZ=DATAN2(SWAZ,CWAZ)*RTOD
       CEAZ=(2.0*CWLAT*DSIN(ELAT)*SDLON*SDLON-SDLAT)/SGC
      SEAZ=SADLN*CWLAT/SGC
      EAZ=DATAN2(SEAZ,CEAZ)*RTOD
110    EAZ=360.0-EAZ
       IF(DELON-0.0)111,111,115
111    ZTAZ=EAZ
       ZRAZ=WAZ
       GO TO 120
115    ZTAZ=WAZ
       ZRAZ=EAZ
C
C
C   COMPUTE THE STRAIGHT LINE DISTANCE AND GREAT CIRCLE ANGLE BETWEEN T AND R
C
120    D=DSQRT(DELHT*DELHT+2.0*(RERTH+THTS)*(RERTH+RHTS)*P)
      ZD=D
       CGC=1.0-P
      GC=DATAN2(SGC,CGC)
C
C   COMPUTE GREAT CIRCLE DISTANCE AND ELEVATION ANGLES
C
140   ZDGC=GC*RERTH
       RETURN
      END
C------------------------------------------------------------------

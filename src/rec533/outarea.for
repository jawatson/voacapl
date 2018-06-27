c# outarea.f
      SUBROUTINE OUTarea(luo,ix,iy,rlatd,rlongd,xmuf,mode,iangle,
     +                   xdbu,sna,psn,snxx)
C--------------------------------
C
C     THIS ROUTINE OUTPUTS all variables for area coverage contour plotting
C
      COMMON /RGRID/ IPROJ,PLAT,PLON,XMIN,XMAX,YMIN,YMAX,NX,NY
      character mode*6,alfs(7)*6

      xdbu1=xdbu
      if(xdbu1.lt.-999.) xdbu1=-999.
      sna1=sna
      if(sna1.lt.-999.) sna1=-999.
      snxx1=snxx
      if(snxx1.lt.-999.) snxx1=-999.
      write(alfs(1),'(f6.2)') xmuf                !  MUF
      alfs(2)=mode
      write(alfs(3),'(f6.1)') float(iangle)       !  ANGLE
      write(alfs(4),'(f6.1)') xdbu1               !  DBU
      write(alfs(5),'(f6.1)') sna1                !  SNR
      write(alfs(6),'(f6.3)') psn                 !  FS/N
      write(alfs(7),'(f6.1)') snxx1               !  SNxx

      xlongd=rlongd
      if(iproj.ne.7) then    !  Lat/Lon projection, see if Longitude needs adjustment
         if(xmin.lt.0.) then
            if(ix.eq.1 .or. xlongd.gt.180.) xlongd=xlongd-360.
         end if
      end if
      if(xlongd.lt.-359.) xlongd=0.   !  probably North or south pole caused problem
      write(luo,'(2i3,2f10.4,7a6)') ix,iy,rlatd,xlongd,alfs
      return
      END
C--------------------------------

!------------------------------------------------------
subroutine ant99_calc(freq,azimuth,elev,gain,efficiency,*)
!          external antenna calculations:
!          If outside frequency range, gain=-99.9 returned.
!          If ELEVATION angle or AZIMUTH angle in error, return 1
    use Cant99 

    gain=-99.9
    efficiency=0.
    if(freq.lt.frequency(1) .or. freq.gt.frequency(nfreq)) go to 900 ! out of freq range
    if(elev.lt.0. .or. elev.gt.90.) go to 910 ! out of elevation angle range
    azim=azimuth
    if(azim.lt.0.) azim=azim+360.
    if(azim.ge.360.) azim=azim-360.
    if(azim.lt.0. .or. azim.ge.360.) go to 920   !  out of azimuth angle range
    do i=1,nfreq
        if(abs(freq-frequency(i)).lt..001) then   !  frequency match
	        gain=ant99_gain(i,azim,elev,luaa,ifreq1,gain1)
	        efficiency=eff(i)
            return
        end if
    end do
!   interpolate on frequency
    do i=2,nfreq
        if(freq.le.frequency(i)) then       !  interpolate between (i-1) and (i)
	        g1=ant99_gain(i-1,azim,elev,luaa,ifreq1,gain1)
	        eff1=eff(i-1)
	        g2=ant99_gain(i  ,azim,elev,luaa,ifreq2,gain2)
	        eff2=eff(i)
	        xf=(freq-frequency(i-1))/(frequency(i)-frequency(i-1))
	        efficiency=eff1 + (eff2-eff1)*xf
	        gain=g1 + (g2-g1)*xf
            return
        end if
    end do

    gain=-99.9
    return
!*************************************************************************
900 continue

    return
910 write(*,911) elev
911 format(' Elevation angle=',f10.3,' is not allowed.',&
        ' MUST be in the range [0 to 90 degrees].')
    go to 999
920 write(*,921) azimuth
921 format(' Azimuth angle=',f10.3,' is not allowed.',&
        ' MUST be in the range [-360 to 360 degrees].')
999 return 1
end
!------------------------------------------------------
      
function ant99_gain(ifreq,azimuth,elev,luaa,ifreq1,gain1)
    dimension gain1(91,360)
    data bad/-99998./
    ant99_gain=-99.9
    if(ifreq.ne.ifreq1) read(luaa,rec=ifreq) gain1
    ifreq1=ifreq
    iaz=azimuth
    do ia=iaz,0,-1        !  find lower azimuth
        if(gain1(1,ia+1).lt.bad) go to 10
        low_az=ia
        go to 20
10      continue
    end do
    write(*,'('' Cannot get here in ant99_gain.'')')
20  do ia=iaz+1,359       !  find upper azimuth
        if(gain1(1,ia+1).lt.bad) go to 30
        iup_az=ia
        go to 40
30      continue
    end do
    iup_az=360
40  continue
    iel=elev
    do ie=iel,0,-1        !  find lower elevation
        if(gain1(ie+1,1).lt.bad) go to 50
        low_el=ie
        go to 60
50      continue
    end do
    write(*,'('' Cannot get here in ant99_gain.'')')
60  do ie=iel+1,90        !  find upper elevation
        if(gain1(ie+1,1).lt.bad) go to 70
        iup_el=ie
        go to 80
70      continue
    end do
    iup_el=90
80  continue

    g=ant99_interp(gain1,low_az,azimuth,iup_az,low_el,elev,iup_el)
    ant99_gain=g
    return
end

!------------------------------------------------------
function ant99_interp(z,iy1,y,iy2,ix1,x,ix2)
!          interpolation
    dimension z(91,*)
    jy2=iy2
    if(jy2.eq.360) jy2=0
    z1=z(ix1+1,iy1+1)
    z2=z(ix2+1,iy1+1)
    z3=z(ix1+1,jy2+1)
    z4=z(ix2+1,jy2+1)
    if (ix1.eq.ix2) then ! 90deg elevation
        z12 = z2
        z34 = z4
    else
        xf = (x-float(ix1))/float(ix2-ix1)
        z12 = z1 + (z2-z1)*xf
        z34 = z3 + (z4-z3)*xf
    end if
    zz=z12 + (z34-z12)*(y-float(iy1))/float(iy2-iy1)
    ant99_interp=zz
    return
end
!-------------------------------------------------------
subroutine ant99_read(filename,lu,lua,*)
      use Cant99

      character filename*(*)
      character alf*80
      dimension iazimuth(360)
      data bad/-99999./
      filenam=filename
      nch=len(trim(filename))
      open(lu,file=filename(1:nch),status='old',err=910)
      rewind(lu)
      read(lu,'(a)',end=100) title
      read(lu,*) nparms
      do 10 i=1,nparms
10    read(lu,*) parms(i)
      itype=nint(parms(2))

      ifreq1=0
      ifreq2=0
      nfreq=0
      luaa=lua               !  save scratch file lu
      open(lua,status='scratch',access='direct',recl=360*91*4)
20    read(lu,'(a)',end=100) alf
      if(alf(1:1).eq.'#') then                   !  comment record
      else if(alf(1:9).eq.'frequency') then
	 nfreq=nfreq+1
	 read(alf(10:),*) frequency(nfreq)

21       format(' Reading frequency #',i3,'=',f8.3,' MHz')
	 dbi(nfreq)=0.
	 eff(nfreq)=0.
	 do 30 iaz=1,360
	 iazimuth(iaz)=0
	 do 30 iel=1,91
30       gain1(iel,iaz)=bad         !  dummy value
      else if(alf(1:9).eq.'normalize') then
	 read(alf(10:),*) dbi(nfreq)
      else if(alf(1:18).eq.'antenna_efficiency') then
	 read(alf(19:),*) eff(nfreq)
!          make sure the gain record is valid
	 do 40 iaz=1,360
	 if(iazimuth(iaz).eq.0 .or. iazimuth(iaz).eq.7) go to 40
	 write(*,51) filename(1:nch)
	 write(*,31) 
31      format(' For any AZIMUTH ANGLE defined, the ELEVATION'&
            ' ANGLES 0 and 90 MUST be defined.')
	 write(*,'('' AZIMUTH ANGLE='',i5,'' in error.'')') iaz
	 go to 900
40       continue
	 write(lua,rec=nfreq,iostat=ios,err=920) gain1
      else if(alf(1:8).eq.'matching') then       !  ignore this record
      else if(alf(1:10).eq.'          ') then    !  comment record
      else                                       !  must be a data record
	 read(alf,*) iaz,iel,g
	 if(iaz.lt.0 .or. iaz.ge.360) then       !  azimuth angle error
	    write(*,51) filename(1:nch)
51          format(' Error READing antenna file=',a)
	    write(*,'('' Data file contains AZIMUTH ANGLE error.'')')
	    write(*,'('' AZIMUTH ANGLE must be [0 to 359].'')')
	    write(*,'('' Value read was='',i8)') iaz
	    go to 900
	 end if
	 if(iel.lt.0 .or. iel.gt.90) then        !  elevation angle error
	    write(*,51) filename(1:nch)
	    write(*,'('' Data file contains ELEVATION ANGLE error.'')')
	    write(*,'('' ELEVATION ANGLE must be [0 to 90].'')')
	    write(*,'('' Value read was='',i8)') iel
	    go to 900
	 end if
	 gain1(iel+1,iaz+1)=g
	 if(iel.eq.0) then
	    iazimuth(iaz+1)=ior(iazimuth(iaz+1),1)
	 else if(iel.eq.90) then
	    iazimuth(iaz+1)=ior(iazimuth(iaz+1),2)
	 else
	    iazimuth(iaz+1)=ior(iazimuth(iaz+1),4)
         end if
      end if
      go to 20
100   close(lu)
      return
!*************************************************************************
900   close(lu)
      return 1
910   write(*,911) filename(1:nch)
911   format('Could not OPEN antenna file=',a)
      return 1
920   write(*,921) ios
921   format('Error READing antenna file=',i5)
      stop
      end
!------------------------------------------------------
      subroutine ant99_close       !  close the scratch unit
      use Cant99
      close(luaa)
      return
      end
!------------------------------------------------------

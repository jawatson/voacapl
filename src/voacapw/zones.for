c-----------------------------------------------------------
      subroutine zones_to_TP(zones,nzones,idx_TP,nTP)
c        convert CIRAF zones to Test Points
      character zones(*)*4,zone*4
      dimension idx_TP(*)
      common /Cciraf/ nciraf_tp,ciraf(911),ciraf_lat(911),ciraf_lon(911)
         character ciraf*4
      nTP=0
      do 100 iz=1,nzones
      zone=zones(iz)
      if(zone(3:4).eq.'  ') then      !  full zone
	 do i=1,nciraf_tp
	 if(zone(1:2).eq.ciraf(i)(1:2)) then        !  add test point
	    nTP=nTP+1
	    idx_TP(nTP)=i
         end if
         end do
      else if(zone(4:4).ne.' ') then      !  XX quadrant
	 do i=1,nciraf_tp
	 if(zone(1:4).eq.ciraf(i)(1:4)) then        !  add test point
	    nTP=nTP+1
	    idx_TP(nTP)=i
         end if
         end do
      else if(zone(3:3).eq.'N' .or. zone(3:3).eq.'S') then  !  N or S  quadrants
	 do i=1,nciraf_tp
	 if(zone(1:3).eq.ciraf(i)(1:3)) then        !  add test point
	    nTP=nTP+1
	    idx_TP(nTP)=i
         end if
         end do
      else if(zone(3:3).eq.'E') then  !  NE or SE  quadrants
	 do i=1,nciraf_tp
	 if(zone(1:2).eq.ciraf(i)(1:2) .and. 
     +      ciraf(i)(4:4).eq.'E') then        !  add test point
	    nTP=nTP+1
	    idx_TP(nTP)=i
         end if
         end do
      else if(zone(3:3).eq.'W') then  !  NW or SW  quadrants
	 do i=1,nciraf_tp
	 if(zone(1:2).eq.ciraf(i)(1:2) .and. 
     +      ciraf(i)(4:4).eq.'W') then        !  add test point
	    nTP=nTP+1
	    idx_TP(nTP)=i
         end if
         end do
      else
	 stop 'Should not get here in zone_to_TP.'
      end if
100   continue
      return
      end
c------------------------------------------------------------
      subroutine parse_zones(cirafz,zones,nzones)
c          parse the CIRAF zone field into its parts
      character cirafz*30,zones(*)*4
      character tempz*31
      nzones=0
      tempz=cirafz
      nchz=lenchar(tempz)
      tempz(nchz+1:nchz+1)=','         !  add terminator
      nchz=nchz+1
      last=1
      do 100 i=2,nchz                 !  find next ','
      if(tempz(i:i).ne.',') go to 100
      call pzones(tempz(last:i-1),i-last,zones,nzones)
      last=i+1
100   continue
      return
      end
c------------------------------------------------------------
      subroutine pzones(alf,nch,zones,nzones)
      character alf*(*),zones(*)*4,ich*1,numb*2,quad*2,nums(2)*2
      dimension n(2)
      if(index(alf(1:nch),'-').gt.0) then            !  process '-'
	 nums(1)='  '
	 nums(2)='  '
	 n(1)=0
	 n(2)=0
	 inum=1
	 do 10 i=1,nch
	 ich=alf(i:i)
	 if(ich.ge.'0' .and. ich.le.'9') then
	    n(inum)=n(inum)+1
	    nn=n(inum)
	    if(nn.gt.2) stop 'CIRAF zone (in -) > 2 digits'
	    nums(inum)(nn:nn)=ich
	 else if(ich.eq.'-') then
	    inum=inum+1
	    if(inum.gt.2) stop 'CIRAF zone had more than 1 -.'
	 else
	    stop 'CIRAF zone (with -) had bad character.'
	 end if
10       continue
	 do 15 i=1,2
	 nn=n(inum)
	 if(nn.eq.1) then
	    ich=nums(i)(1:1)
	    nums(i)=' '//ich
	 else if(nn.eq.0) then
	    stop 'CIRAF zone (with -) had no number'
         end if
15       continue
	 read(nums(1),'(i2)') iz1
	 read(nums(2),'(i2)') iz2
	 if(iz2.le.iz1) then
	    stop 'CIRAF zone (with N-M) has N>M.'
	 end if
	 do 20 iz=iz1,iz2
	 nzones=nzones+1
	 write(zones(nzones),'(i2,2x)') iz
20       continue
      else
	 numb='  '
	 nnumb=0
	 quad='  '
	 nquad=0
	 do 50 i=1,nch
	 ich=alf(i:i)
	 if(ich.ge.'0' .and. ich.le.'9') then
	    nnumb=nnumb+1
	    if(nnumb.gt.2) stop 'CIRAF zone > 2 digits'
	    numb(nnumb:nnumb)=ich
         else
	    nquad=nquad+1
	    if(nquad.gt.2) stop 'CIRAF quadrant > 2 characters'
	    quad(nquad:nquad)=ich
	 end if
50       continue
	 if(nnumb.eq.1) then
	    ich=numb(1:1)
	    numb=' '//ich
	 else if(nnumb.eq.0) then
	    stop 'CIRAF zone had no number'
	 end if
	 nzones=nzones+1
	 zones(nzones)=numb//quad
      end if
      return
      end
c------------------------------------------------------------

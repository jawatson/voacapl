      subroutine getTR(tlat,tlon,rlat,rlon)
      INCLUDE 'ficepac.hdr'
      character cdeg*10

      cdeg=tlatdeg
      nch=10
      call rblankc(cdeg,nch)
      call chardeg(cdeg,0,tlat,ierr)
      cdeg=tlondeg
      nch=10
      call rblankc(cdeg,nch)
      call chardeg(cdeg,1,tlon,ierr)
      cdeg=rlatdeg
      nch=10
      call rblankc(cdeg,nch)
      call chardeg(cdeg,0,rlat,ierr)
      cdeg=rlondeg
      nch=10
      call rblankc(cdeg,nch)
      call chardeg(cdeg,1,rlon,ierr)
      return
      end

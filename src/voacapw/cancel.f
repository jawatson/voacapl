c--------------------------------------------------------------
      subroutine count_batch(lu,num)  !  count # files to process
      character alf*1
      num=0
10    read(lu,'(a)',end=100) alf
      num=num+1
      go to 10
100   rewind(lu)
      return
      END
c--------------------------------------------------------------
c      integer*4 function cancel_batch()
c      common /Ccancel_batch/ icancel_batch
c      icancel_batch=1
c      cancel_batch=1
c      return
c      END
c--------------------------------------------------------------

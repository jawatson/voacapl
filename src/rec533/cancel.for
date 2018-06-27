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
      integer*4 function cancel_batch()
      common /Ccancel_batch/ icancel_batch
      icancel_batch=1
      cancel_batch=1
      return
      END
c--------------------------------------------------------------

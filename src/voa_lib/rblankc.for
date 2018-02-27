      subroutine rblankc(ialf,nch)   !  remove ALL blanks
      character ialf*(*),ich
      j=0
      nchold=nch
      do 10 i=1,nch
      ich=ialf(i:i)
      if(ich.eq.' ') go to 10
      j=j+1
      ialf(j:j)=ich
 10   continue
      nch=j
      if(nch.lt.nchold) ialf(nch+1:nchold)=' '
      return
      end

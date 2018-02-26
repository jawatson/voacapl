      subroutine squeez(alf,maxch)     !  squeeze multiple blanks out
      character alf*(*),tmp*120
      nch=lcount(alf,maxch)

10    idx=index(alf(1:nch),'  ')
      if(idx.eq.0) return
      nn=nch-idx-1
      tmp(1:nn)=alf(idx+2:nch)//' '
      alf(idx+1:nch)=tmp(1:nn)
      nch=nch-1
      go to 10
      end

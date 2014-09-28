c# col_DOS.for
      character*6 function col_DOS(index,icode)  ! convert color index to color
      common /DOS_col/ ncolors,colors(20),index_DOS(20),DOScol(20)
      character colors*6,DOScol*6
      do 10 i=1,ncolors
      if(index.eq.index_DOS(i)) then
         if(icode.eq.0) col_DOS=colors(i)
         if(icode.ne.0) col_DOS=DOScol(i)
         return
      end if
10    continue
      col_DOS='Ignore'       !  Ignore if not found
      return
      end
C------------------------------------------------------------------

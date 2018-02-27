c# icolDOS.for
      function icolDOS(color)    !  convert color name to DOS color index
      character color*6
      common /DOS_col/ ncolors,colors(20),index_DOS(20),DOScol(20)
      character colors*6,DOScol*6
      do 10 i=1,ncolors
      if(color.eq.colors(i) .or. color.eq.DOScol(i)) then
         icolDOS=index_DOS(i)
         return
      end if
10    continue
      icolDOS=-1      !  Ignore if not found
      return
      end
C------------------------------------------------------------------

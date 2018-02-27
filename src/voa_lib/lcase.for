      subroutine lcase(ialf,nch)
c          Convert all UPPER case letters in IALF to lower case
      character ialf*(*)
      do 10 i=1,nch
      ich=ichar(ialf(i:i))
      if(ich.ge.65 .and. ich.le. 90) ialf(i:i)=char(ich+32)
 10   continue
      return
      end

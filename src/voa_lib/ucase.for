      subroutine ucase(ialf,nch)
c          Convert all lower case letters in IALF to UPPER case
      character ialf*(*)
      do 10 i=1,nch
      ich=ichar(ialf(i:i))
      if(ich.ge.97 .and. ich.le.122) ialf(i:i)=char(ich-32)
 10   continue
      return
      end

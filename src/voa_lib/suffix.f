      subroutine suffix(ifile,nchm,isufx,nchs)
c          Append a suffix to the file name
c          Search for 1st blank or '.'
      character *(*) ifile,isufx,ich*1
      do 10 i=1,nchm-nchs
      ich=ifile(i:i)
      if(ich.eq.' ' .or. ich.eq.'.') go to 20
 10   continue
      i=nchm-nchs+1
 20   ifile(i:i+nchs-1)=isufx(1:nchs)
      return
      end


c# antfile.f
      subroutine antfile(dir,file,antname)
c          concatenate dir/file into antname
      character dir*8,file*12,antname*21
      antname='                     '
      n=0
      do 10 i=1,8
      if(dir(i:i).eq.' ') go to 15
      n=n+1
      antname(n:n)=dir(i:i)
10    continue
15    n=n+1
      antname(n:n)='/'
      do 20 i=1,12
      if(file(i:i).eq.' ') go to 25
      n=n+1
      antname(n:n)=file(i:i)
20    continue
25    continue
      return
      end

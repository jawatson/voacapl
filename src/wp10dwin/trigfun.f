        subroutine trigfun (a, b, uust)
c
c  pre-sets arrays of trig values
c  if uust=1, 91 values are set
c  if uust=1/3, 271 values are set    -- WHAT??? This won't work!!!
c
      common/general/d1,e1,f1,p1,q1,s1,beta,rlambda
c      
        dimension a(0:361),b(0:361)
c
        do 100 z=0,90,uust
        a(ifix(z)) = sin(z * q1)
        b(ifix(z)) = cos(z * q1)
  100   continue        
        a(90) = sin(90.05 * q1)
        b(90) = cos(90.105 * q1)
        b(0) = cos(.005)
        return
        end
c---------------------------------------------------------------


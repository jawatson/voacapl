      function asin_gh(s)
      data pi/3.14159265/
      if(abs(s).le.1.) then
         c=sqrt(1.-s*s)
         asin_gh=atan(s/c)
      else
         if(s.lt.0.) then
            asin_gh=-pi/2.
         else
            asin_gh=pi/2.
         end if
      end if
      return
      end
c----------------------------------------------------
      function acos_gh(c)
      data pi/3.14159265/
      if(abs(c).le.1.) then
         s=sqrt(1.-c*c)
         acos_gh=atan(s/c)
      else
         if(c.lt.0.) then
            acos_gh=0.
         else
            acos_gh=-pi
         end if
      end if
      return
      end
c----------------------------------------------------

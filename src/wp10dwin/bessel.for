        subroutine bessel (xx, bj1)
c
c            bessel function calculation subroutine
c
      if (xx.le.3.0) then 
        xx3 = xx/3.0
        aa = .5 - .56249 * xx3**2 + .21093 * xx3**4 - .03954 * xx3**6
        bb = .00443 * xx3**8 - .00031 * xx3**10 + .00001 * xx3**12
        bj1 = xx * (aa + bb)
      else
        x3x = 3.0/xx
        cc = .79788+1.56e-06 * x3x+ .01659 * x3x**2+ .00017 * x3x**3
        dd = -.00249 * x3x**4 + .00113 * x3x**5 - .0002 * x3x**6
        ff = cc + dd
        qq =xx-2.35619+0.12499* x3x+ .00005 * x3x**2- .00637 * x3x**3
        zz = .00074 * x3x**4 + .00079 * x3x**5 - .00029 * x3x**6
        tt = qq + zz
        bj1 = ff * cos(tt) / sqrt(xx)
      endif
      return
      end
c---------------------------------------------------------------


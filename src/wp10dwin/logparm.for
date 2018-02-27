        subroutine logparm (iant,nel, rlnel, rl1, hnel, h1, dc, z0)
c
c       LOG-PERIODIC ANTENNA PARAMETERS CALCULATION
c
      common/general/d1,e1,f1,p1,q1,s1,beta,rlambda
      common/logp/fi(35),rim(35),x(35),y(35),iek(35),rl(35),h(30)
      common/ccparm/uust,nat,ah1,bh1
      common/surf/ro,costh,sinth,rnumb,gamma,epq
c      
        dimension d(30),ria(30)
c
ccc        write(*,100)nel,rlnel,rl1,hnel,h1,dc,z0
ccc  100   format(' nel,rlnel,rl1,hnel,h1,dc,z0 = ',I2,6f6.1)     
c
        rl(nel) = rlnel
        rl(1) = rl1
        h(nel) = hnel
        h(1) = h1
        rlr = rl1 / rlnel
        exl = 1.0 / (nel - 1.0)
        tau = rlr **exl
        rld = rlnel - rl1
        sinth = (hnel - h1) / dc
        costh = sqrt(1 - sinth **2)
ccc        write(*,110)rlr,exl,tau,rld,sinth,costh
ccc  110   format(' rlr,exl,tau,rld,sinth,costh = ',6f6.1)        
c
      if (iant.ne.5) then
c===========================================================================
c  CALC. ELEM. LENGTH L(I), HEIGHT H(I), DISTANCES D(I) - VERT.LOG-PERIODIC
c===========================================================================
        dl1 = dc * rl1 / rld
        da1 = dl1 * costh
        db1 = dl1 * sinth
        dc1 = db1 - rl1
        tg23 = sinth / costh
        alfa23 = atan(tg23)
        tg3 = dc1 / da1
        alfa3 = atan(tg3)
        alfa2 = alfa23 - alfa3
        sigma = (1 - tau) / (4 * (sinth - tg3 * costh))
        theta = sin(alfa3) * costh / sin(alfa2)
        h(nel) = rl(nel) * (1 + theta)
        h0 = h1 - rl1 * (1 + theta)
        do 600 i = (nel - 1),1, -1
        rl(i) = rl(i + 1) * tau
        d(i) = 4 * rl(i + 1) * sigma
        h(i) = rl(i) * (1 + theta) + h0
  600   continue
        y(1) = rl(1) / (tg23 * (1 - tg3 / tg23))
        do 610 i = 2, nel
        y(i) = y(i - 1) + d(i - 1) * costh
  610   continue
      else 
c============================================================================
c CALC. ELEM. LENGTH L(I), HEIGHT H(I), DISTANCES D(I) - HORIZ.LOG-PERIODIC
c============================================================================
        alftan = rld / dc
        thetan = sinth / costh
        theta = atan(thetan)
        alfa = atan(alftan)
        sigma = (1 - tau) / (4 * alftan)
        d(nel) = 4 * rl(nel) * sigma
        do 620 i = (nel - 1),1,-1
        rl(i) = rl(i + 1) * tau
        d(i) = 4 * rl(i + 1) * sigma
  620   continue
        x(1) = rl(1) / alftan * costh
        do 630 i = 2,nel
        x(i) = x(i - 1) + d(i - 1) * costh
        h(i) = h(i - 1) + d(i - 1) * sinth
  630   continue
      endif
c============================================================================
c       CALCULATION OF LC, LLOW, LUP AND L0 ( l/a = 500 is assumed )
c============================================================================
        z0m = z0 / 1000
        shf = 1.098790227 - 1.055146365 * z0m + 3.208544524 *
     &      z0m **2 - 5.766460847 * z0m **3 + 4.054233788 * z0m **4
        rlc = rlambda * shf / 4.0
        bar = 1.1 + 30.7 * sigma * (1 - tau)
        rllow = rlc / bar
        rlup = 1.1 * rlc
ccc        write(*,200)rllow,rlup
ccc  200   format(' rllow,rlup = ',2f6.1)        
        rl0 = rllow + .7166 * (rlc - rllow)
c============================================================================
c       DETERMINATION OF INDEX EK(I) OF ELEMENTS FALLING IN LLOW - LUP RANGE
c============================================================================
        k = 0
        do 650 i = 1,nel
        if (rl(i).ge.rllow .and. rl(i).le.rlup) then
          k = k + 1
          iek(k) = i
        endif
  650   continue
        nat = k
ccc        write(*,220)nat
ccc  220   format(' nat = ',i2)
ccc        read(*,230)iii
ccc  230   format(i1)                
c============================================================================
c       CALCULATION OF CURRENT IA(I) AND PHASE FI(I) OF ACTIVE ELEMENTS
c============================================================================
        call curcal(rl0, rllow, rlc, rlup, aa1, aa2, aa3, aa4,
     1    aa5, bb1, bb2, bb3, bb4, bb5, cc1, cc2, cc3, cc4, cc5)
      do 900 k = 1,nat
      i = iek(k)
      if (rl(i).ge.rllow.and.rl(i).lt.(rllow+(rl0-rllow)/2.0)) then
          ria(i) = aa1 * rl(i) **2 + bb1 * rl(i) + cc1
      elseif (rl(i).ge.(rllow + (rl0 - rllow) / 2.0) .and.
     &  rl(i).lt.(rllow + 3.0 * (rl0 - rllow) / 4.0)) then
          ria(i) = aa2 * rl(i) **2 + bb2 * rl(i) + cc2
      elseif (rl(i).ge.(rllow + 3 * (rl0 - rllow) / 4.0) .and.
     &  rl(i).le.rl0) then
          ria(i) = aa3 * rl(i) **2 + bb3 * rl(i) + cc3
      elseif (rl(i).ge.rl0 .and. rl(i).le.rlc) then
          ria(i) = aa4 * rl(i) **2 + bb4 * rl(i) + cc4
      elseif (rl(i).ge.rlc .and.rl(i).le.rlup) then
          ria(i) = aa5 * rl(i) **2 + bb5 * rl(i) + cc5
      else
          write(*,*)'current calculation error!'
      endif  
      if (rl(i).ge.rllow .and. rl(i).le.rl0) then
        af = 150 / (rl0 - rllow)
      else
        af = 142 / (rlup - rl0)
      endif
        bf = -af * rl0
        fi(i) = af * rl(i) + bf
  900   continue
c============================================================================
c       CALCULATION OF NORMALIZED CURRENT IM(I) TO IMAX=1 AND PHASES FI(I)
c============================================================================
        kmax = 0
        riamax = 0
        do 960 k = 1,nat
        i = iek(k)
        if (ria(i).ge.riamax) then
          kmax = i
          riamax = ria(i)
          fimin = fi(i)
        endif
  960   continue
        do 980 k = 1,nat
        i = iek(k)
        rinm = ria(i) / riamax
        fi(i) = fi(i) - fimin
        rim(i) = rinm / sin(beta * rl(i))
  980   continue
        return
        end
c--------------------------------------------------------------------

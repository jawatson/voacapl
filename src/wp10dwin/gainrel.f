      subroutine gainrel (iant,u, v, z9)
c
c    RELATIVE GAIN CALCULATION  SUBROUTINE
c          iant = antenna type index number [1-10]
c          u    = elevation angle [0-90]
c          v    = off azimuth angle [0-360]
c          z9   = returned relative gain
c
      common/general/d1,e1,f1,p1,q1,s1,beta,rlambda
      common/cros/cpfr,cp11,fr1,h1,pfr,p11,p11s,q2,r2,r3,sl
      common/ccparm/uust,nat,ah1,bh1
      common/warr/w3(0:90),w4(0:90),w5(0:90),w6(0:90),
     &            w9(0:90),w10(0:90)
      common/wwar/ww1(0:90,0:30),ww2(0:90,0:30),
     &            ww3(0:90,0:30),ww4(0:90,0:30)
c     
      common/logp/fi(35),rim(35),x(35),y(35),iek(35),rl(35),h(30)
      common/trig/a(0:361),b(0:361)
      common/surf/ro,costh,sinth,rnumb,gamma,epq
c
      z9=0.
      if(iant.eq.0) return
        a0 = a(ifix(u))
        b0 = b(ifix(u))
        if ((v.ge.0.and.v.le.90).or.
     &      (v.ge.360.and.v.le.450)) then 
          kv = mod(v,360.0)
          jv = kv
          a1 = a(jv)
          b1 = b(jv)
        elseif ((v.gt.90.and.v.le.180).or.
     &          (v.gt.450.and.v.le.540)) then
          kv = mod(v,360.0)
          jv = (180 - kv)
          a1 = a(jv)
          b1 = -b(jv)
        elseif (v.gt.180.and.v.le.270) then
          jv = (v - 180)
          a1 = -a(jv)
          b1 = -b(jv)
        else
          jv = (360 -v)
          a1 = -a(jv)
          b1 = b(jv)
        endif              
        a11 = a1 * b0
        a12 = b1 * b0
c
c        
      IF (IANT.EQ.1 ) THEN
c===========================================================================
c       MULTIBAND APERIODIC REFL. DIPOLE ARRAY     
c==========================================================================
        b2 = cos(p11 * a11) - cp11
        w0 = abs(1 - a11 * a11)
        z1 = b2 / w0
        fx = fr1 * b0
        fy = 1 + 1 / (fx * fx)
        q2 = 1 - 1 / sqrt(fy)
        kv=mod(v,360.0)
        if (kv.lt.90 .or. kv.gt.270) then
          z2 = sqrt(abs(1 + q2**2 - 2 * q2 * cos(p11s * b0 * b1)))
        else
          z2 = 1 - q2
        endif
        pfb = pfr * b0
        asl = a1 - sl
        pfrb = pfb * asl
        u1 = 0
        u2 = 0
        do 6100 il = 0,r2 - 1
        u12 = pfrb * il
        an = sin(u12)
        u2 = u2 + an
        bn = cos(u12)
        u1 = u1 + bn
 6100   continue
        z3 = sqrt(u1 * u1 + u2 * u2)
        w7 = a1 * a0
        w7 = w7*w7*(w4(ifix(u))*w4(ifix(u))+w3(ifix(u))*w3(ifix(u)))
        w8 = b1*b1*(w5(ifix(u))*w5(ifix(u))+w6(ifix(u))*w6(ifix(u)))
        z4 = sqrt(abs(w7 + w8))
        z9 = z1 * z2 * z3 * z4
c
c
      elseif (iant.eq.2 ) then
c=========================================================================     
c       DUAL-BAND CENTRE-FED TUNED REFL. DIPOLE ARRAY
c=========================================================================
        pfb = pfr * b0
        b2 = cos(p11 * a11) - cp11
        w0 = abs(1 - a11 * a11)
        z1 = b2 / w0
        z2 = sqrt(abs(1 + q2**2 + 2 * q2 * cos(p11 - p11 * b0 * b1)))
        asl = a1 - sl
        pfrb = pfb * asl
        u1 = 0
        u2 = 0
        do 6200 il = 0,r2 - 1
        u12 = pfrb * il
        an = sin(u12)
        u2 = u2 + an
        bn = cos(u12)
        u1 = u1 + bn
 6200   continue
        z3 = sqrt(u1 * u1 + u2 * u2)
        w7 = a1 * a0
        w7 = w7*w7*(w4(ifix(u))*w4(ifix(u))+w3(ifix(u))*w3(ifix(u)))
        w8 = b1*b1*(w5(ifix(u))*w5(ifix(u))+w6(ifix(u))*w6(ifix(u)))
        z4 = sqrt(abs(w7 + w8))
        z9 = z1 * z2 * z3 * z4
c
c
      elseif (iant.eq.3 ) then
c=========================================================================
c       DUAL-BAND END-FED TUNED REFL. DIPOLE ARRAY
c=========================================================================
        b2 = cos(pfr * a11) - cpfr
        w0 = abs(1 - a11 * a11)
        z1 = b2 / w0
        z2 = sqrt(abs(1 + q2**2 + 2 * q2 * cos(p11 - p11 * b0 * b1)))
        pfb = pfr * b0 * 2
        asl = a1 - sl
        pfrb = pfb * asl
        u1 = 0
        u2 = 0
        do 6300 il = 0,(r2 / 2.0 - 1)
        u12 = pfrb * il
        an = sin(u12)
        u2 = u2 + an
        bn = cos(u12)
        u1 = u1 + bn
 6300   continue
        z3 = sqrt(u1 * u1 + u2 * u2)
        w7 = a1 * a0
        w7 = w7*w7*(w4(ifix(u))*w4(ifix(u))+w3(ifix(u))*w3(ifix(u)))
        w8 = b1*b1*(w5(ifix(u))*w5(ifix(u))+w6(ifix(u))*w6(ifix(u)))
        z4 = sqrt(abs(w7 + w8))
        z9 = z1 * z2 * z3 * z4
c
      elseif (iant.eq.4 ) then
c============================================================================
c       TROPICAL ANTENNAS
c============================================================================
        pfb = pfr * b0
        b2 = cos(p11 * a11) - cp11
        w0 = abs(1 - a11 * a11)
        z1 = b2 / w0
        pfra = pfb * b1
        u3 = 0
        u4 = 0
        do 6400 il = 0,r3-1
        u13 = pfra * il
        am = sin(u13)
        u4 = u4 + am
        bm = cos(u13)
        u3 = u3 + bm
 6400   continue
        z2 = sqrt(u3 * u3 + u4 * u4)
        asl = a1 - sl
        pfrb = pfb * asl
        u1 = 0
        u2 = 0
        do 6420 il = 0,r2 - 1
        u12 = pfrb * il
        an = sin(u12)
        u2 = u2 + an
        bn = cos(u12)
        u1 = u1 + bn
 6420   continue
        z3 = sqrt(u1 * u1 + u2 * u2)
        w7 = a1 * a0
        w7 = w7*w7*(w4(ifix(u))*w4(ifix(u))+w3(ifix(u))*w3(ifix(u)))
        w8 = b1*b1*(w5(ifix(u))*w5(ifix(u))+w6(ifix(u))*w6(ifix(u)))
        z4 = sqrt(abs(w7 + w8))
        z9 = z1 * z2 * z3 * z4
c
c
      elseif (iant.eq.5) then
c=========================================================================
c       LOG-PERIODIC
c=========================================================================
        b3 = -a12 * costh + a0 * sinth
        gamma = beta * b3 / costh
c===========================================================================
c       HORIZONTAL LOG-PERIODIC
c===========================================================================
        df = 1 - b0 * b0 * a1 * a1
        str = 0
        sti = 0
        sfr = 0
        sfi = 0
c        write(*,6450)nat
c 6450   format(' nat = ',i4)
c        write(*,6460)(iek(jj),jj=1,nat)
c 6460   format(' iek = ',25i3)
c        write(*,6470)'rl',rl
c 6470   format(' ',a2,' = ',25f4.1)
c        write(*,6470)'x',x
c        write(*,6470)'fi',fi
c        write(*,6470)'ri',rim
        do 6500 k = 1,nat
        i = iek(k)
        fg = beta * rl(i)
        ft = (cos(fg * b0 * a1) - cos(fg)) / df
        aw = sin(gamma * x(i) + fi(i) * q1)
        bw = cos(gamma * x(i) + fi(i) * q1)
        wr = aw * ww1(ifix(u), i) - bw * ww2(ifix(u), i)
        wi = bw * ww1(ifix(u), i) + aw * ww2(ifix(u), i)
        tr = rim(i) * ft * wr
        ti = rim(i) * ft * wi
        str = str + tr
        sti = sti + ti
        ur = bw * ww3(ifix(u), i) - aw * ww4(ifix(u), i)
        ui = aw * ww3(ifix(u), i) + bw * ww4(ifix(u), i)
        fir = rim(i) * ft * ur
        fij = rim(i) * ft * ui
        sfr = sfr + fir
        sfi = sfi + fij
 6500   continue
        etheta = sqrt(str**2 + sti**2) * a1 * a0
        efi = -sqrt(sfr**2 + sfi**2) * b1
        z9 = sqrt(etheta**2 + efi**2)
c
c
      elseif (iant.eq.6) then
c=========================================================================
c       LOG-PERIODIC
c=========================================================================
        b3 = -a12 * costh + a0 * sinth
        gamma = beta * b3 / costh
c===========================================================================
c       VERTICAL LOG-PERIODIC
c===========================================================================
        delta = beta * a0
        sfr = 0
        sfi = 0
        do 6600 k = 1,nat
        i = iek(k)
        fg = beta * rl(i)
        ft = (cos(fg * a0) - cos(fg)) / b0
        aw = sin(gamma * y(i) + fi(i) * q1)
        bw = cos(gamma * y(i) + fi(i) * q1)
        ur = bw * ww1(ifix(u), i) - aw * ww2(ifix(u), i)
        ui = bw * ww2(ifix(u), i) + aw * ww1(ifix(u), i)
        fti = rim(i) * ft
        fir = fti * ur
        fij = fti * ui
        sfr = sfr + fir
        sfi = sfi + fij
 6600   continue
        z9 = sqrt(sfr**2 + sfi**2)
c
c
      elseif (iant.eq.7 ) then
c===========================================================================
c       HORIZONTAL RHOMBIC
c===========================================================================
        a2 = sinth * b1 + costh * a1
        b2 = costh * b1 - sinth * a1
        a3 = sinth * b1 - costh * a1
        b3 = costh * b1 + sinth * a1
        t5 = a2 * b0 - 1
        t6 = a3 * b0 - 1
        a4 = a2 / t5
        b4 = b2 / t5
        a5 = a3 / t6
        b5 = b3 / t6
        a6 = a4 - a5
        b6 = b4 + b5
        t7 = t5 + t6
        c2 = sin(sl * t5)
        d2 = cos(sl * t5)
        c3 = sin(sl * t6)
        d3 = cos(sl * t6)
        c4 = sin(sl * t7)
        d4 = cos(sl * t7)
        r1 = 1 - d3 - d2 + d4
        r2 = c4 - c3 - c2
        a7 = sin(h1 * a0)
        b7 = cos(h1 * a0)
        r3 = 1 + w5(ifix(u)) * b7 + w6(ifix(u)) * a7
        r4 = w5(ifix(u)) * a7 - w6(ifix(u)) * b7
        h4 = r1 * r3 - r2 * r4
        h5 = r1 * r4 + r2 * r3
        r5 = 1 - w3(ifix(u)) * b7 - w4(ifix(u)) * a7
        r6 = w3(ifix(u)) * a7 - w4(ifix(u)) * b7
        w1 = r1 * r5 - r2 * r6
        w2 = r1 * r6 + r2 * r5
        h6 = 30 * b6 * sqrt(h4**2 + h5**2)
        w3x = 30 * a0 * a6 * sqrt(w1**2 + w2**2)
        z9 = sqrt(h6**2 + w3x**2)
c
c
      elseif (iant.eq.8.or.iant.eq.9 ) then
c==========================================================================    
c       QUADRANT & CROSS-DIPOLE ANTENNAS
c==========================================================================
        aa1 = .707 * (b1 - a1)
        bb1 = .707 * (b1 + a1)
        a11 = aa1 * b0
        a12 = bb1 * b0
        b2 = cos(p11 * a11) - cp11
        w0 = abs(1 - a11 * a11)
        z1 = b2 / w0
        b3 = cos(p11 * a12) - cp11
        b4 = p11 * (bb1 - aa1) * b0
        if (iant.eq.8) then
          b5 = sin(b4)
          b4=cos(b4)
        else
          b5 = 0
          b4=1
        endif
        w0 = 1 - a11 * a11
        w00 = 1 - a12 * a12
        z2 = b3 / w00
        w7 = aa1 * a0 * b0 * z1
        w71y = w7 * w9(ifix(u))
        w72y = w7 * w10(ifix(u))
c******    modified 18/8/92 : b1 changed to bb1 ******
        w7 = -bb1 * a0 * b0 * z2
        w711x = w7 * w9(ifix(u))
        w721x = w7 * w10(ifix(u))
        w71x = w711x * b4 - w721x * b5
        w72x = w711x * b5 + w721x * b4
        w7 = -bb1 * a0 * a0 * z2
        w31x = w7 * w3(ifix(u))
        w41x = w7 * w4(ifix(u))
        w3x = w31x * b4 - w41x * b5
        w4x = w31x * b5 + w41x * b4
        w7 = aa1 * a0 * a0 * z1
        w3y = w7 * w3(ifix(u))
        w4y = w7 * w4(ifix(u))
        w7 = (w3x + w3y)**2 + (w4x + w4y)**2
     &     + (w71x + w71y)**2 + (w72x + w72y)**2
        w81x = -aa1 * w5(ifix(u)) * z2
        w8y = -bb1 * w5(ifix(u)) * z1
        w91x = -aa1 * w6(ifix(u)) * z2
        w8x = w81x * b4 - w91x * b5
        w9x = w81x * b5 + w91x * b4
        w9y = -bb1 * w6(ifix(u)) * z1
        w8 = (w8x + w8y)**2 + (w9x + w9y)**2
        z9 = sqrt(w7 + w8)
c
      elseif (iant.eq.10 ) then
c============================================================================
c       VERTICAL MONOPOLE
c===========================================================================
        z9 = w9(ifix(u)) * w10(ifix(u))
      endif
      return
      end
c--------------------------------------------------------------------

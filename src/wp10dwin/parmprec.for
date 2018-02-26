      subroutine parmprec (iant, iperf)
c      
        dimension w1(0:90), w2(0:90), au(0:30), bu(0:30)
c        
        common/warr/ w3(0:90),w4(0:90),w5(0:90),w6(0:90),
     &            w9(0:90),w10(0:90)
        common/wwar/ ww1(0:90,0:30),ww2(0:90,0:30),
     &               ww3(0:90,0:30),ww4(0:90,0:30)
c        
      common/general/d1,e1,f1,p1,q1,s1,beta,rlambda
      common/cros/cpfr,cp11,fr1,h1,pfr,p11,p11s,q2,r2,r3,sl
      common/ccparm/uust,nat,ah1,bh1
      common/logp/fi(35),rim(35),x(35),y(35),iek(35),rl(35),h(30)
      common/surf/ro,costh,sinth,rnumb,gamma,epq
      common/trig/a(0:361),b(0:361)
c
      do 4030 u=0,90
c      ,uust
c
      b0 = b(ifix(u))
      a0 = a(ifix(u))
c
      if(iant.le.4.or.iant.eq.8.or.iant.eq.9) then
        pfra = pfr * a0 * 2
        w1(ifix(u)) = 0
        w2(ifix(u)) = 0
        if (iant.ge.4) then 
          pfrh = pfra * h1
          w1(ifix(u)) = cos(pfrh)
          w2(ifix(u)) = sin(pfrh)
        else
          do 4004 il=0,nint(r3-1)
          rik = il / 2.0
          h11 = (h1 + rik) * pfra
          am = sin(h11)
          w2(ifix(u)) = w2(ifix(u)) + am
          bm = cos(h11)
          w1(ifix(u)) = w1(ifix(u)) + bm
 4004     continue
        endif
c===========================================================================
c       CALCUL. ON PERFECT GROUND (PERF%=1) (Rv = T1+jT2 = 1, Rh = T3+jT4= -1)
c===========================================================================
        if (iperf.ne.0)then 
          w3(ifix(u)) = 0
          w4(ifix(u)) = 2 * w2(ifix(u))
          w5(ifix(u)) = 0
          w6(ifix(u)) = 2 * w2(ifix(u))
          w9(ifix(u)) = 2 * w1(ifix(u))
          w10(ifix(u)) = 0
        else
c===========================================================================
c       CALCULATION ON IMPERFECT GROUND (PERF%=0)
c===========================================================================
          call refcof(a0, b0, d1,e1,t1, t2, t3, t4)
          w31 = 1 - t1
          w32 = 1 + t1
          w33 = 1 + t3
          w34 = 1 - t3
          w3(ifix(u)) = w31 * w1(ifix(u)) - t2 * w2(ifix(u))
          w4(ifix(u)) = w32 * w2(ifix(u)) - t2 * w1(ifix(u))
          w5(ifix(u)) = w33 * w1(ifix(u)) + t4 * w2(ifix(u))
          w6(ifix(u)) = w34 * w2(ifix(u)) + t4 * w1(ifix(u))
c     
          w9(ifix(u)) = w32 * w1(ifix(u)) + t2 * w2(ifix(u))
          w10(ifix(u)) = w31 * w2(ifix(u)) + t2 * w1(ifix(u))
        endif  
      elseif(iant.eq.5.or.iant.eq.6) then
c==========================================================================
c       LOG-PERIODIC ANT. PARAM.
c==========================================================================
        delta = beta * 2 * a0
        if (iant.eq.6) delta = delta / 2.0
        do 4012 k = 1,nat
        i = iek(k)
        au(i) = sin(delta * h(i))
        bu(i) = cos(delta * h(i))
        if (iperf.ne.0) then
          if (iant.eq.6) then
            ww1(ifix(u), i) = 2 * bu(i)
            ww2(ifix(u), i) = 0
          else
            ww1(ifix(u), i) = 1. - bu(i)   !  changed 11/8/91 from Rossi
            ww3(ifix(u), i) = 1. - bu(i)   !  changed 11/8/91
            ww2(ifix(u), i) = au(i)
            ww4(ifix(u), i) = au(i)
          endif
        else
          call refcof(a0, b0, d1,e1,t1, t2, t3, t4)
          if (iant.eq.6) then
            ww1(ifix(u), i) = (1 + t1) * bu(i) + t2 * au(i)
            ww2(ifix(u), i) = (1 - t1) * au(i) + t2 * bu(i)
          else
            ww1(ifix(u), i) = 1 - t1 * bu(i) - t2 * au(i)
            ww2(ifix(u), i) = t1 * au(i) - t2 * bu(i)
            ww3(ifix(u), i) = 1 + t3 * bu(i) + t4 * au(i)
            ww4(ifix(u), i) = t4 * bu(i) - t3 * au(i)
          end if
        endif
4012    continue
      elseif(iant.eq.7) then
c==========================================================================
c       RHOMBIC ANTENNAS
c==========================================================================
        if (iperf.ne.0) then
          w3(ifix(u)) = 1
          w4(ifix(u)) = 0
          w5(ifix(u)) = -1
          w6(ifix(u)) = 0
        else
          call refcof(a0, b0, d1,e1,t1, t2, t3, t4)
          w3(ifix(u)) = t1
          w4(ifix(u)) = t2
          w5(ifix(u)) = t3
          w6(ifix(u)) = t4
        endif
      elseif(iant.eq.10) then 
c==========================================================================
c       VERTICAL MONOPOLE
c==========================================================================
        a1h = sin(h1 * a0)
        b1h = cos(h1 * a0)
        if (iperf.ne.0) then
          w9(ifix(u)) = abs((b1h - bh1) / b0)
          w10(ifix(u)) = 1
        else
          call refcof(a0, b0, d1,e1,t1, t2, t3, t4)
          thetr = -ah1 * t2 * a0 + (b1h - bh1) * (1 + t1) + a1h * t2
          theti = (1 - t1) * (a1h - ah1 * a0) + t2 * (b1h - bh1)
          w9(ifix(u)) = sqrt(thetr * thetr + theti * theti) / (2 * b0)
          if (abs(ro).le.0.001) then
            w10(ifix(u)) = 1
          else
            fteta0 = abs((b1h - bh1) / b0)
            hh = ro / 60
            sa = 0
            sb = 0
            do 4024 jj = 1,30
            x0 = (2 * jj - 1) * hh
            y0 = 2 * jj * hh
            yy = x0
            call surfim(yy, dlteta, psi)
            dx0 = dlteta
            alf = psi
            yy = y0
            call surfim(yy, dlteta, psi)
            dy0 = dlteta
            bta = psi
            qx = x0 * b0
            qy = y0 * b0
            sx = sqrt(h1**2 + x0**2)
            sy = sqrt(h1**2 + y0**2)
            xx = qx
            call bessel(xx, bj1)
            q0j1 = bj1
            xx = qy
            call bessel(xx, bj1)
            w0j1 = bj1
            q0x = q0j1 * dx0
            w0y = w0j1 * dy0
            f4 = 4 * (bh1 * cos(alf - x0) - cos(alf - sx)) * q0x
            s4 = 4 * (bh1 * sin(alf - x0) - sin(alf - sx)) * q0x
            if (jj.ne.30) then
              sa = sa+f4+2*(bh1*cos(bta - y0) - cos(bta - sy))*w0y
              sb = sb+s4+2*(bh1 * sin(bta-y0)-sin(bta-sy))*w0y
            else
              sa = hh/3.0*(sa+f4+(bh1*cos(bta-y0)-cos(bta-sy))*w0y)
              sb = hh/3.0*(sb+s4+(bh1*sin(bta-y0)-sin(bta-sy))*w0y)
            endif
4024        continue
            if(fteta0.ne.0.) then
               w10(ifix(u))=sqrt((1 + sa/fteta0)**2 + (sb/fteta0)**2)
            else
               w10(ifix(u))=0.
            end if
          endif
        endif
      endif
4030  continue
      return
      end
c--------------------------------------------------------------------

      subroutine gainorm ( z6, umax, vmax, iperf,iant,ifs)
*
*       calculation of the gain normalizing factor
*
*        dimension w(90)
*
c  pre-set angles of maximum radiation for simple cases
c  in the vertical plane
c
      if(iant.eq.4.or.(iant.lt.4.and.ifs.ne.0)) then        
        if(iperf.eq.1) then
          u=0.0
        else
          if(iant.eq.4) then
            u=45.0
          else
            u=5.0
          endif
        endif
c
c  find maximum in horizontal plane
c         
        vmin1=270
        vmax1=450
        vm=10
        rmax=0
        do 1435 v=vmin1,vmax1,vm
        call gainrel(iant,u, v, z9)
        if(z9.gt.rmax) then
          rmax=z9
          vmax=v
        endif
 1435   continue        
        z6=z9
        vmin1=vmax-10
        vmax1=vmax+10
        vm=1
        call azmax(iant,u, vmin1, vmax1, vm, rmax, vmax)
        if(iperf.eq.1) return
      else
        vmax=0
        if(iperf.ne.0) then
          u=0
          v=0
          call gainrel(iant,u,v,z9)
          z6=z9
          return
        endif
      endif
      v=vmax
      wmx=0
      do 1520  u = 1,90
      call gainrel(iant,u, v, z9)
      if(z9.ge.wmx) then
        wmx=z9
        umax=u
      endif
1520  continue
      z6 = wmx
      if(ifs.ne.0.or.iant.eq.4) then
        u=umax
        i=0
        rmax=z6
        vmin1=vmax-5.0
        vmax1=vmax+5.0
        vm=1
        call azmax(iant,u, vmin1, vmax1, vm, rmax, vmax)
        z6 = rmax
        if(vmax.ge.360) vmax=vmax-360
      endif
      return
      end
c--------------------------------------------------------------------

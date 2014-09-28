        subroutine azmax (iant,u, vmin1, vmax1, vm, rmax, vmax)
c          given:
c               u = elevation angle
c               azimuth from vmin1 to vmax1, step vm
c          find:
c               rmax = maximum relative gain
c               vmax = azimuth angle at vmax
c
        dimension w(0:362)
c
        i = 0
        j = 0
        w(0) = rmax
        do 1675 v = vmin1, vmax1, vm
        call gainrel(iant,u, v, z9)
        i = i + 1
        w(i) = z9
ccc        if (w(i).lt.w(i - 1)) then
        if (w(i).lt.rmax) then
          j=0
ccc        elseif (w(i).gt.w(i - 1)) then
        elseif (w(i).gt.rmax) then
          rmax=w(i)
          vmax=v
        else       
          j = j + 1
          rmax = w(i)
          if(v.ne.vmax) vmax = v - j / 2.0 + 1.0
        endif
 1675   continue
        return
        end
c----------------------------------------------------------------

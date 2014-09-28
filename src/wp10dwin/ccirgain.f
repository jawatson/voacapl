      subroutine ccirgain(iant,u, v, z6,giso,gain)
c******************************************************************
c    GAIN CALCULATION  SUBROUTINE
c      INPUT  PARAMETERS:
c          iant = antenna type index number [0-10]
c          u    = elevation angle [0-90]
c          v    = off main beam azimuth angle [0-360]
c          z6   = gain normalizing factor from antinit
c          giso = maximum gain of antenna relative to isotrope (user input)
c      OUTPUT PARAMETERS:
c          gain    = calculated gain returned
c******************************************************************
c       Written by Greg Hand NTIA/ITS Boulder, Colorado   Aug 1991
c       Please call (303)497-3375 with any questions.
c*******************************************************************
c                                antenna floor value
      common /floorc/ floor
      common /ctype11g/ gain_type11(91)
      common /ccirparm/ parm(20)
c
      gain=giso
c                                     iant=0=isotrope
      if(iant.eq.0) return
      if(iant.eq.11) then           ! gain table
         ielev=nint(u)
         if(ielev.lt.0) ielev=0
         if(ielev.gt.90) ielev=90
         gain=giso + gain_type11(ielev+1)
         return
      else if(iant.eq.12) then
         call curtain(parm,v,u,gain,z6)
         gain=gain+giso
         return
      end if
c                                     calculate relative gain
      call gainrel(iant,u,v,z9)
c                                     convert relative to maximum
      if(z6.eq.0.) then
         gain=-floor
         return
      end if
      dgs=z9/z6
      if(dgs.le..03162278) then
         g9=-30.
      else
         g9=20.*alog10(dgs)
      end if
c                                     floor value limit
      if(g9.lt.-floor) g9=-floor 
      gain=g9 + gain
      if(gain.lt.-floor) gain=-floor
      return
      end
c------------------------------------------------------------------

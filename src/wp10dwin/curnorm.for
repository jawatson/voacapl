
      subroutine curnorm(parm,z6,umax,vmax,gmax)
c*******************************************************************
c       PURPOSE:
c          initialize NTIA Report 83-215 antenna calculations
c       INPUT  PARAMETERS:
c          parm(1-n) = parameters that define the antenna characteristics
c       OUTPUT PARAMETERS:
c          z6 = gain normalizing factor (used by ccirgain)
c          umax = elevation angle at z6
c          vmax = azimuthal angle at z6
c*******************************************************************
c       Written by Greg Hand NTIA/ITS Boulder, Colorado   Aug 1991
c       Please call (303)497-3375 with any questions.
c*******************************************************************
      dimension parm(20)
      gmax=-99999.
      nazim=0
      if(parm(15).ne.0) nazim=nint(abs(parm(15)))+1
      nazim=nazim*2
      do 10 iazim=0,nazim
      azim=iazim
      if(nazim.ne.0) azim=iazim-nazim/2
      do 10 ielev=0,45
      elev=ielev
      call curtain(parm,azim,elev,gain,z6)
      if(gain.gt.gmax) then
         gmax=gain
         umax=elev
         vmax=azim
      end if
10    continue
      return
      end

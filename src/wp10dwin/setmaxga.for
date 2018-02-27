      subroutine setmaxgain(freqoper)
c*******************************************************************
c       PURPOSE:
c          set maximum gain parameter (giso) for use by ANTCAL
c       INPUT  PARAMETERS:
c          freqoper  = Operating Frequency
c          freqdesign= Design Frequency (=0=use Operating Frequency)
c                    = in range .7 to 1.4  use as FreqRatio
c                        thus   fdesign=foper/FreqRatio
c       OUTPUT PARAMETERS:
c*******************************************************************
      common /designfreq/ freqdesign
      common /ccirparm/ parm(20),z6,umax,vmax,giso,
     +                  modegain,gainmax(3,2),gainmaxb(30)
      iant=nint(parm(2))                  !  type [1,2,...12]
      if(modegain.eq.0) then
         giso=parm(1)                     !  maximum gain
      else if(modegain.eq.1) then
c          interpolate on Frequency ratio and Operating frequency
         foper=freqoper                   !  set operating frequency
         if(foper.lt.2. .or. foper.gt.30.) foper=10.
         fdesign=freqdesign               !  set design frequency
c          was this Frequency Ratio (.7 to 1.4)
         if(fdesign.ge..7 .and. fdesign.le.1.4)fdesign=foper/freqdesign
         if(fdesign.lt.2. .or. fdesign.gt.30.) fdesign=foper
         parm(5)=foper
         parm(8)=fdesign
         if(iant.eq.8) parm(6)=fdesign     !  quadrant antenna
         giso=gainterp(gainmax,foper,fdesign,iant)
      else if(modegain.eq.2) then
         parm(5)=freqoper
         giso=gainterb(gainmaxb,freqoper)  !  types 5,6,7,10
      end if
      return
      end
c----------------------------------------------------------------------

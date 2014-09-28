      subroutine antcal(bmain,bazim,del,gain)
c******************************************************************
c          bmain- is main beam azmuthal bearing (degrees)
c          bazim- is azimuthal bearing (degrees) to calculate
c          del  - is elevation angle (degrees)
c          gain - is returned
c*****************************************************************
c   calculates gain of antenna for azimuthal offset and elevation angle
c
      common /gain_10/ gain10(90,29)
      common /ccirparm/ parm(20),z6,umax,vmax,giso,
     +                  modegain,gainmax(3,2),gnorm(30)
      common /ctype11g/ gain_type11(91)
      jant=nint(parm(2))         !  ccir antenna type
      gain=giso
      if(jant.eq.10) then        !  vertical monopole
         ielev=nint(del)
         if(ielev.lt.0) ielev=0
         if(ielev.gt.90) ielev=90
         if(ielev.eq.0) then
            gain=-30.
         else
            freq=parm(5)          !  operating freq
            ifreq=freq
            if(ifreq.lt.30) then
               gain=gain10(ielev,ifreq-1)+(freq-float(ifreq))*
     +                     (gain10(ielev,ifreq)-gain10(ielev,ifreq-1))
            else
               gain=gain10(ielev,29)
            end if
         end if
      else if(jant.eq.11) then        !  gain table
         ielev=nint(del)
         if(ielev.lt.0) ielev=0
         if(ielev.gt.90) ielev=90
         gain=gain + gain_type11(ielev+1)
      else if(jant.eq.12) then   !  NTIA Report 87-215 Curtain arrays
         ofaz=bazim-bmain
         if(ofaz.lt.0.) ofaz=ofaz+360.
         freq=parm(5)          !  operating freq
         ifreq=freq
         if(ifreq.lt.30) then
            gn=gnorm(ifreq)+(freq-float(ifreq))*
     +                      (gnorm(ifreq+1)-gnorm(ifreq))
         else
            gn=gnorm(30)
         end if
         call curtain(parm,ofaz,del,gain,gn)
         if(gain.lt.-30.) gain=-30.
      else if(jant.gt.0) then    !  not Isotrope
         ofaz=bazim-bmain
         if(ofaz.lt.0.) ofaz=ofaz+360.
         call ccirgain(jant,del,ofaz,z6,giso,gain)
      end if
      return
      end
c------------------------------------------------------------------

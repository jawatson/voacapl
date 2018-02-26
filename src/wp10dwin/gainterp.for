      function gainterp(gainaa,freqoper,freqdesign,iant)
c          interpolate directivity gain given:
c              gainaa(1-3,1) = gain @  2MHz Fr=.70,1.0,1.4  iant=1
c              gainaa(1-3,2) = gain @ 30MHz Fr=.70,1.0,1.4  iant=1
c              gainaa(1-3,1) = gain @  2MHz Fr=.85,1.0,1.2  iant=2,3,4,8,9
c              gainaa(1-3,2) = gain @ 30MHz Fr=.85,1.0,1.2  iant=2,3,4,8,9
c              freqoper      = operating frequency
c              freqdesign    = design    frequency
      dimension gainaa(3,2)
      Fr=freqoper/freqdesign      !  Frequency ratio
ccc      if(Fr.lt. .7) Fr= .7        !  lower limit
ccc      if(Fr.gt.1.4) Fr=1.4        !  upper limit
      if(iant.eq.1 .and. (Fr.lt. .7 .or. Fr.gt.1.4)) then
         gainterp=-30.            !  outside limits
      else if(iant.ne.1 .and. (Fr.lt. .85 .or. Fr.gt.1.2)) then
         gainterp=-30.            !  outside limits
      else
         g1=gainterp1(gainaa(1,1),Fr,iant)!   2 MHz Operating
         g2=gainterp1(gainaa(1,2),Fr,iant)!  30 MHz Operating
         gainterp=g1 + (g2-g1)*(freqoper-2.)/28.
      end if
      return
      end
c----------------------------------------------------
      function gainterp1(gainaa,Fr,iant)
      dimension gainaa(3),Frs(3,2)
      data frs/.7,1.0,1.4,.85,1.0,1.2/
      jant=iant
      if(jant.ne.1) jant=2
      idx=2                   !  between 1.0 & 1.4 (or 1.2)
      if(Fr.lt.1.) idx=1      !  between  .7 (or .85) & 1.0
      gainterp1=gainaa(idx) + (gainaa(idx+1)-gainaa(idx))*
     +             (Fr-Frs(idx,jant))/(Frs(idx+1,jant)-Frs(idx,jant))
      return
      end
c----------------------------------------------------

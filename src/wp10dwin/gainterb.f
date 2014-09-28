      function gainterb(gainab,freqoper)
c          interpolate directivity gain given:
c              gainab(2-30) = gain @  2 - 30 MHz
c              freqoper      = operating frequency
      dimension gainab(*)
      idx=freqoper
      if(idx.eq.30) idx=29     !  can't exceed 30
      fact=freqoper-float(idx)
      gainterb=gainab(idx) + (gainab(idx+1)-gainab(idx))*fact
      return
      end
c----------------------------------------------------

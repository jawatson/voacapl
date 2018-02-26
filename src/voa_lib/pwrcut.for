      subroutine pwrcut(snr50,snr_lw,snr_up,SNR88,SNR91,power_cut)
c          calculte Percent Power Cut possible based on George Lane Algorithm
c          An estimate of the number of days on which a power reduction can be
c          used during a given hour and month can be made by computing the
c          area under the assumed normal distribution of Signal-to-Noise
c          ratios over the days of the month.
c
c          SNR50 = SNR from IONCAP, the median S/N ratio
c          SNR_LW = lower decile deviation of SNR
c          SNR_UP = upper decile deviation of SNR
c          SNR88  = 3 dB (half power) SNR limit (nominally 88 dB)
c          SNR91  = 6 dB (quarter power) SNR limit (nominally 91 dB)
c          power_cut = [0 to .75], the calculated fraction power cut available
      dimension snr(11),fact(4)
      data fact/1.28,.84,.525,.255/
      std_lw=snr_lw/1.28              !  convert to standard deviation
      snr(11)=snr50 - fact(1)*std_lw*2.
      snr(10)=snr50 - fact(1)*std_lw
      snr(9)=snr50 - fact(2)*std_lw
      snr(8)=snr50 - fact(3)*std_lw
      snr(7)=snr50 - fact(4)*std_lw
      snr(6)=snr50
      std_up=snr_up/1.28              !  convert to standard deviation
      snr(5)=snr50 + fact(4)*std_up
      snr(4)=snr50 + fact(3)*std_up
      snr(3)=snr50 + fact(2)*std_up
      snr(2)=snr50 + fact(1)*std_up
      snr(1)=snr50 + fact(1)*std_up*2.
ccc      write(72,'('' snr='',11f8.3)') snr
      day3dB=dayinterp(snr,snr88)     !  fract days that exceed SNR88
      day6dB=dayinterp(snr,snr91)     !  fract days that exceed SNR91
      power_cut=1. - (1.-day3dB) - (day3dB-day6dB)/2. - day6dB/4.
ccc      write(72,1) power_cut,day3dB,day6dB
ccc1     format(' power_cut=',f8.3,5x,'day3dB=',f8.3,5x,'day6dB=',f8.3)
      return
      end
c---------------------------------------------------------------
      function dayinterp(snr,snrx)     !  fract days that exceed SNR88
      dimension snr(11)
      dayinterp=0.
      if(snrx.gt.snr(1)) return
      do 10 i=1,10
      if(snrx.le.snr(i) .and. snrx.ge.snr(i+1)) then
	 dayinterp=(float(i-1) + (snr(i)-snrx)/(snr(i)-snr(i+1)))/10.
         return
      end if
10    continue
      dayinterp=1.
      return
      end
c---------------------------------------------------------------

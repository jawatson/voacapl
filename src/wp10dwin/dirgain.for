        subroutine dirgain(iant,z6,giso,*)
c**********************************************************************
c       DIRECTIVITY GAIN CALCULATION (SIMPSON RULE INTEGRATION)
c          calculates gain relative to isotrope
c       INPUT  PARAMETERS:
c          iant = antenna index number [0-10]
c          z6   = gain normalizing factor (from ccirantinit)
c       OUTPUT PARAMETERS:
c          giso = directivity gain relative to isotrope
c**********************************************************************
      common/general/d1,e1,f1,p1,q1,s1,beta,rlambda
      common/cros/cpfr,cp11,fr1,h1,pfr,p11,p11s,q2,r2,r3,sl
      common/trig/a(0:361),b(0:361)
      common /c_bar/ pct_done,s_pct_done,pct_fact,iabort_dir
         real*8 pct_done
         character s_pct_done*4
c
c                                 iant=0=isotrope
      if(iant.eq.0) return
        iperf = 1
        call parmprec(iant,iperf)
        sup1 = 0.0
        sup2 = 0.0
        i = 0
        j = 0
        ust = 1.0
        vst = 1.0
        svd = 0.0
        svp = 0.0
        do 7840 iv=0,360
        v=iv
        if(iv.ne.0 .and. mod(iv,60).eq.0) then
           if(iabort_dir.ne.0) return 1
           pct_done=pct_done + pct_fact/6.
ccc           call window_update@(pct_done)
           write(s_pct_done,'(i3,1h%)') nint(pct_done*100.)
           call window_update@(s_pct_done)
        end if
        j = 0
        sud = 0
        sup = 0
        do 7710 u=0,90
        b0 = b(ifix(u))
        call gainrel(iant,u, v, z9)
        dsu = z9 **2 * b0
      if(abs(u).le.0.001) then
          sau = dsu
      else
        if(abs(u-90.0).le.0.001) then
          sbu = dsu
        else
          j = j + 1
          if (j.ne. (j / 2) * 2) then 
            sud = sud + dsu
          else
            sup = sup + dsu
          endif
        endif
      endif
 7710   continue
        dsv = (sau + sbu + 4 * sud + 2 * sup) * ust / 3.0 * q1 * q1
        if(abs(v).le.0.001) then
          sav = dsv
        else
          if (abs(v-360.0).le.0.001) then
            sbv = dsv
          else
            i = i + 1
            if (i.ne.(i / 2) * 2) then
              svd = svd + dsv
            else
              svp = svp + dsv
            endif
          endif
        endif
 7840   continue
        superf = (sav + sbv + 4 * svd + 2 * svp) * vst / 3.0
        requiv = sqrt(superf / (4 * p1))
      if(z6.gt.0.0) then
        giso = 20.* alog10(z6 / requiv)
      else
        giso=0.0
      endif
        iperf = 0
        call parmprec(iant, iperf)
        return
        end
c---------------------------------------------------------------

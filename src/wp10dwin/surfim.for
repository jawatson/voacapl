        subroutine surfim (yy, dlteta, psi)
c
      common/surf/ro,costh,sinth,rnumb,gamma,epq
c
      if (ro.le.0) then 
        srag = 0
        detar = costh
        detai = sinth
      else
        srag1 = yy / rnumb
        srag2 = alog(2 * yy / gamma)
        srag = srag1 * srag2
        sraq = srag * srag
        dnm = epq + sraq + 2 * srag * sinth
        etanr = sraq * costh
        etani = epq * srag + sraq * sinth
        etar = etanr / dnm
        etai = etani / dnm
        detar = costh - etar
        detai = sinth - etai
      endif
      dlteta = sqrt(detar**2 + detai**2)
      psi = atan(detai / detar)
      return
      end
c---------------------------------------------------------------


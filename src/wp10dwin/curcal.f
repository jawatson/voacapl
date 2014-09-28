        subroutine curcal (rl0, rllow, rlc, rlup, aa1, aa2, aa3,
     &    aa4, aa5, bb1, bb2, bb3, bb4, bb5, cc1, cc2, cc3, cc4, cc5)
c
c       EMPIRICAL ACTIVE ELEMENT CURVE CALCULATION SUBR.
c
c============================================================================
c       INTERVAL  LLOW <= L <+(L0-LLOW)/2
c=================================================
        x1 = rllow
        y1 = .316
        x2 = rllow + (rl0 - rllow) / 4.0
        y2 = .676
        x3 = rllow + (rl0 - rllow) / 2.0
        y3 = .876
        call parabol(x1, x2, x3, y1, y2, y3, aa, bb, cc)
        aa1 = aa
        bb1 = bb
        cc1 = cc
c============================================================================
c       INTERVAL : LLOW+(L0-LLOW)/2 <= L < 3*(L0-LLOW)/4
c============================================================================
        x1 = rl0
        y1 = 1
        x2 = rllow + 3 * (rl0 - rllow) / 4.0
        y2 = .976
        x3 = rllow + (rl0 - rllow) / 2.0
        y3 = .876
        call parabol(x1, x2, x3, y1, y2, y3, aa, bb, cc)
        aa2 = aa
        bb2 = bb
        cc2 = cc
c============================================================================
c       INTERVAL : 3*(L0-LLOW)/4 <= L < LLOW
c============================================================================
        x1 = rl0
        y1 = 1
        x2 = rllow + 3 * (rl0 - rllow) / 4.0
        y2 = .976
        aa3 = (y2 - y1) / (x2 - x1) **2
        bb3 = -2 * aa3 * x1
        cc3 = y1 + x1 * x1 * aa3
        aa = aa3
        bb = bb3
        cc = cc3
c============================================================================
c       INTERVAL : L0 <= L < LC
c============================================================================
        x1 = rl0
        y1 = 1
        x2 = rl0 + (rlc - rl0) / 2.0
        y2 = .922
        x3 = rlc
        y3 = .707
        call parabol(x1, x2, x3, y1, y2, y3, aa, bb, cc)
        aa4 = aa
        bb4 = bb
        cc4 = cc
c============================================================================
c       INTERVAL : LC <= L < LUP
c============================================================================
        x1 = rlc + (rlup - rlc) / 2.0
        y1 = .501
        x2 = rlup
        y2 = .316
        x3 = rlc
        y3 = .707
        call parabol(x1, x2, x3, y1, y2, y3, aa, bb, cc)
        aa5 = aa
        bb5 = bb
        cc5 = cc
        return
        end
c---------------------------------------------------------------


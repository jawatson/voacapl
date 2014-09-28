        subroutine parabol (x1, x2, x3, y1, y2, y3, aa, bb, cc)
c============================================================================
c       PARABOLIC PARAMETRIZED CURVE FITTING SUBROUTINE
c============================================================================
        dd = x1 * x1 * (x2 - x3) - x1 * (x2 * x2 - x3 * x3) +
     &     (x2 * x2 * x3 - x3 * x3 * x2)
        da = y1 * (x2 - x3) - x1 * (y2 - y3) + (y2 * x3 - x2 * y3)
        db = x1 * x1 * (y2 - y3) - y1 * (x2 * x2 - x3 * x3) +
     &     (x2 * x2 * y3 - x3 * x3 * y2)
        dc = x1 * x1 * (x2 * y3 - y2 * x3) - x1 *
     &    (x2 * x2 * y3 - y2 * x3 * x3) + y1 *
     &    (x2 * x2 * x3 - x3 * x3 * x2)
        aa = da / dd
        bb = db / dd
        cc = dc / dd
        return
        end
c--------------------------------------------------------------------


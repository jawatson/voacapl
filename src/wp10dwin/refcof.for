        subroutine refcof (a0, b0, d1,e1,t1, t2, t3, t4)
c
        c1 = e1 - b0 * b0
        c11 = sqrt(c1 * c1 + d1 * d1)
        e2 = sqrt(abs(c1 + c11) / 2.0)
        f2 = sqrt(abs(-c1 + c11) / 2.0)
        rk1 = e1 * a0 - e2
        rl1 = rk1 + e2 * 2
        rm1 = d1 * a0 - f2
        rn1 = rm1 + f2 * 2
        rl1n = rl1 * rl1 + rn1 * rn1
        t1 = (rk1 * rl1 + rm1 * rn1) / rl1n
        t2 = (rk1 * rn1 - rm1 * rl1) / rl1n
        g1 = a0 - e2
        h2 = a0 + e2
        hn = h2 * h2 + f2 * f2
        t3 = (g1 * h2 - f2 * f2) / hn
        t4 = (h2 * f2 + g1 * f2) / hn
        return
        end
c--------------------------------------------------------------------


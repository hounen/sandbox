      function  tkinm1 (j, p, h)                                                !/*tk          1*/
! ======================================================================        !/*tk          2*/
!***********************************************************************        !/*tk          3*/
!  Kinetic energy for particles in pre-equilibrium decay.              *        !/*tk          4*/
!***********************************************************************        !/*tk          5*/
!                                                                               !/*tk          6*/
!   CEM95 written by S. G. Mashnik                                              !/*tk          7*/
!                                                                               !/*tk          8*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*tk          9*/
!   Edited by AJS, August, 1997.                                                !/*tk         10*/
!   Edited by AJS, December, 1997.                                              !/*tk         11*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*tk         12*/
!                                                                               !/*tk         13*/
! ======================================================================        !/*tk         14*/
#include <cem/cdouble.h>                                                        /*!tk         15*/
#ifndef CEM                                                                     /*!tk         16*/
      tkinm1=dp0                                                                !/*tk         17*/
#else                                                                           /*!tk         18*/
      common /bl1003/  u, a, z                                                  !/*tk         19*/
      common /bl1005/  aj(6)                                                    !/*tk         20*/
      common /bl1009/ afj(7)                                                    !/*tk         21*/
      common /bl1011/  vj(6)                                                    !/*tk         22*/
      common /bl1015/  rj(7)                                                    !/*tk         23*/
      common /blbj/    bj(7)                                                    !/*tk         24*/
      common /br0j/    r0j(6), bn                                               !/*tk         25*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*tk         26*/
      common /repcnt/  irpt, irep, irec, itk1, itk3                             !/*tk         27*/
! ======================================================================        !/*tk         28*/
      if (j.le.1) then                                                          !/*tk         29*/
        dj = bn                                                                 !/*tk         30*/
      else                                                                      !/*tk         31*/
        dj = -vj(j)                                                             !/*tk         32*/
      endif                                                                     !/*tk         33*/
      t = p + h - aj(j) - dp1                                                   !/*tk         34*/
      r2 = rj(j)                                                                !/*tk         35*/
      r1 = r2 + vj(j)                                                           !/*tk         36*/
      if (r2.lt.dp1m3) then                                                     !/*tk         37*/
        tkinm1 = r1                                                             !/*tk         38*/
        return                                                                  !/*tk         39*/
      endif                                                                     !/*tk         40*/
      if (j.le.2) then                                                          !/*tk         41*/
!  Neutron or proton                                                            !/*tk         42*/
        if (t .le. -dp1m2) then                                                 !/*tk         43*/
          tkinm1 = r1                                                           !/*tk         44*/
        elseif (t.le.dp1m1) then                                                !/*tk         45*/
          b1 = rang()                                                           !/*tk         46*/
          if (j.le.1) then                                                      !/*tk         47*/
            tkinm1 = -dj + sqrt(dj**2 + b1*(r2**2 + dp2*dj*r2))                 !/*tk         48*/
          else                                                                  !/*tk         49*/
            tkinm1 = sqrt(b1)*r2 + vj(j)                                        !/*tk         50*/
          endif                                                                 !/*tk         51*/
        else                                                                    !/*tk         52*/
          e1 = (r1 - dj*t)/(t + dp1)                                            !/*tk         53*/
          e1 = max(-dp1m1, e1)                                                  !/*tk         54*/
          iq = 0                                                                !/*tk         55*/
   10     b1 = rang()                                                           !/*tk         56*/
          iq = iq + 1                                                           !/*tk         57*/
          e = vj(j) + b1*r2                                                     !/*tk         58*/
          t1 = (e + dj)/(e1 + dj)                                               !/*tk         59*/
          t2 = (r1 - e)/(r1 - e1)                                               !/*tk         60*/
          t3 = t1*(abs(t2)**t)                                                  !/*tk         61*/
          if (iq.ge.100) then                                                   !/*tk         62*/
            itk1 = itk1 + 1                                                     !/*tk         63*/
            tkinm1 = e                                                          !/*tk         64*/
            return                                                              !/*tk         65*/
          endif                                                                 !/*tk         66*/
          b2 = rang()                                                           !/*tk         67*/
          if (b2.gt.t3) go to 10                                                !/*tk         68*/
          tkinm1 = e                                                            !/*tk         69*/
        endif                                                                   !/*tk         70*/
      else                                                                      !/*tk         71*/
!   Composite particle                                                          !/*tk         72*/
        if (t .le. -dp1m1) then                                                 !/*tk         73*/
          tkinm1 = r1                                                           !/*tk         74*/
        elseif (t .le. dp1m1) then                                              !/*tk         75*/
          e1 = r1                                                               !/*tk         76*/
   20     b1 = rang()                                                           !/*tk         77*/
          e = vj(j) + b1*r2                                                     !/*tk         78*/
          pj1 = aj(j) - dp15m1                                                  !/*tk         79*/
          t3 = (abs((u - r1 + e)/(u - r1 + e1))**pj1)*                          !/*tk         80*/
     &                                          ((e + dj)/(e1 + dj))            !/*tk         81*/
          b2 = rang()                                                           !/*tk         82*/
          if (b2.gt.t3) go to 20                                                !/*tk         83*/
          tkinm1 = e                                                            !/*tk         84*/
        else                                                                    !/*tk         85*/
          pj1 = aj(j) - dp15m1                                                  !/*tk         86*/
          pj2 = p + h - dp15m1                                                  !/*tk         87*/
          pj3 = pj2 - dp1                                                       !/*tk         88*/
          pj2 = dp2*pj2                                                         !/*tk         89*/
          es = u*(aj(j) - dph) + (u - r2)*pj3                                   !/*tk         90*/
          e1 = (es + sqrt(es*es - (u - r2)*pj1*pj2*dp2*u))/pj2 - u + r1         !/*tk         91*/
          iq = 0                                                                !/*tk         92*/
   30     b1 = rang()                                                           !/*tk         93*/
          iq = iq + 1                                                           !/*tk         94*/
          e = vj(j) + b1*r2                                                     !/*tk         95*/
          if ((e1 + dp1m3).gt.r1) then                                          !/*tk         96*/
            tkinm1 = e                                                          !/*tk         97*/
            return                                                              !/*tk         98*/
          endif                                                                 !/*tk         99*/
          aux = (e + dj)/(e1 + dj)                                              !/*tk        100*/
          t3 = (abs((u - r1 + e)/(u - r1 + e1))**pj1)*aux*                      !/*tk        101*/
     &                                (abs((r1 - e)/(r1 - e1))**t)              !/*tk        102*/
          if (iq.ge.100) then                                                   !/*tk        103*/
            itk3 = itk3 + 1                                                     !/*tk        104*/
            tkinm1 = e                                                          !/*tk        105*/
            return                                                              !/*tk        106*/
          endif                                                                 !/*tk        107*/
          b2 = rang()                                                           !/*tk        108*/
          if (b2.gt.t3) go to 30                                                !/*tk        109*/
          tkinm1 = e                                                            !/*tk        110*/
        endif                                                                   !/*tk        111*/
      endif                                                                     !/*tk        112*/
#endif                                                                          /*!tk        113*/
      return                                                                    !/*tk        114*/
      end                                                                       !/*tk        115*/

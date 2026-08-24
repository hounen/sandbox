      function gameqf (j, per, am, radncl)                                      !/*ga          1*/
! ======================================================================        !/*ga          2*/
!                                                                               !/*ga          3*/
!     CEM95 written by S. G. Mashnik                                            !/*ga          4*/
!                                                                               !/*ga          5*/
!     Calculates the partial widths for statistical equilibrium decay           !/*ga          6*/
!     into particle emission channels and fission. The values of j              !/*ga          7*/
!     correspond to:                                                            !/*ga          8*/
!                                                                               !/*ga          9*/
!       j     channel           afj(j)           rj(j)                          !/*ga         10*/
!                                                                               !/*ga         11*/
!       1     neutron          mass of          thermal                         !/*ga         12*/
!       2     proton         nucleus after      energy                          !/*ga         13*/
!       3     deuteron        emission of       of nucleus                      !/*ga         14*/
!       4     triton          particle j        at emission                     !/*ga         15*/
!       5     He-3                "             (or fission)                    !/*ga         16*/
!       6     He-4                "             barrier.                        !/*ga         17*/
!       7     fission       mass of fissioning nuc.                             !/*ga         18*/
!                                                                               !/*ga         19*/
!     rj is the "thermal" energy available in the channel.                      !/*ga         20*/
!     afj is the mass number of the residual nucleus, for particle              !/*ga         21*/
!     emission; the mass number of the nucleus, for fission.                    !/*ga         22*/
!                                                                               !/*ga         23*/
!     Edited by A. J. Sierk   LANL  T-2,  February, 1996.                       !/*ga         24*/
!     Changed to double by F. Gallmeier, ORNL CPED, October 1998.               !/*ga         25*/
!                                                                               !/*ga         26*/
! ======================================================================        !/*ga         27*/
#include <cem/cdouble.h>                                                        /*!ga         28*/
#ifndef CEM                                                                     /*!ga         29*/
      gameqf=dp0                                                                !/*ga         30*/
#else                                                                           /*!ga         31*/
      common /adbf/ amf, ijsp, nhump, r0m                                       !/*ga         32*/
      common /bl1009/ afj(7)                                                    !/*ga         33*/
      common /bl1014/ gam(6)                                                    !/*ga         34*/
      common /bl1015/  rj(7)                                                    !/*ga         35*/
      common /bl1016/  cc(6)                                                    !/*ga         36*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*ga         37*/
      common /pii/     pi                                                       !/*ga         38*/
! ======================================================================        !/*ga         39*/
      if (j.ne.7) then                                                          !/*ga         40*/
        if (j.eq.1) then                                                        !/*ga         41*/
          alfa = dp76m2 + dp22m1/afj(1)**thrd                                   !/*ga         42*/
          beta = (dp212m2/afj(1)**twthrd - dp5m2)/alfa                          !/*ga         43*/
        else                                                                    !/*ga         44*/
          alfa = dp1 + cc(j)                                                    !/*ga         45*/
          beta = dp0                                                            !/*ga         46*/
        endif                                                                   !/*ga         47*/
        q1 = am*afj(j)                                                          !/*ga         48*/
        q2 = q1*rj(j)                                                           !/*ga         49*/
        q3 = (gam(j)*afj(j)**twthrd)*(alfa/q1**2)                               !/*ga         50*/
        q5 = dp2*beta*q1 - dp3                                                  !/*ga         51*/
        q4 = q5/dp2 + q2                                                        !/*ga         52*/
        q5 = q5*(sqrt(q2) - dph) + dp2*q2                                       !/*ga         53*/
        gameqf = q3*(q4*exp(-per) + q5*exp(dp2*sqrt(q2) - per))                 !/*ga         54*/
      else                                                                      !/*ga         55*/
        q2 = dp334m1/(pi*radncl**2)                                             !/*ga         56*/
        f1 = q2/(amf*afj(j))                                                    !/*ga         57*/
        if (nhump.gt.1) then                                                    !/*ga         58*/
          rja = rj(7)                                                           !/*ga         59*/
!         rjb = rja + bfa - bfb                                                 !/*ga         60*/
          rjb = rja                                                             !/*ga         61*/
          q1 = dp2*sqrt(amf*afj(7)*rja)                                         !/*ga         62*/
          gameq1 = f1*((q1 - dp1)*exp(q1 - per) + exp(-per))                    !/*ga         63*/
          q1 = dp2*sqrt(amf*afj(7)*rjb)                                         !/*ga         64*/
          gameq2 = f1*((q1 - dp1)*exp(q1 - per) + exp(-per))                    !/*ga         65*/
          gameqf = gameq1*gameq2/(gameq1 + gameq2)                              !/*ga         66*/
        else                                                                    !/*ga         67*/
          q1 = dp2*sqrt(amf*afj(7)*rj(7))                                       !/*ga         68*/
          gameqf = f1*((q1 - dp1)*exp(q1 - per) + exp(-per))                    !/*ga         69*/
        endif                                                                   !/*ga         70*/
      endif                                                                     !/*ga         71*/
#endif                                                                          /*!ga         72*/
      return                                                                    !/*ga         73*/
      end                                                                       !/*ga         74*/

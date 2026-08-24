      function qints (x, lq)                                                    !/*qi          1*/
! ======================================================================        !/*qi          2*/
!                                                                               !/*qi          3*/
!      Parabolic interpolation of tabulated cross sections.                     !/*qi          4*/
!      For lq = 1-4 and 15-19, use a parabola in log(sigma) vs. log(E)          !/*qi          5*/
!      for energies (x) less than the 2nd point.                                !/*qi          6*/
!     Note:                                                                     !/*qi          7*/
!     (Only lq values from 1-19, 26, 27 are used in the rest of the             !/*qi          8*/
!      program.)                                                                !/*qi          9*/
!                                                                               !/*qi         10*/
!   Called by: BINEL ISOBAR SIGMAT                                              !/*qi         11*/
!                                                                               !/*qi         12*/
!   CEM95 written by S. G. Mashnik                                              !/*qi         13*/
!                                                                               !/*qi         14*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*qi         15*/
!   Modified for new cross section tables, June, 1997 (AJS)                     !/*qi         16*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*qi         17*/
!                                                                               !/*qi         18*/
! ======================================================================        !/*qi         19*/
#include <cem/cdouble.h>                                                        /*!qi         20*/
#ifndef CEM                                                                     /*!qi         21*/
      qints=dp0                                                                 !/*qi         22*/
#else                                                                           /*!qi         23*/
      logical ilog                                                              !/*qi         24*/
      common /tablcs/ sigma(31,28), argus(31,6)                                 !/*qi         25*/
! ======================================================================        !/*qi         26*/
      lpmax = 30                                                                !/*qi         27*/
      ilog = .false.                                                            !/*qi         28*/
      if (lq.le.4) then                                                         !/*qi         29*/
!  1-4                                                                          !/*qi         30*/
        i = 1                                                                   !/*qi         31*/
        ilog = .true.                                                           !/*qi         32*/
      elseif (lq.le.10) then                                                    !/*qi         33*/
!  5-10                                                                         !/*qi         34*/
        i = 2                                                                   !/*qi         35*/
        lpmax = 31                                                              !/*qi         36*/
      elseif (lq.le.14) then                                                    !/*qi         37*/
!  11-14                                                                        !/*qi         38*/
        i = 3                                                                   !/*qi         39*/
        lpmax = 31                                                              !/*qi         40*/
      elseif (lq.le.19) then                                                    !/*qi         41*/
!  15-19                                                                        !/*qi         42*/
        i = 3                                                                   !/*qi         43*/
        lpmax = 31                                                              !/*qi         44*/
        ilog = .true.                                                           !/*qi         45*/
      elseif (lq.le.21 .or. lq.eq.28) then                                      !/*qi         46*/
!  20, 21, 28                                                                   !/*qi         47*/
        i = 4                                                                   !/*qi         48*/
!  22                                                                           !/*qi         49*/
      elseif (lq.eq.22) then                                                    !/*qi         50*/
        i = 5                                                                   !/*qi         51*/
!  23-27                                                                        !/*qi         52*/
      elseif (lq.le.27) then                                                    !/*qi         53*/
        i = 6                                                                   !/*qi         54*/
      else                                                                      !/*qi         55*/
        print 1000, lq                                                          !/*qi         56*/
        call exit(0)                                                            !/*qi         57*/
      endif                                                                     !/*qi         58*/
      lpha = 1                                                                  !/*qi         59*/
   10 continue                                                                  !/*qi         60*/
      if (x.eq.argus(lpha,i)) then                                              !/*qi         61*/
!  If x = table value, set cross section to tabulated one.                      !/*qi         62*/
        qints = sigma(lpha,lq)                                                  !/*qi         63*/
        return                                                                  !/*qi         64*/
      elseif (x.gt.argus(lpha,i)) then                                          !/*qi         65*/
!  Increment lpha until x is <= tabulated x.                                    !/*qi         66*/
        if (lpha.ge.lpmax-1) then                                               !/*qi         67*/
          lpha = lpmax - 1                                                      !/*qi         68*/
          go to 20                                                              !/*qi         69*/
        else                                                                    !/*qi         70*/
          lpha = lpha + 1                                                       !/*qi         71*/
          go to 10                                                              !/*qi         72*/
        endif                                                                   !/*qi         73*/
      elseif (x.lt.argus(lpha,i)) then                                          !/*qi         74*/
        if (lpha.le.1 .and. .not.ilog .and. i.eq.3) then                        !/*qi         75*/
!  If x < first table value and not using parabolic log interpolation;          !/*qi         76*/
!  below threshold; set cross section to 0.                                     !/*qi         77*/
          qints = dp0                                                           !/*qi         78*/
          return                                                                !/*qi         79*/
        elseif (lpha.le.1 .and. i.eq.2) then                                    !/*qi         80*/
!  For pi+N reactions with energy < 0; use 0-energy cross section:              !/*qi         81*/
          qints = sigma(1,lq)                                                   !/*qi         82*/
          return                                                                !/*qi         83*/
        else                                                                    !/*qi         84*/
          lpha = max(2,lpha)                                                    !/*qi         85*/
          lpha = min(lpmax-1,lpha)                                              !/*qi         86*/
        endif                                                                   !/*qi         87*/
      endif                                                                     !/*qi         88*/
   20 phi1 = sigma(lpha-1,lq)                                                   !/*qi         89*/
      psi1 = argus(lpha-1,i)                                                    !/*qi         90*/
      phi2 = sigma(lpha,lq)                                                     !/*qi         91*/
      psi2 = argus(lpha,i)                                                      !/*qi         92*/
      phi3 = sigma(lpha+1,lq)                                                   !/*qi         93*/
      psi3 = argus(lpha+1,i)                                                    !/*qi         94*/
      if (ilog .and. lpha.eq.2) then                                            !/*qi         95*/
        x = log(x)                                                              !/*qi         96*/
        phi1 = log(sigma(lpha-1,lq))                                            !/*qi         97*/
        psi1 = log(max(argus(lpha-1,i), dp1m6))                                 !/*qi         98*/
        phi2 = log(sigma(lpha,lq))                                              !/*qi         99*/
        psi2 = log(argus(lpha,i))                                               !/*qi        100*/
        phi3 = log(sigma(lpha+1,lq))                                            !/*qi        101*/
        psi3 = log(argus(lpha+1,i))                                             !/*qi        102*/
      endif                                                                     !/*qi        103*/
      a = psi2 - psi3                                                           !/*qi        104*/
      b = psi3 - psi1                                                           !/*qi        105*/
      c = psi1 - psi2                                                           !/*qi        106*/
      delta = a*psi1**2 + b*psi2**2 + c*psi3**2                                 !/*qi        107*/
      deltaa = phi1*a + phi2*b + phi3*c                                         !/*qi        108*/
      deltab = (phi2 - phi3)*psi1**2 + (phi3 - phi1)*psi2**2 +                  !/*qi        109*/
     &         (phi1 - phi2)*psi3**2                                            !/*qi        110*/
      deltac = (psi2*phi3 - psi3*phi2)*psi1**2 +                                !/*qi        111*/
     &         (psi3*phi1 - psi1*phi3)*psi2**2 +                                !/*qi        112*/
     &         (psi1*phi2 - psi2*phi1)*psi3**2                                  !/*qi        113*/
      fact = dp0                                                                !/*qi        114*/
      if (delta.ne.dp0) fact = dp1/delta                                        !/*qi        115*/
      a = deltaa*fact                                                           !/*qi        116*/
      b = deltab*fact                                                           !/*qi        117*/
      c = deltac*fact                                                           !/*qi        118*/
      qints = a*x**2 + b*x + c                                                  !/*qi        119*/
      if (ilog .and. lpha.eq.2) then                                            !/*qi        120*/
        x = exp(x)                                                              !/*qi        121*/
        qints = exp(qints)                                                      !/*qi        122*/
      else                                                                      !/*qi        123*/
        qints = max(qints, dp0)                                                 !/*qi        124*/
      endif                                                                     !/*qi        125*/
! ======================================================================        !/*qi        126*/
 1000 format (1x,'Qints called with improper value of lq = ',i4)                !/*qi        127*/
! ======================================================================        !/*qi        128*/
#endif                                                                          /*!qi        129*/
      return                                                                    !/*qi        130*/
      end                                                                       !/*qi        131*/

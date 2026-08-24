      subroutine direct (v, u, tin1, mb, mv, np, partin, kp, ith)               !/*di          1*/
#ifdef CEM                                                                      /*!di          2*/
! ======================================================================        !/*di          3*/
!                                                                               !/*di          4*/
!     Determines direction of secondary particle motion.                        !/*di          5*/
!                                                                               !/*di          6*/
!     kp = 0 denotes a  satisfactory termination.                               !/*di          7*/
!     kp = 2 denotes an unsatisfactory termination.                             !/*di          8*/
!                                                                               !/*di          9*/
!   Called by: BINEL                                                            !/*di         10*/
!                                                                               !/*di         11*/
!   Calls: COSTA JTYPEA ROTOR                                                   !/*di         12*/
!                                                                               !/*di         13*/
!   CEM95 written by S. G. Mashnik                                              !/*di         14*/
!                                                                               !/*di         15*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*di         16*/
!   Edited by AJS, July, 1997.                                                  !/*di         17*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*di         18*/
!                                                                               !/*di         19*/
! ======================================================================        !/*di         20*/
!                                                                               !/*di         21*/
!  Definition of partin (pmemo is similar):                                     !/*di         22*/
!                       partin(1); x coordinate of particle                     !/*di         23*/
!                       partin(2); y coordinate of particle                     !/*di         24*/
!                       partin(3); z coordinate of particle                     !/*di         25*/
!                       partin(4); sin(theta), direction of momentum            !/*di         26*/
!                       partin(5); cos(theta), direction of momentum            !/*di         27*/
!                       partin(6); sin(phi), direction of momentum              !/*di         28*/
!                       partin(7); cos(phi), direction of momentum              !/*di         29*/
!                       partin(8); kinetic energy of particle                   !/*di         30*/
!                       partin(9); rest mass of particle                        !/*di         31*/
!                                                                               !/*di         32*/
! ======================================================================        !/*di         33*/
#include <cem/cdouble.h>                                                        /*!di         34*/
      dimension pmemo(9,150), imemo(5,150), v(3), pakv(3), partin(9),           !/*di         35*/
     &          plst(3), pl(3), pakst(3), pin(3)                                !/*di         36*/
      common /memory/ pmemo, imemo                                              !/*di         37*/
      common /pii/    pi                                                        !/*di         38*/
! ======================================================================        !/*di         39*/
      twpi = dp2*pi                                                             !/*di         40*/
      nd = 0                                                                    !/*di         41*/
      kp = 0                                                                    !/*di         42*/
      st = rang()                                                               !/*di         43*/
      if (mb.gt.1) then                                                         !/*di         44*/
!   Two baryons:                                                                !/*di         45*/
        if (st.lt.dph) then                                                     !/*di         46*/
          m1 = 1                                                                !/*di         47*/
          m2 = 3                                                                !/*di         48*/
        else                                                                    !/*di         49*/
          s2 = rang()                                                           !/*di         50*/
          if (s2.lt.dph) then                                                   !/*di         51*/
            m1 = 2                                                              !/*di         52*/
            m2 = 3                                                              !/*di         53*/
          else                                                                  !/*di         54*/
            m1 = 1                                                              !/*di         55*/
            m2 = 2                                                              !/*di         56*/
          endif                                                                 !/*di         57*/
        endif                                                                   !/*di         58*/
      else                                                                      !/*di         59*/
!   One baryon:                                                                 !/*di         60*/
        if (st.lt.dph) then                                                     !/*di         61*/
          m1 = 2                                                                !/*di         62*/
          m2 = 3                                                                !/*di         63*/
        else                                                                    !/*di         64*/
          s2 = rang()                                                           !/*di         65*/
          if (s2.lt.dph) then                                                   !/*di         66*/
            m1 = 1                                                              !/*di         67*/
            m2 = 3                                                              !/*di         68*/
          else                                                                  !/*di         69*/
            m1 = 1                                                              !/*di         70*/
            m2 = 2                                                              !/*di         71*/
          endif                                                                 !/*di         72*/
        endif                                                                   !/*di         73*/
      endif                                                                     !/*di         74*/
   20 lambda = 1                                                                !/*di         75*/
      pakv(1) = dp0                                                             !/*di         76*/
      pakv(2) = dp0                                                             !/*di         77*/
      pakv(3) = dp0                                                             !/*di         78*/
      m1temp = mv + m1                                                          !/*di         79*/
      m2temp = mv + m2                                                          !/*di         80*/
   30 ltemp = mv + lambda                                                       !/*di         81*/
      if (lambda.ne.m1 .and. lambda.ne.m2) then                                 !/*di         82*/
        ja = jtypa (ith, mb, lambda)                                            !/*di         83*/
        ctl = costa (ja, tin1)                                                  !/*di         84*/
        fl = twpi*rang()                                                        !/*di         85*/
        stl = sqrt(dp1 - ctl**2)                                                !/*di         86*/
        temp1 = cos(fl)                                                         !/*di         87*/
        temp2 = sin(fl)                                                         !/*di         88*/
!   pmemo(4,  ) temporarily contains momentum magnitude:                        !/*di         89*/
        temp3 = pmemo(4, ltemp)                                                 !/*di         90*/
        pmemo(1,ltemp) = temp3*stl*temp1                                        !/*di         91*/
        pmemo(2,ltemp) = temp3*stl*temp2                                        !/*di         92*/
        pmemo(3,ltemp) = temp3*ctl                                              !/*di         93*/
        pakv(1) = pakv(1) + pmemo(1,ltemp)                                      !/*di         94*/
        pakv(2) = pakv(2) + pmemo(2,ltemp)                                      !/*di         95*/
        pakv(3) = pakv(3) + pmemo(3,ltemp)                                      !/*di         96*/
      endif                                                                     !/*di         97*/
      if (lambda.lt.np) then                                                    !/*di         98*/
        lambda = lambda + 1                                                     !/*di         99*/
        go to 30                                                                !/*di        100*/
      endif                                                                     !/*di        101*/
      pakvm = sqrt(pakv(1)**2 + pakv(2)**2 + pakv(3)**2)                        !/*di        102*/
      if (np.ne.3) then                                                         !/*di        103*/
        if (pmemo(4,m1temp).ge.(pakvm + pmemo(4,m2temp)))                       !/*di        104*/
     &        go to 50                                                          !/*di        105*/
        if (pmemo(4,m1temp).le.abs(pakvm - pmemo(4,m2temp)))                    !/*di        106*/
     &        go to 50                                                          !/*di        107*/
      endif                                                                     !/*di        108*/
      v2 = v(1)**2 + v(2)**2 + v(3)**2                                          !/*di        109*/
      temp4 = sqrt(partin(8)*(partin(8) + dp2*partin(9)))                       !/*di        110*/
      spv = temp4*partin(4)*partin(7)*v(1) + temp4*partin(5)*v(3) +             !/*di        111*/
     &      temp4*partin(4)*partin(6)*v(2)                                      !/*di        112*/
      temp6 = sqrt(dp1 - v2)                                                    !/*di        113*/
      temp5 = spv*(dp1/temp6 - dp1)/v2                                          !/*di        114*/
      temp6 = (partin(8) + partin(9))/temp6                                     !/*di        115*/
      temp3 = temp5 - temp6                                                     !/*di        116*/
      pin(1) = temp4*partin(4)*partin(7) + v(1)*temp3                           !/*di        117*/
      pin(2) = temp4*partin(4)*partin(6) + v(2)*temp3                           !/*di        118*/
      pin(3) = temp4*partin(5)+v(3)*temp3                                       !/*di        119*/
      lambda = 1                                                                !/*di        120*/
   40 ltemp = mv + lambda                                                       !/*di        121*/
      if (lambda.ne.m1 .and. lambda.ne.m2) then                                 !/*di        122*/
        pl(1) = pmemo(1,ltemp)                                                  !/*di        123*/
        pl(2) = pmemo(2,ltemp)                                                  !/*di        124*/
        pl(3) = pmemo(3,ltemp)                                                  !/*di        125*/
        call rotor (pin, v, pl, plst)                                           !/*di        126*/
        pmemo(1,ltemp) = plst(1)                                                !/*di        127*/
        pmemo(2,ltemp) = plst(2)                                                !/*di        128*/
        pmemo(3,ltemp) = plst(3)                                                !/*di        129*/
      endif                                                                     !/*di        130*/
      if (lambda.lt.np) then                                                    !/*di        131*/
        lambda = lambda + 1                                                     !/*di        132*/
        go to 40                                                                !/*di        133*/
      endif                                                                     !/*di        134*/
      call rotor (pin, v, pakv, pakst)                                          !/*di        135*/
      ctm1 = (pmemo(4,m2temp)**2 - pmemo(4,m1temp)**2 - pakvm**2)/              !/*di        136*/
     &        (dp2*pakvm*pmemo(4,m1temp))                                       !/*di        137*/
      ctm2 = (pmemo(4,m1temp)**2 - pmemo(4,m2temp)**2 - pakvm**2)/              !/*di        138*/
     &        (dp2*pakvm*pmemo(4,m2temp))                                       !/*di        139*/
      fm1 = twpi*rang()                                                         !/*di        140*/
      fm2 = pi + fm1                                                            !/*di        141*/
      stm1 = sqrt(dp1 - ctm1**2)                                                !/*di        142*/
      stm2 = sqrt(dp1 - ctm2**2)                                                !/*di        143*/
      cfm1 = cos(fm1)                                                           !/*di        144*/
      sfm1 = sin(fm1)                                                           !/*di        145*/
      cfm2 = cos(fm2)                                                           !/*di        146*/
      sfm2 = sin(fm2)                                                           !/*di        147*/
      pl(1) = pmemo(4,m1temp)*stm1*cfm1                                         !/*di        148*/
      pl(2) = pmemo(4,m1temp)*stm1*sfm1                                         !/*di        149*/
      pl(3) = pmemo(4,m1temp)*ctm1                                              !/*di        150*/
      call rotor (pakst, v, pl, plst)                                           !/*di        151*/
      pmemo(1,m1temp) = plst(1)                                                 !/*di        152*/
      pmemo(2,m1temp) = plst(2)                                                 !/*di        153*/
      pmemo(3,m1temp) = plst(3)                                                 !/*di        154*/
      pl(1) = pmemo(4,m2temp)*stm2*cfm2                                         !/*di        155*/
      pl(2) = pmemo(4,m2temp)*stm2*sfm2                                         !/*di        156*/
      pl(3) = pmemo(4,m2temp)*ctm2                                              !/*di        157*/
      call rotor (pakst, v, pl, plst)                                           !/*di        158*/
      pmemo(1,m2temp) = plst(1)                                                 !/*di        159*/
      pmemo(2,m2temp) = plst(2)                                                 !/*di        160*/
      pmemo(3,m2temp) = plst(3)                                                 !/*di        161*/
      return                                                                    !/*di        162*/
   50 nd = nd + 1                                                               !/*di        163*/
      if (nd.lt.100) then                                                       !/*di        164*/
        go to 20                                                                !/*di        165*/
      else                                                                      !/*di        166*/
        kp = 2                                                                  !/*di        167*/
      endif                                                                     !/*di        168*/
#endif                                                                          /*!di        169*/
      return                                                                    !/*di        170*/
      end                                                                       !/*di        171*/

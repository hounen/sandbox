      subroutine vmnsp(partin,ipatin,partne,u,mv,np,ith,mb,tin1,lp)             !/*vm          1*/
#ifdef CEM                                                                      /*!vm          2*/
! ======================================================================        !/*vm          3*/
!                                                                               !/*vm          4*/
!     Calculate secondary particle number and determine absolute                !/*vm          5*/
!     values of momenta in inelastic (pion production) interaction.             !/*vm          6*/
!                                                                               !/*vm          7*/
!   Called by: BINEL                                                            !/*vm          8*/
!                                                                               !/*vm          9*/
!   Calls: JTYPB PMOM                                                           !/*vm         10*/
!                                                                               !/*vm         11*/
!   CEM95 written by S. G. Mashnik                                              !/*vm         12*/
!                                                                               !/*vm         13*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*vm         14*/
!   Edited by AJS, July-August, 1997.                                           !/*vm         15*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*vm         16*/
!   Edited by F. Gallmeier, ORNL CPED, December 1998.                           !/*vm         17*/
!                                                                               !/*vm         18*/
! ======================================================================        !/*vm         19*/
!                                                                               !/*vm         20*/
!  np is the number of particles in the final state; set to 0 if the            !/*vm         21*/
!  block "memory" is overfilled.                                                !/*vm         22*/
!                                                                               !/*vm         23*/
!  Definition of partin (pmemo similar):                                        !/*vm         24*/
!                       partin(1); Normalized x coordinate of particle          !/*vm         25*/
!                       partin(2); Normalized y coordinate of particle          !/*vm         26*/
!                       partin(3); Normalized z coordinate of particle          !/*vm         27*/
!                       partin(4); sin(theta), direction of momentum            !/*vm         28*/
!                       partin(5); cos(theta), direction of momentum            !/*vm         29*/
!                       partin(6); sin(phi), direction of momentum              !/*vm         30*/
!                       partin(7); cos(phi), direction of momentum              !/*vm         31*/
!                       partin(8); kinetic energy of particle                   !/*vm         32*/
!                       partin(9); rest mass of particle                        !/*vm         33*/
!                                                                               !/*vm         34*/
!   pmemo(4,ltemp) is temporarily the total momentum of the particle.           !/*vm         35*/
!                                                                               !/*vm         36*/
!  Definition of ipatin (imemo similar):                                        !/*vm         37*/
!                       ipatin(1); charge of particle                           !/*vm         38*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*vm         39*/
!                       ipatin(3); strangeness of particle                      !/*vm         40*/
!                       ipatin(4); particle baryon number                       !/*vm         41*/
!                       ipatin(5); zone number of nucleus where particle        !/*vm         42*/
!                                  is located.                                  !/*vm         43*/
!                                                                               !/*vm         44*/
! ======================================================================        !/*vm         45*/
#include <cem/cdouble.h>                                                        /*!vm         46*/
      dimension pmemo(9,150), imemo(5,150), partin(9), ipatin(5)                !/*vm         47*/
      dimension partne(9)                                                       !/*vm         48*/
      common /memory/ pmemo, imemo                                              !/*vm         49*/
      common /mnuc/   emnucm, emnucg, emnucb, emneut, emprot                    !/*vm         50*/
      common /mpi/    empich, empi0                                             !/*vm         51*/
! ======================================================================        !/*vm         52*/
      kh = 0                                                                    !/*vm         53*/
      lp = 0                                                                    !/*vm         54*/
   10 u1 = u                                                                    !/*vm         55*/
      lambda = 1                                                                !/*vm         56*/
   20 ltemp = mv + lambda                                                       !/*vm         57*/
      if (ltemp.gt.150) then                                                    !/*vm         58*/
        np = 0                                                                  !/*vm         59*/
        call expirx(1,'VMNSP','memory exceeded in cascade.')                    !/*vm         60*/
        return                                                                  !/*vm         61*/
      endif                                                                     !/*vm         62*/
      if (lambda.le.1) then                                                     !/*vm         63*/
        pmemo(9,mv+1) = partne(9)                                               !/*vm         64*/
        imemo(2,mv+1) = 0                                                       !/*vm         65*/
        imemo(3,mv+1) = 0                                                       !/*vm         66*/
        imemo(4,mv+1) = 1                                                       !/*vm         67*/
      elseif (lambda.eq.3) then                                                 !/*vm         68*/
        pmemo(9,mv+3) = partin(9)                                               !/*vm         69*/
        imemo(2,mv+3) = 0                                                       !/*vm         70*/
        imemo(3,mv+3) = ipatin(3)                                               !/*vm         71*/
        imemo(4,mv+3) = ipatin(4)                                               !/*vm         72*/
      else                                                                      !/*vm         73*/
        pmemo(9,ltemp) = empich                                                 !/*vm         74*/
        imemo(2,ltemp) = 0                                                      !/*vm         75*/
        imemo(3,ltemp) = 0                                                      !/*vm         76*/
        imemo(4,ltemp) = 0                                                      !/*vm         77*/
      endif                                                                     !/*vm         78*/
      jb = jtypb (ith, mb, lambda)                                              !/*vm         79*/
      pmemo(4,ltemp) = pmom (jb, tin1)                                          !/*vm         80*/
      el = sqrt(pmemo(4,ltemp)**2 + pmemo(9,ltemp)**2)                          !/*vm         81*/
      deltu = u1 - el                                                           !/*vm         82*/
      if (lambda.eq.2) then                                                     !/*vm         83*/
        if (deltu.le.partin(9)) then                                            !/*vm         84*/
          kh = kh + 1                                                           !/*vm         85*/
          if (kh.lt.100) go to 10                                               !/*vm         86*/
          lp = 2                                                                !/*vm         87*/
          return                                                                !/*vm         88*/
        endif                                                                   !/*vm         89*/
        if (ith.ne.0) then                                                      !/*vm         90*/
          pmemo(4,mv+3) = sqrt(deltu**2 - partin(9)**2)                         !/*vm         91*/
          pmemo(9,mv+3) = partin(9)                                             !/*vm         92*/
          imemo(2,mv+3) = 0                                                     !/*vm         93*/
          imemo(3,mv+3) = ipatin(3)                                             !/*vm         94*/
          imemo(4,mv+3) = ipatin(4)                                             !/*vm         95*/
          if (pmemo(4,mv+1)-pmemo(4,mv+2)-pmemo(4,mv+3).le.dp0) then            !/*vm         96*/
            if (pmemo(4,mv+1).gt.abs(pmemo(4,mv+2)-pmemo(4,mv+3)))              !/*vm         97*/
     &      then                                                                !/*vm         98*/
              np = 3                                                            !/*vm         99*/
              return                                                            !/*vm        100*/
            endif                                                               !/*vm        101*/
          endif                                                                 !/*vm        102*/
          kh = kh + 1                                                           !/*vm        103*/
          if (kh.lt.100) go to 10                                               !/*vm        104*/
          lp = 2                                                                !/*vm        105*/
          return                                                                !/*vm        106*/
        endif                                                                   !/*vm        107*/
        u1 = deltu                                                              !/*vm        108*/
        lambda = lambda + 1                                                     !/*vm        109*/
        go to 20                                                                !/*vm        110*/
      endif                                                                     !/*vm        111*/
      if (deltu.gt.empich) then                                                 !/*vm        112*/
        u1 = deltu                                                              !/*vm        113*/
        lambda = lambda + 1                                                     !/*vm        114*/
        go to 20                                                                !/*vm        115*/
      endif                                                                     !/*vm        116*/
      if (lambda.le.1 .or. lambda.eq.3) then                                    !/*vm        117*/
        kh = kh + 1                                                             !/*vm        118*/
        if (kh.lt.100) go to 10                                                 !/*vm        119*/
        lp = 2                                                                  !/*vm        120*/
        return                                                                  !/*vm        121*/
      endif                                                                     !/*vm        122*/
      el = deltu + el                                                           !/*vm        123*/
      pmemo(4,ltemp) = sqrt(el**2 - pmemo(9,ltemp)**2)                          !/*vm        124*/
      np = lambda                                                               !/*vm        125*/
      i = 1                                                                     !/*vm        126*/
   30 itemp = mv + i                                                            !/*vm        127*/
      c = pmemo(4,itemp)                                                        !/*vm        128*/
   40 if (np.gt.i) then                                                         !/*vm        129*/
        i = i + 1                                                               !/*vm        130*/
        if (c.ge.pmemo(4,itemp+1)) then                                         !/*vm        131*/
          go to 40                                                              !/*vm        132*/
        else                                                                    !/*vm        133*/
          go to 30                                                              !/*vm        134*/
        endif                                                                   !/*vm        135*/
      endif                                                                     !/*vm        136*/
      pmax = c                                                                  !/*vm        137*/
      sigma = dp0                                                               !/*vm        138*/
        do 50 i = 1,np                                                          !/*vm        139*/
        itemp = mv + i                                                          !/*vm        140*/
        sigma = sigma + pmemo(4,itemp)                                          !/*vm        141*/
   50   continue                                                                !/*vm        142*/
      if (dp2*pmax.ge.sigma) then                                               !/*vm        143*/
        kh = kh + 1                                                             !/*vm        144*/
        if (kh.lt.100) go to 10                                                 !/*vm        145*/
        lp = 2                                                                  !/*vm        146*/
        return                                                                  !/*vm        147*/
      endif                                                                     !/*vm        148*/
#endif                                                                          /*!vm        149*/
      return                                                                    !/*vm        150*/
      end                                                                       !/*vm        151*/

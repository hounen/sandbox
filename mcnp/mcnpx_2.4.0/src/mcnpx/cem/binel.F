      subroutine binel (partin, ipatin, partne, ipatne, l, ms, mb,              !/*bi          1*/
     &                  ksi, me, v, u, tin1, mv, np, nin, z, a)                 !/*bi          2*/
#ifdef CEM                                                                      /*!bi          3*/
! ======================================================================        !/*bi          4*/
!                                                                               !/*bi          5*/
!     Inelastic interaction (pion production!) subroutine.                      !/*bi          6*/
!                                                                               !/*bi          7*/
!   Called by: TYPINT                                                           !/*bi          8*/
!                                                                               !/*bi          9*/
!   Calls: CHINEL DIRECT ISOBAR SIGMAT VMNSP                                    !/*bi         10*/
!                                                                               !/*bi         11*/
!     np = number of particles (in final state?),                               !/*bi         12*/
!     nin = (???),                                                              !/*bi         13*/
!     mv = last index filled in pmemo, imemo arrays.                            !/*bi         14*/
!                                                                               !/*bi         15*/
!   CEM95 written by S. G. Mashnik                                              !/*bi         16*/
!                                                                               !/*bi         17*/
!   Edited by A. J. Sierk,  LANL T-2, February, 1996.                           !/*bi         18*/
!   Edited by AJS, August-September, 1997.                                      !/*bi         19*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*bi         20*/
!                                                                               !/*bi         21*/
! ======================================================================        !/*bi         22*/
!                                                                               !/*bi         23*/
!  Definition of partin (partne similar for 2nd particle):                      !/*bi         24*/
!                       partin(1); x coordinate of particle                     !/*bi         25*/
!                       partin(2); y coordinate of particle                     !/*bi         26*/
!                       partin(3); z coordinate of particle                     !/*bi         27*/
!                       partin(4); sin(theta), direction of momentum            !/*bi         28*/
!                       partin(5); cos(theta), direction of momentum            !/*bi         29*/
!                       partin(6); sin(phi), direction of momentum              !/*bi         30*/
!                       partin(7); cos(phi), direction of momentum              !/*bi         31*/
!                       partin(8); kinetic energy of particle                   !/*bi         32*/
!                       partin(9); rest mass of particle                        !/*bi         33*/
!                                                                               !/*bi         34*/
!  Definition of ipatin (ipatne similar for 2nd particle):                      !/*bi         35*/
!                       ipatin(1); charge of particle                           !/*bi         36*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*bi         37*/
!                       ipatin(3); strangeness of particle                      !/*bi         38*/
!                       ipatin(4); particle baryon number                       !/*bi         39*/
!                       ipatin(5); zone number of nucleus where particle        !/*bi         40*/
!                                  is located.                                  !/*bi         41*/
!                                                                               !/*bi         42*/
! ======================================================================        !/*bi         43*/
#include <cem/cdouble.h>                                                        /*!bi         44*/
      dimension partin(9), ipatin(5), partne(9), ipatne(5), v(3)                !/*bi         45*/
      common /mpi/ empich, empi0                                                !/*bi         46*/
! ======================================================================        !/*bi         47*/
      nin = 0                                                                   !/*bi         48*/
      ik = 0                                                                    !/*bi         49*/
      if (tin1.ge.dp20) then                                                    !/*bi         50*/
        betath = dp0                                                            !/*bi         51*/
      else                                                                      !/*bi         52*/
! (9/11/97) This means total pion production cross section                      !/*bi         53*/
!   divided by total inelastic cross section. [total - elastic -                !/*bi         54*/
!   SCX]                                                                        !/*bi         55*/
        betath = sigmat(l, ms, mb, ksi, 7, tin1)/                               !/*bi         56*/
     &          (sigmat(l, ms, mb, ksi, 0, tin1) -                              !/*bi         57*/
     &           sigmat(l, ms, mb, ksi, 1, tin1) -                              !/*bi         58*/
     &           sigmat(l, ms, mb, ksi, 2, tin1))                               !/*bi         59*/
      endif                                                                     !/*bi         60*/
      b1 = rang()                                                               !/*bi         61*/
      if (b1.lt.betath) then                                                    !/*bi         62*/
!   One pion produced.                                                          !/*bi         63*/
        ith = 1                                                                 !/*bi         64*/
        th = dp1                                                                !/*bi         65*/
      else                                                                      !/*bi         66*/
!   Two pions produced (??).                                                    !/*bi         67*/
        ith = 0                                                                 !/*bi         68*/
        th = dp2                                                                !/*bi         69*/
      endif                                                                     !/*bi         70*/
      if ((u - partin(9) - empich*th) .gt. dp96m2) then                         !/*bi         71*/
   10   call vmnsp(partin,ipatin,partne,u,mv,np,ith,mb,tin1,lp)                 !/*bi         72*/
        if (np.le.0) return                                                     !/*bi         73*/
        if (lp.eq.0) then                                                       !/*bi         74*/
          call direct (v, u, tin1, mb, mv, np, partin, kp, ith)                 !/*bi         75*/
          if (kp.ne.0) then                                                     !/*bi         76*/
            ik = ik + 1                                                         !/*bi         77*/
            if (ik.ge.50) then                                                  !/*bi         78*/
              nin = 2                                                           !/*bi         79*/
            else                                                                !/*bi         80*/
              go to 10                                                          !/*bi         81*/
            endif                                                               !/*bi         82*/
          else                                                                  !/*bi         83*/
!   Find charges of outgoing particles:                                         !/*bi         84*/
            call chinel (ipatin, l, ms, mb, ksi, np, mv, tin1, me,              !/*bi         85*/
     &                   ipatne, z, a)                                          !/*bi         86*/
          endif                                                                 !/*bi         87*/
        else                                                                    !/*bi         88*/
          nin = 2                                                               !/*bi         89*/
        endif                                                                   !/*bi         90*/
      else                                                                      !/*bi         91*/
        nin = 2                                                                 !/*bi         92*/
      endif                                                                     !/*bi         93*/
#endif                                                                          /*!bi         94*/
      return                                                                    !/*bi         95*/
      end                                                                       !/*bi         96*/

!     $Id: phase.F,v 1.4 2002/03/27 18:15:14 gwm Exp $                        !/*ph          1*/
      function phase (ecm,m3,mi)                                                !/*ph          2*/
#include <lcs/double.h>                                                         /*!ph          3*/
!     ==================================================================        !/*ph          4*/
!     phase calculates the two body phase factor                                !/*ph          5*/
!                                                                               !/*ph          6*/
! PHASE  calls   SQRT                                                           !/*ph          7*/
! called by TWOPIN  ONEPIN  PIN1PI                                              !/*ph          8*/
!     ==================================================================        !/*ph          9*/
!                                                                               !/*ph         10*/
#ifdef CHEAP                                                                    /*!ph         11*/
      double precision m3, mi                                                   !/*ph         12*/
#else                                                                           /*!ph         13*/
      real m3, mi                                                               !/*ph         14*/
#endif                                                                          /*!ph         15*/
      e3=(ecm*ecm+m3*m3-mi*mi)/(dp2*ecm)                                        !/*ph         16*/
      p3sq=e3*e3-m3*m3                                                          !/*ph         17*/
      e4=ecm-e3                                                                 !/*ph         18*/
      if ((e3.le.m3).or.(e4.le.mi)) go to 10                                    !/*ph         19*/
      phase=sqrt(p3sq)*e3*e4/ecm                                                !/*ph         20*/
      return                                                                    !/*ph         21*/
   10 continue                                                                  !/*ph         22*/
      phase=dp0                                                                 !/*ph         23*/
      return                                                                    !/*ph         24*/
      end                                                                       !/*ph         25*/

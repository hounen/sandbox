!     $Id: angid.F,v 1.4 2002/03/27 18:14:54 gwm Exp $                        !/*an          1*/
      subroutine angid                                                          !/*an          2*/
#include <lcs/double.h>                                                         /*!an          3*/
#include <lcs/btcom.h>                                                          /*!an          4*/
!     ******************************************************************        !/*an          5*/
!****    calculates  cos and sin theta,sin and cos phi    **************        !/*an          6*/
!     ******************************************************************        !/*an          7*/
      icurr=curr(1)                                                             !/*an          8*/
      if (icurr.gt.2) go to 20                                                  !/*an          9*/
!****  incident particle - nucleon                                              !/*an         10*/
      if (it.eq.21.or.it.eq.22) go to 10                                        !/*an         11*/
!****  single  production                                                       !/*an         12*/
      if (rlke.gt.35.d+02) call errorf ('angid.1')                              !/*an         13*/
      if (rlke.lt.5.d+02) go to 60                                              !/*an         14*/
      tesiso=0.75d0                                                             !/*an         15*/
      if (rlke.lt.1.d+03) go to 40                                              !/*an         16*/
      tesiso=0.5d0                                                              !/*an         17*/
      if (rlke.lt.13.d+02) go to 40                                             !/*an         18*/
      tesiso=0.25d0                                                             !/*an         19*/
      if (rlke.lt.25.d+02) go to 40                                             !/*an         20*/
      go to 50                                                                  !/*an         21*/
!****  double production                                                        !/*an         22*/
   10 if (rlke.gt.35.d+02) call errorf ('angid.2')                              !/*an         23*/
      go to 50                                                                  !/*an         24*/
!**** incident particle-pion                                                    !/*an         25*/
   20 continue                                                                  !/*an         26*/
      r=rang()                                                                  !/*an         27*/
      if (rlke.gt.25.d+02) call errorf ('angid.3')                              !/*an         28*/
      cst=-0.9999995d0                                                          !/*an         29*/
      snt=0.003162d0                                                            !/*an         30*/
      if (it.ne.11) go to 30                                                    !/*an         31*/
      if (r.le.0.75d0) go to 60                                                 !/*an         32*/
      go to 70                                                                  !/*an         33*/
!****  (pi+)-(p),(pi-)-(n)                                                      !/*an         34*/
!****  (pi0)-(n),(pi0)-(p)                                                      !/*an         35*/
   30 if (it.ne.12.and.it.ne.28) call errorf ('angid.4')                        !/*an         36*/
      if (rlke.lt.5.d+02) cst=-cst                                              !/*an         37*/
      if (r.le.0.80d0) go to 60                                                 !/*an         38*/
      go to 70                                                                  !/*an         39*/
   40 continue                                                                  !/*an         40*/
      r=rang()                                                                  !/*an         41*/
      if (r.le.tesiso) go to 60                                                 !/*an         42*/
!**** backward/forward                                                          !/*an         43*/
   50 continue                                                                  !/*an         44*/
      r=rang()                                                                  !/*an         45*/
!****  test for direction                                                       !/*an         46*/
      cst=0.9999995d0                                                           !/*an         47*/
      snt=0.003162d0                                                            !/*an         48*/
      if (r.le.dph) go to 70                                                    !/*an         49*/
      cst=-0.9999995d0                                                          !/*an         50*/
      go to 70                                                                  !/*an         51*/
!****  isotropic                                                                !/*an         52*/
   60 call polrn (cst,snt)                                                      !/*an         53*/
!****  calculates  cos,sin phi                                                  !/*an         54*/
   70 call azirn (sopc,sops)                                                    !/*an         55*/
      return                                                                    !/*an         56*/
      end                                                                       !/*an         57*/

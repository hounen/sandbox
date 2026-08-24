!     $Id: elas.F,v 1.4 2002/03/27 18:15:01 gwm Exp $                         !/*el          1*/
      subroutine elas (ans)                                                     !/*el          2*/
#include <lcs/double.h>                                                         /*!el          3*/
!     ==================================================================        !/*el          4*/
!     elas does the elastic collision in the center of mass                     !/*el          5*/
!     system as outlined in lams-2360.                                          !/*el          6*/
!                                                                               !/*el          7*/
! ELAS   calls   SQRT  FLTRNF  COS                                              !/*el          8*/
! called by AIOUT  GAMMA                                                        !/*el          9*/
!     ==================================================================        !/*el         10*/
!                                                                               !/*el         11*/
#include <lcs/vegas.h>                                                          /*!el         12*/
      ela(10)=ans                                                               !/*el         13*/
      ela(11)=sqrt(dp1-ela(10)*ela(10))                                         !/*el         14*/
      ela(12)=cm(7)*ela(11)                                                     !/*el         15*/
      ela(19)=cm(9)                                                             !/*el         16*/
      cm(9)=ela(10)*cm(9)+ela(12)                                               !/*el         17*/
      ela(12)=ela(19)*ela(11)                                                   !/*el         18*/
      cm(7)=ela(10)*cm(7)-ela(12)                                               !/*el         19*/
      rand=rang()                                                               !/*el         20*/
      ela(19)=rand*6.28318d0                                                    !/*el         21*/
      ela(13)=cos(ela(19))                                                      !/*el         22*/
      ela(14)=sqrt(dp1-ela(13)*ela(13))                                         !/*el         23*/
      if (ela(19)-dppi) 20,10,10                                                !/*el         24*/
   10 ela(14)=-ela(14)                                                          !/*el         25*/
   20 ela(4)=ela(14)*cm(6)                                                      !/*el         26*/
      ela(2)=-ela(4)                                                            !/*el         27*/
      ela(8)=ela(14)*cm(5)                                                      !/*el         28*/
      ela(6)=-ela(8)                                                            !/*el         29*/
      ela(5)=ela(13)                                                            !/*el         30*/
      ela(12)=dp1-ela(5)                                                        !/*el         31*/
      ela(3)=cm(5)*cm(6)*ela(12)                                                !/*el         32*/
      ela(7)=ela(3)                                                             !/*el         33*/
      ela(1)=cm(5)*cm(5)*ela(12)+ela(13)                                        !/*el         34*/
      ela(9)=cm(6)*cm(6)*ela(12)+ela(13)                                        !/*el         35*/
!                                                                               !/*el         36*/
      ela(16)=cm(7)*ela(1)+cm(8)*ela(2)+cm(9)*ela(3)                            !/*el         37*/
      ela(17)=cm(7)*ela(4)+cm(8)*ela(5)+cm(9)*ela(6)                            !/*el         38*/
      ela(18)=cm(7)*ela(7)+cm(8)*ela(8)+cm(9)*ela(9)                            !/*el         39*/
!                                                                               !/*el         40*/
!       correction added to fix bad error [yy 24jul84]                          !/*el         41*/
!      correct the angles(5,6) pointing to the particles, as                    !/*el         42*/
!      well as the momenta (7,8,9)                                              !/*el         43*/
      ela2=sqrt(ela(16)**2+ela(17)**2+ela(18)**2)                               !/*el         44*/
      cm(5)=ela(16)/ela2                                                        !/*el         45*/
      cm(6)=ela(18)/ela2                                                        !/*el         46*/
!                                                                               !/*el         47*/
      cm(7)=ela(16)                                                             !/*el         48*/
      cm(11)=-ela(16)                                                           !/*el         49*/
      cm(8)=ela(17)                                                             !/*el         50*/
      cm(12)=-ela(17)                                                           !/*el         51*/
      cm(9)=ela(18)                                                             !/*el         52*/
      cm(13)=-ela(18)                                                           !/*el         53*/
      return                                                                    !/*el         54*/
      end                                                                       !/*el         55*/

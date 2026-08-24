!     $Id: cole4.F,v 1.4 2002/03/27 18:14:58 gwm Exp $                        !/*co          1*/
      subroutine cole4                                                          !/*co          2*/
#include <lcs/double.h>                                                         /*!co          3*/
#include <lcs/btcom.h>                                                          /*!co          4*/
      k=clsm                                                                    !/*co          5*/
      com=(e(4)-dncms)/rcpmv                                                    !/*co          6*/
!     k.e. of particle 4 in mev                                                 !/*co          7*/
!     type of partil. 1-5  current nucleon                                      !/*co          8*/
      if (curr(1)-dp3) 120,10,10                                                !/*co          9*/
   10 if (aabs) 20,30,20                                                        !/*co         10*/
   20 com=com+absec                                                             !/*co         11*/
      go to 120                                                                 !/*co         12*/
   30 if (it-5) 40,70,40                                                        !/*co         13*/
   40 if (it-24) 50,70,50                                                       !/*co         14*/
   50 if (it-6) 60,80,60                                                        !/*co         15*/
   60 if (it-26) 120,80,120                                                     !/*co         16*/
   70 univ=dp0                                                                  !/*co         17*/
!     neut with wrong energy                                                    !/*co         18*/
      go to 90                                                                  !/*co         19*/
   80 univ=dp1                                                                  !/*co         20*/
   90 unive=space(k+3)-space(k+9)                                               !/*co         21*/
!     region i n-p well depth difference                                        !/*co         22*/
      if (univ) 110,100,110                                                     !/*co         23*/
  100 com=com+unive                                                             !/*co         24*/
      go to 120                                                                 !/*co         25*/
  110 com=com-unive                                                             !/*co         26*/
  120 return                                                                    !/*co         27*/
      end                                                                       !/*co         28*/

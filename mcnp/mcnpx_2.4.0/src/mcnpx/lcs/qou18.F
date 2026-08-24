!     $Id: qou18.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                        !/*qo          1*/
      subroutine qou18                                                          !/*qo          2*/
! QOU18      calls...  QDK  QSTOR                                               !/*qo          3*/
!     QOU18  called by...   ISOB                                                !/*qo          4*/
#include <lcs/double.h>                                                         /*!qo          5*/
#include <lcs/isobar.h>                                                         /*!qo          6*/
      go to (10,20,80,120,80,100), i3_                                          !/*qo          7*/
   10 i=3                                                                       !/*qo          8*/
      col_(15)=dp1                                                              !/*qo          9*/
      k=27                                                                      !/*qo         10*/
      go to 30                                                                  !/*qo         11*/
   20 i=3                                                                       !/*qo         12*/
      col_(15)=dp4                                                              !/*qo         13*/
      k=15                                                                      !/*qo         14*/
   30 pnidk_(1)=pm_(i)                                                          !/*qo         15*/
      j=i                                                                       !/*qo         16*/
      do 40 l=2,4                                                               !/*qo         17*/
      pnidk_(l)=pxyz_(j)                                                        !/*qo         18*/
   40 j=j+4                                                                     !/*qo         19*/
      pnidk_(5)=e_(i)                                                           !/*qo         20*/
      pnidk_(6)=pt_(k-11)                                                       !/*qo         21*/
      call qdk                                                                  !/*qo         22*/
      if (k-27) 60,50,60                                                        !/*qo         23*/
   50 pt_(15)=pt_(15)+((pnidk_(12)-pnidk_(6))/rcpmv_)                           !/*qo         24*/
   60 pt_(k)=pt_(k)+((pnidk_(13)-dncms_)/rcpmv_)                                !/*qo         25*/
      i3_=1                                                                     !/*qo         26*/
   70 iv_=k                                                                     !/*qo         27*/
      return                                                                    !/*qo         28*/
   80 col_(15)=dp3                                                              !/*qo         29*/
      k=15                                                                      !/*qo         30*/
      if (pt_(14)-dp2) 90,90,120                                                !/*qo         31*/
   90 i3_=2                                                                     !/*qo         32*/
      go to 70                                                                  !/*qo         33*/
  100 l=14                                                                      !/*qo         34*/
      do 110 m=5,7                                                              !/*qo         35*/
      pt_(m)=pnidk_(l)                                                          !/*qo         36*/
      pt_(m+12)=pnidk_(l+3)                                                     !/*qo         37*/
  110 l=l+1                                                                     !/*qo         38*/
      pt_(11)=pnidk_(12)                                                        !/*qo         39*/
      pt_(12)=pnidk_(6)                                                         !/*qo         40*/
      i=4                                                                       !/*qo         41*/
      k=39                                                                      !/*qo         42*/
      col_(15)=dp5                                                              !/*qo         43*/
      go to 30                                                                  !/*qo         44*/
  120 i1_=3                                                                     !/*qo         45*/
  130 k=12*i1_-33                                                               !/*qo         46*/
      if (i1_-4) 140,150,160                                                    !/*qo         47*/
  140 i2_=-1                                                                    !/*qo         48*/
      go to 190                                                                 !/*qo         49*/
  150 i2_=0                                                                     !/*qo         50*/
      go to 190                                                                 !/*qo         51*/
  160 if (i1_-5) 150,180,170                                                    !/*qo         52*/
  170 i3_=4                                                                     !/*qo         53*/
      go to 70                                                                  !/*qo         54*/
  180 i2_=1                                                                     !/*qo         55*/
  190 if (pt_(k)) 200,210,200                                                   !/*qo         56*/
  200 call qstor                                                                !/*qo         57*/
  210 i1_=i1_+1                                                                 !/*qo         58*/
      go to 130                                                                 !/*qo         59*/
      end                                                                       !/*qo         60*/

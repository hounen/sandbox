!     $Id: qoll.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                         !/*qo          1*/
      subroutine qoll                                                           !/*qo          2*/
! QOLL       calls...  SQRT                                                     !/*qo          3*/
!     QOLL   called by...   QOU17                                               !/*qo          4*/
#include <lcs/double.h>                                                         /*!qo          5*/
#include <lcs/isobar.h>                                                         /*!qo          6*/
      a_=pm_(4)*pm_(4)                                                          !/*qo          7*/
      col_(15)=dp0                                                              !/*qo          8*/
      col_(1)=e_(1)+e_(2)                                                       !/*qo          9*/
!     total energy particles 1 and 2                                            !/*qo         10*/
      do 10 i=1,3                                                               !/*qo         11*/
   10 col_(i+1)=pm_(i)*pm_(i)                                                   !/*qo         12*/
!     mass particle i sqd.                                                      !/*qo         13*/
      col_(5)=col_(3)+col_(2)+dp2*(e_(1)*e_(2)-(pxyz_(1)*pxyz_(2)+pxyz_(        !/*qo         14*/
     & 5)*pxyz_(6)+pxyz_(9)*pxyz_(10)))                                         !/*qo         15*/
      col_(6)=sqrt(col_(5))                                                     !/*qo         16*/
      col_(7)=col_(6)/col_(1)                                                   !/*qo         17*/
!     gam                                                                       !/*qo         18*/
      col_(8)=dp2*col_(6)                                                       !/*qo         19*/
      col_(9)=(col_(4)+col_(5)-a_)/col_(8)                                      !/*qo         20*/
      com2_=col_(9)*col_(9)                                                     !/*qo         21*/
      if (col_(4)-2.9882156d+27) 20,20,40                                       !/*qo         22*/
! gt,pm_(3)=isobar--lte,test for roundoff range,(min)sqd+or-5e23                !/*qo         23*/
   20 if (col_(4)-2.9872156d+27) 50,30,30                                       !/*qo         24*/
!     lt,pion or nucleon mass=pm_(3)                                            !/*qo         25*/
   30 col_(4)=2.9877156d+27                                                     !/*qo         26*/
      pm_(3)=5.466005d+13                                                       !/*qo         27*/
   40 if (com2_-col_(4)) 60,80,80                                               !/*qo         28*/
   50 if (col_(4)-sqnm_) 40,40,110                                              !/*qo         29*/
!     lte,have nucleon or pion--gt,go to error                                  !/*qo         30*/
   60 if (com2_-9.9d-1*col_(4)) 80,70,70                                        !/*qo         31*/
   70 com2_=col_(4)                                                             !/*qo         32*/
      col_(9)=pm_(3)                                                            !/*qo         33*/
   80 col_(10)=sqrt(com2_-col_(4))                                              !/*qo         34*/
!     p3 prime                                                                  !/*qo         35*/
      if (ik_) 90,160,90                                                        !/*qo         36*/
   90 col_(11)=(col_(5)+col_(2)-col_(3))/col_(8)                                !/*qo         37*/
!     e1 prime                                                                  !/*qo         38*/
      col_(12)=sqrt(col_(11)*col_(11)-col_(2))                                  !/*qo         39*/
!     p1 prime                                                                  !/*qo         40*/
      col_(13)=(col_(7)*e_(1)-col_(11))/col_(12)                                !/*qo         41*/
!     beta                                                                      !/*qo         42*/
      com_=dp1-(col_(13)*col_(13)+col_(7)*col_(7))                              !/*qo         43*/
      if (com_-5.d-6) 100,140,140                                               !/*qo         44*/
  100 if (com_+5.d-6) 110,130,130                                               !/*qo         45*/
  110 col_(15)=dp1                                                              !/*qo         46*/
!     error                                                                     !/*qo         47*/
  120 return                                                                    !/*qo         48*/
  130 col_(14)=.002236067977d0                                                  !/*qo         49*/
      go to 150                                                                 !/*qo         50*/
  140 col_(14)=sqrt(com_)                                                       !/*qo         51*/
!     alpha                                                                     !/*qo         52*/
  150 e_(3)=(col_(9)+col_(10)*(col_(13)*cst_+col_(14)*sopc_*snt_))/col_(        !/*qo         53*/
     & 7)                                                                       !/*qo         54*/
      e_(4)=col_(1)-e_(3)                                                       !/*qo         55*/
      go to 120                                                                 !/*qo         56*/
  160 do 170 i=11,14                                                            !/*qo         57*/
  170 col_(i)=dp0                                                               !/*qo         58*/
      e_(3)=dp0                                                                 !/*qo         59*/
      e_(4)=dp0                                                                 !/*qo         60*/
      go to 120                                                                 !/*qo         61*/
!     for part at rest e1 and p1 prime,alpha and beta not                       !/*qo         62*/
!     calculated. e(3) ande(4) done later                                       !/*qo         63*/
      end                                                                       !/*qo         64*/

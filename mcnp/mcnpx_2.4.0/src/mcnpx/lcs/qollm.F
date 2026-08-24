!     $Id: qollm.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                        !/*qo          1*/
      subroutine qollm                                                          !/*qo          2*/
! QOLLM      calls...  SQRT                                                     !/*qo          3*/
!     QOLLM  called by...   ISOB                                                !/*qo          4*/
#include <lcs/double.h>                                                         /*!qo          5*/
#include <lcs/isobar.h>                                                         /*!qo          6*/
      if (ik_) 10,60,10                                                         !/*qo          7*/
   10 univ_=e_(2)+col_(6)-col_(11)                                              !/*qo          8*/
      unive_=e_(1)+col_(11)                                                     !/*qo          9*/
      univr_=col_(1)+col_(6)                                                    !/*qo         10*/
      k=16                                                                      !/*qo         11*/
      do 20 i=1,9,4                                                             !/*qo         12*/
      col_(k)=(pxyz_(i)*univ_-pxyz_(i+1)*unive_)/univr_                         !/*qo         13*/
      col_(k+3)=(pxyz_(i)+pxyz_(i+1))/col_(1)                                   !/*qo         14*/
!     vx                                                                        !/*qo         15*/
   20 k=k+1                                                                     !/*qo         16*/
      col_(22)=(-pxyz_(9)*pxyz_(6))/col_(1)                                     !/*qo         17*/
!     qx                                                                        !/*qo         18*/
!     abbreviated form since pxyz_(1)=pxyz_(5)=0.0                              !/*qo         19*/
      col_(23)=(pxyz_(2)*pxyz_(9))/col_(1)                                      !/*qo         20*/
!     qy                                                                        !/*qo         21*/
!     abbreviated form since pxyz_(1)=pxyz_(5)=0.0                              !/*qo         22*/
      col_(24)=dp0                                                              !/*qo         23*/
!     abbreviated form since pxyz_(1)=pxyz_(5)=0.0                              !/*qo         24*/
      a_=snt_/col_(14)                                                          !/*qo         25*/
      b=a_*col_(10)                                                             !/*qo         26*/
!     (-beta*cos phi*sin theta/alpha + cos theta)/p1p*p3p                       !/*qo         27*/
      univ_=col_(10)*(cst_-a_*sopc_*col_(13))/col_(12)                          !/*qo         28*/
      unive_=b*sops_/col_(12)                                                   !/*qo         29*/
!     p3p*sin phi*sin theta/p1p*alpha                                           !/*qo         30*/
      univr_=(sopc_*b)+((e_(3)+col_(9))/(col_(7)+dp1))                          !/*qo         31*/
!     cos phi*sin theta*p3p/alpha  +  (e3+e3p)/(dp1+gamma)                      !/*qo         32*/
      k=19                                                                      !/*qo         33*/
      do 30 i=3,11,4                                                            !/*qo         34*/
      pxyz_(i)=col_(k)*univr_+col_(k+3)*unive_+col_(k-3)*univ_                  !/*qo         35*/
   30 k=k+1                                                                     !/*qo         36*/
      do 40 i=1,9,4                                                             !/*qo         37*/
   40 pxyz_(i+3)=pxyz_(i)+pxyz_(i+1)-pxyz_(i+2)                                 !/*qo         38*/
   50 return                                                                    !/*qo         39*/
   60 do 70 i=16,17                                                             !/*qo         40*/
      col_(i)=dp0                                                               !/*qo         41*/
   70 col_(i+3)=dp0                                                             !/*qo         42*/
      col_(18)=pxyz_(9)*pm_(2)/col_(6)                                          !/*qo         43*/
!     p(1),z *m2/m=p(bar prime)1,z                                              !/*qo         44*/
      col_(21)=pxyz_(9)/col_(1)                                                 !/*qo         45*/
!     vz velocity                                                               !/*qo         46*/
      do 80 i=22,24                                                             !/*qo         47*/
   80 col_(i)=dp0                                                               !/*qo         48*/
!     cross product p1 prime x v                                                !/*qo         49*/
      pxyz_(3)=col_(10)*snt_*sopc_                                              !/*qo         50*/
!     x component p3 bar =p3 prime x sin theta x cos phi                        !/*qo         51*/
      pxyz_(7)=col_(10)*snt_*sops_                                              !/*qo         52*/
!     y comp. p3 bar =p3 prime x sin theta x sin phi                            !/*qo         53*/
      pxyz_(11)=col_(10)*cst_                                                   !/*qo         54*/
      z=pxyz_(9)/col_(6)                                                        !/*qo         55*/
      pxyz_(11)=pxyz_(11)+(z*pxyz_(9)*pxyz_(11))/(col_(1)+col_(6))+z*col        !/*qo         56*/
     & _(9)                                                                     !/*qo         57*/
!     z comp. p3 bar=p3 prime cos theta+(p1z sq*p3z (prime)cos                  !/*qo         58*/
!     theta/(e prime*(e+e prime))+p1z*e3 prime/e prime                          !/*qo         59*/
      e_(3)=sqrt(pxyz_(3)*pxyz_(3)+pxyz_(7)*pxyz_(7)+pxyz_(11)*pxyz_(11)        !/*qo         60*/
     & +pm_(3)*pm_(3))                                                          !/*qo         61*/
      do 90 i=1,9,4                                                             !/*qo         62*/
   90 pxyz_(i+3)=pxyz_(i)-pxyz_(i+2)                                            !/*qo         63*/
      e_(4)=sqrt(pxyz_(4)*pxyz_(4)+pxyz_(8)*pxyz_(8)+pxyz_(12)*pxyz_(12)        !/*qo         64*/
     & +pm_(4)*pm_(4))                                                          !/*qo         65*/
      if (pt_(38)) 50,100,50                                                    !/*qo         66*/
  100 pt_(3)=((e_(4)-pm_(4))/rcpmv_)+pt_(3)                                     !/*qo         67*/
      go to 50                                                                  !/*qo         68*/
      end                                                                       !/*qo         69*/

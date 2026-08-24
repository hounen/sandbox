!     $Id: qstor.F,v 1.4 2002/03/27 18:15:19 gwm Exp $                        !/*qs          1*/
      subroutine qstor                                                          !/*qs          2*/
! QSTOR      calls...  SQRT                                                     !/*qs          3*/
!     QSTOR  called by...   QOU18  QOU19                                        !/*qs          4*/
#include <lcs/double.h>                                                         /*!qs          5*/
#include <lcs/isobar.h>                                                         /*!qs          6*/
      l=(i1_*12)-28                                                             !/*qs          7*/
      if (i2_) 10,60,70                                                         !/*qs          8*/
   10 jj=0                                                                      !/*qs          9*/
      if (pm_(3)-dncms_) 30,30,20                                               !/*qs         10*/
   20 i1_=i1_+1                                                                 !/*qs         11*/
      jj=1                                                                      !/*qs         12*/
!     x-y-z-coordinates of collision point                                      !/*qs         13*/
   30 univ_=sqrt(pxyz_(i1_)*pxyz_(i1_)+pxyz_(i1_+4)*pxyz_(i1_+4)+pxyz_          !/*qs         14*/
     & (i1_+8)*pxyz_(i1_+8))                                                    !/*qs         15*/
      k=i1_+8                                                                   !/*qs         16*/
      do 40 i=i1_,k,4                                                           !/*qs         17*/
      pt_(l)=pxyz_(i)/univ_                                                     !/*qs         18*/
   40 l=l+1                                                                     !/*qs         19*/
      i1_=i1_-jj                                                                !/*qs         20*/
   50 return                                                                    !/*qs         21*/
   60 k=14                                                                      !/*qs         22*/
      go to 90                                                                  !/*qs         23*/
   70 if (i2_-2) 80,110,110                                                     !/*qs         24*/
   80 k=17                                                                      !/*qs         25*/
   90 univ_=sqrt(pnidk_(k)*pnidk_(k)+pnidk_(k+1)*pnidk_(k+1)+pnidk_(k+2)        !/*qs         26*/
     & *pnidk_(k+2))                                                            !/*qs         27*/
      pt_(l-3)=dp1                                                              !/*qs         28*/
      j=k+2                                                                     !/*qs         29*/
      do 100 i=k,j                                                              !/*qs         30*/
      pt_(l)=pnidk_(i)/univ_                                                    !/*qs         31*/
  100 l=l+1                                                                     !/*qs         32*/
      go to 50                                                                  !/*qs         33*/
  110 univ_=sqrt(pt_(l-3)*pt_(l-3)+pt_(l-2)*pt_(l-2)+pt_(l-1)*pt_(l-1))         !/*qs         34*/
      k=l-1                                                                     !/*qs         35*/
      m=l-3                                                                     !/*qs         36*/
      do 120 i=m,k                                                              !/*qs         37*/
      pt_(l)=pt_(i)/univ_                                                       !/*qs         38*/
  120 l=l+1                                                                     !/*qs         39*/
      pt_(m)=dp1                                                                !/*qs         40*/
      go to 50                                                                  !/*qs         41*/
      end                                                                       !/*qs         42*/

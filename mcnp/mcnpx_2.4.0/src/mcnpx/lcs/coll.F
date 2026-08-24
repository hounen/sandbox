!     $Id: coll.F,v 1.4 2002/03/27 18:14:58 gwm Exp $                         !/*co          1*/
      subroutine coll (m)                                                       !/*co          2*/
#include <lcs/double.h>                                                         /*!co          3*/
#include <lcs/btcom.h>                                                          /*!co          4*/
      if (m) 10,20,20                                                           !/*co          5*/
   10 a=pm(4)*pm(4)                                                             !/*co          6*/
      go to 30                                                                  !/*co          7*/
   20 a=sqnm                                                                    !/*co          8*/
   30 col(15)=dp0                                                               !/*co          9*/
      med=clsm                                                                  !/*co         10*/
      eco(1)=cfepn(med)                                                         !/*co         11*/
      eco(2)=cfepn(med+3)                                                       !/*co         12*/
!     proton(neutron) energy cut-off                                            !/*co         13*/
      col(1)=e(1)+e(2)                                                          !/*co         14*/
!     total energy particles 1 and 2                                            !/*co         15*/
      do 40 i=1,3                                                               !/*co         16*/
   40 col(i+1)=pm(i)*pm(i)                                                      !/*co         17*/
!     mass particle i sqd.                                                      !/*co         18*/
      col(5)=col(3)+col(2)+dp2*(e(1)*e(2)-(pxyz(1)*pxyz(2)+pxyz(5)*pxyz(        !/*co         19*/
     & 6)+pxyz(9)*pxyz(10)))                                                    !/*co         20*/
      col(6)=sqrt(col(5))                                                       !/*co         21*/
      col(7)=col(6)/col(1)                                                      !/*co         22*/
!     gam                                                                       !/*co         23*/
      col(8)=dp2*col(6)                                                         !/*co         24*/
      col(9)=(col(4)+col(5)-a)/col(8)                                           !/*co         25*/
      com2=col(9)*col(9)                                                        !/*co         26*/
      if (col(4)-2.9882156d+27) 50,50,70                                        !/*co         27*/
!     gt,pm(3)=isobar--lte,test for roundoff range,(min)sqd+or-5e23             !/*co         28*/
   50 if (col(4)-2.9872156d+27) 80,60,60                                        !/*co         29*/
!     lt,pion or nucleon mass=pm(3)                                             !/*co         30*/
   60 col(4)=2.9877156d+27                                                      !/*co         31*/
      pm(3)=5.466005d+13                                                        !/*co         32*/
   70 if (com2-col(4)) 90,110,110                                               !/*co         33*/
   80 if (col(4)-sqnm) 70,70,130                                                !/*co         34*/
!     lte,have nucleon or pion--gt,go to error                                  !/*co         35*/
   90 if (com2-9.9d-1*col(4)) 110,100,100                                       !/*co         36*/
  100 com2=col(4)                                                               !/*co         37*/
      col(9)=pm(3)                                                              !/*co         38*/
  110 col(10)=sqrt(com2-col(4))                                                 !/*co         39*/
!     p3 prime                                                                  !/*co         40*/
      col(11)=(col(5)+col(2)-col(3))/col(8)                                     !/*co         41*/
!     e1 prime                                                                  !/*co         42*/
      col(12)=sqrt(col(11)*col(11)-col(2))                                      !/*co         43*/
!     p1 prime                                                                  !/*co         44*/
      col(13)=(col(7)*e(1)-col(11))/col(12)                                     !/*co         45*/
!     beta                                                                      !/*co         46*/
      com=dp1-(col(13)*col(13)+col(7)*col(7))                                   !/*co         47*/
      if (com-5.d-6) 120,160,160                                                !/*co         48*/
  120 if (com+5.d-6) 130,150,150                                                !/*co         49*/
  130 col(15)=dp1                                                               !/*co         50*/
!     error                                                                     !/*co         51*/
  140 return                                                                    !/*co         52*/
  150 col(14)=2.236067977d-3                                                    !/*co         53*/
      go to 170                                                                 !/*co         54*/
  160 col(14)=sqrt(com)                                                         !/*co         55*/
!     alpha                                                                     !/*co         56*/
  170 e(3)=(col(9)+col(10)*(col(13)*cst+col(14)*sopc*snt))/col(7)               !/*co         57*/
      e(4)=col(1)-e(3)                                                          !/*co         58*/
      go to 140                                                                 !/*co         59*/
      end                                                                       !/*co         60*/

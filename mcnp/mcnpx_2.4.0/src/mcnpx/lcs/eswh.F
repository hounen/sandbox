!     $Id: eswh.F,v 1.4 2002/03/27 18:15:03 gwm Exp $                         !/*es          1*/
      subroutine eswh                                                           !/*es          2*/
! ESWH       calls...  AZIRN  SQRT                                              !/*es          3*/
!     ESWH   called by...   PCOL                                                !/*es          4*/
!** elastic scattering with hydrogen,                                           !/*es          5*/
!*  struck particle at rest.                                                    !/*es          6*/
#include <lcs/double.h>                                                         /*!es          7*/
#include <lcs/btcom.h>                                                          /*!es          8*/
      call azirn (sopc,sops)                                                    !/*es          9*/
      pm(4)=dncms                                                               !/*es         10*/
      pt(1)=dp0                                                                 !/*es         11*/
      do 10 i=3,13                                                              !/*es         12*/
   10 pt(i)=dp0                                                                 !/*es         13*/
      do 20 i=15,48                                                             !/*es         14*/
   20 pt(i)=dp0                                                                 !/*es         15*/
      do 30 i=1,24                                                              !/*es         16*/
   30 col(i)=dp0                                                                !/*es         17*/
      a=pm(4)*pm(4)                                                             !/*es         18*/
      col(1)=e(1)+e(2)                                                          !/*es         19*/
!     total energy particles 1 and 2                                            !/*es         20*/
      do 40 i=1,3                                                               !/*es         21*/
   40 col(i+1)=pm(i)*pm(i)                                                      !/*es         22*/
!     mass particle i sqd.                                                      !/*es         23*/
      col(5)=col(3)+col(2)+dp2*(e(1)*e(2)-(pxyz(1)*pxyz(2)+pxyz(5)*pxyz(        !/*es         24*/
     & 6)+pxyz(9)*pxyz(10)))                                                    !/*es         25*/
      col(6)=sqrt(col(5))                                                       !/*es         26*/
      col(7)=col(6)/col(1)                                                      !/*es         27*/
!     gam                                                                       !/*es         28*/
      col(8)=dp2*col(6)                                                         !/*es         29*/
      col(9)=(col(4)+col(5)-a)/col(8)                                           !/*es         30*/
      com2=col(9)*col(9)                                                        !/*es         31*/
      col(10)=sqrt(com2-col(4))                                                 !/*es         32*/
!     p3 prime                                                                  !/*es         33*/
      col(18)=pxyz(9)*pm(2)/col(6)                                              !/*es         34*/
!     p(1),z *m2/m=p(bar prime)1,z                                              !/*es         35*/
      col(21)=pxyz(9)/col(1)                                                    !/*es         36*/
!     vz velocity                                                               !/*es         37*/
      pxyz(3)=col(10)*snt*sopc                                                  !/*es         38*/
!     x component p3 bar =p3 prime x sin theta x cos phi                        !/*es         39*/
      pxyz(7)=col(10)*snt*sops                                                  !/*es         40*/
!     y comp. p3 bar =p3 prime x sin theta x sin phi                            !/*es         41*/
      pxyz(11)=col(10)*cst                                                      !/*es         42*/
      z=pxyz(9)/col(6)                                                          !/*es         43*/
      pxyz(11)=pxyz(11)+(z*pxyz(9)*pxyz(11))/(col(1)+col(6))+z*col(9)           !/*es         44*/
!     z comp. p3 bar=p3 prime cos theta+(p1z sq*p3z (prime)cos                  !/*es         45*/
!     theta/(e prime*(e+e prime))+p1z*e3 prime/e prime                          !/*es         46*/
      e(3)=sqrt(pxyz(3)*pxyz(3)+pxyz(7)*pxyz(7)+pxyz(11)*pxyz(11)+pm(3)         !/*es         47*/
     & *pm(3))                                                                  !/*es         48*/
      do 50 i=1,9,4                                                             !/*es         49*/
   50 pxyz(i+3)=pxyz(i)-pxyz(i+2)                                               !/*es         50*/
      e(4)=sqrt(pxyz(4)*pxyz(4)+pxyz(8)*pxyz(8)+pxyz(12)*pxyz(12)+pm(4)         !/*es         51*/
     & *pm(4))                                                                  !/*es         52*/
!** kinetic energy of 3 and 4                                                   !/*es         53*/
      pt(3)=(e(3)-pm(3))/rcpmv                                                  !/*es         54*/
      pt(15)=(e(4)-pm(4))/rcpmv                                                 !/*es         55*/
!** momentum of 3 and 4, in lab.                                                !/*es         56*/
      p3=sqrt(pxyz(3)*pxyz(3)+pxyz(7)*pxyz(7)+pxyz(11)*pxyz(11))                !/*es         57*/
      p4=sqrt(pxyz(4)*pxyz(4)+pxyz(8)*pxyz(8)+pxyz(12)*pxyz(12))                !/*es         58*/
!** direct cosines of 3 and 4                                                   !/*es         59*/
      pt(8)=pxyz(3)/p3                                                          !/*es         60*/
      pt(9)=pxyz(7)/p3                                                          !/*es         61*/
      pt(10)=pxyz(11)/p3                                                        !/*es         62*/
      pt(20)=pxyz(4)/p4                                                         !/*es         63*/
      pt(21)=pxyz(8)/p4                                                         !/*es         64*/
      pt(22)=pxyz(12)/p4                                                        !/*es         65*/
      return                                                                    !/*es         66*/
      end                                                                       !/*es         67*/

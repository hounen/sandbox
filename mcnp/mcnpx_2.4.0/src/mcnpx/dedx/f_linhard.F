!     $Id: f_linhard.F,v 1.4 2002/03/27 18:14:33 gwm Exp $                    !/*f_          1*/
      function f_linhard (betasq,avz)                                           !/*f_          2*/
!                                                                               !/*f_          3*/
!         betasq = (v/c)**2 for the projectile                                  !/*f_          4*/
!         avz = <Z> for the medium                                              !/*f_          5*/
!             = SUM fme_(i)z_(i)                                                !/*f_          6*/
!         f_linhard = calculated value of shell correction                      !/*f_          7*/
!                                                                               !/*f_          8*/
#include <lcs/double.h>                                                         /*!f_          9*/
      parameter (alpfsc=7.2973530791728595d-03)                                 !/*f_         10*/
      parameter (prot_mass=938.2723128d0)                                       !/*f_         11*/
      parameter (cutoff=(dp1+8.d0/prot_mass)**2)                                !/*f_         12*/
      data zal /12.95d0/, zh2o /3.34d0/                                         !/*f_         13*/
      data p1, p2, p3, p4, p5 /4.774248d-4,1.143478d-3,-5.633920d-2,4.76        !/*f_         14*/
     & 3953d-1,4.844536d-1/                                                     !/*f_         15*/
      data w1, w2, w3, w4, w5, w6, w7, w8 /-1.819954d-6,-2.232760d-5,1.2        !/*f_         16*/
     & 19912d-4,1.837873d-3,-4.457574d-3,-6.837103d-2,5.266586d-1,3.7437        !/*f_         17*/
     & 15d-1/                                                                   !/*f_         18*/
!-----------------------------------------------------------------------        !/*f_         19*/
      y=log(betasq/(avz*alpfsc**2))                                             !/*f_         20*/
      if (avz.gt.zal) go to 10                                                  !/*f_         21*/
      xl1=exp(w1*y**7+w2*y**6+w3*y**5+w4*y**4+w5*y**3+w6*y**2+w7*y+w8)          !/*f_         22*/
      if (avz.gt.zh2o) go to 10                                                 !/*f_         23*/
      xl=xl1                                                                    !/*f_         24*/
      go to 20                                                                  !/*f_         25*/
   10 continue                                                                  !/*f_         26*/
      xl=exp(p1*y**4+p2*y**3+p3*y**2+p4*y+p5)                                   !/*f_         27*/
      if (avz.gt.zal) go to 20                                                  !/*f_         28*/
      xl2=xl                                                                    !/*f_         29*/
      xl=xl1+(avz-zh2o)/(zal-zh2o)*(xl2-xl1)                                    !/*f_         30*/
   20 continue                                                                  !/*f_         31*/
      f_linhard=xl                                                              !/*f_         32*/
      return                                                                    !/*f_         33*/
      end                                                                       !/*f_         34*/

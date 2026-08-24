!     $Id: theta.F,v 1.4 2002/03/27 18:14:38 gwm Exp $                        !/*th          1*/
      function theta (iz,numshl,nprinc,nangle)                                  !/*th          2*/
#include <lcs/double.h>                                                         /*!th          3*/
!                                                                               !/*th          4*/
!        purpose : this routine calculates the theta                            !/*th          5*/
!                  value using the physics from:                                !/*th          6*/
!                                                                               !/*th          7*/
!                    e. mertzbacher and h. lewis, x-ray                         !/*th          8*/
!                    production by heavy charged particles,                     !/*th          9*/
!                    in encyclopedia of physics (s. flugge, ed.),               !/*th         10*/
!                    vol. xxxiv, springer-verlag, berlin (1958).                !/*th         11*/
!                                                                               !/*th         12*/
!                  theta is calculated relativistically using                   !/*th         13*/
!                  equations 4.19, 4.17, and 4.16 of the above                  !/*th         14*/
!                  reference.                                                   !/*th         15*/
!                                                                               !/*th         16*/
!        author :  joe janni and daniel pickens                                 !/*th         17*/
!                  10 september 1983                                            !/*th         18*/
!                                                                               !/*th         19*/
!        inputs :  iz     - the atomic number.                                  !/*th         20*/
!                  numshl - the shell number, values from 1 to 18.              !/*th         21*/
!                           (max_shell = 18 in block8.h)                        !/*th         22*/
!                  nprinc - the principle quantum number.                       !/*th         23*/
!                  nangle - the angular momentum quantum number.                !/*th         24*/
!                                                                               !/*th         25*/
!        outputs : theta  - the calculated theta value.                         !/*th         26*/
!                                                                               !/*th         27*/
!        errors :  none.                                                        !/*th         28*/
!                                                                               !/*th         29*/
#include <dedx/block8.h>                                                        /*!th         30*/
!        calculate the alpha constant.                                          !/*th         31*/
!                                                                               !/*th         32*/
      z = iz                                                                    !/*th         33*/
      prin = nprinc                                                             !/*th         34*/
      angl = nangle                                                             !/*th         35*/
      alpha=1.0/137.0                                                           !/*th         36*/
!                                                                               !/*th         37*/
!        actual theta calulation.                                               !/*th         38*/
!                                                                               !/*th         39*/
      theta=0.0                                                                 !/*th         40*/
      if (eshel(numshl,iz).gt.0.0) theta=(prin/(z-slater(numshl                 !/*th         41*/
     & ,iz)))**2*(eshel(numshl,iz))/eshel(1,1)-((z-slater(numshl,iz))           !/*th         42*/
     & *alpha/prin)**2*(prin/angl-(3.0/4.0))                                    !/*th         43*/
!                                                                               !/*th         44*/
!        assume that minor effects of the relativistic correction               !/*th         45*/
!        in the theta calculation are not permitted to produce                  !/*th         46*/
!        an erroneous slightly negative theta value.                            !/*th         47*/
!                                                                               !/*th         48*/
      if (theta.lt.0.0) theta=0.0                                               !/*th         49*/
!                                                                               !/*th         50*/
      return                                                                    !/*th         51*/
      end                                                                       !/*th         52*/

!     $Id: eta.F,v 1.4 2002/03/27 18:14:33 gwm Exp $                          !/*et          1*/
      function eta (z,numshl,betasq)                                            !/*et          2*/
#include <lcs/double.h>                                                         /*!et          3*/
!                                                                               !/*et          4*/
!        purpose :  this routine computes the eta value using                   !/*et          5*/
!                   the physics in :                                            !/*et          6*/
!                                                                               !/*et          7*/
!                     h. bichsel, passage of charged particles through          !/*et          8*/
!                     matter, in american institute of physics handbook,        !/*et          9*/
!                     third edition (d. gray, ed.), mcgraw-hill book            !/*et         10*/
!                     company inc., new york (1972).                            !/*et         11*/
!                                                                               !/*et         12*/
!                   eta is calculated from equation 3.13 of the above           !/*et         13*/
!                   reference, the relativistic correction, 1.0/(1.0-bet        !/*et         14*/
!                   recommended by bichsel has been applied.                    !/*et         15*/
!                                                                               !/*et         16*/
!        author :   daniel pickens / joe janni                                  !/*et         17*/
!                   computer sciences corporation  / afwl                       !/*et         18*/
!                   10 september 1983                                           !/*et         19*/
!                                                                               !/*et         20*/
!        inputs :   z      - the atomic number of the element.                  !/*et         21*/
!                   numshl - the shell number, values from 1 to 18.             !/*et         22*/
!                            (max_shell = 18 in block8.h)                       !/*et         23*/
!                   betasq - the converted kinetic energy to                    !/*et         24*/
!                            velocity squared.                                  !/*et         25*/
!                                                                               !/*et         26*/
!        outputs :  eta    - the eta value                                      !/*et         27*/
!                                                                               !/*et         28*/
!        errors :   stop 'eta is le zero in function eta'                       !/*et         29*/
!                                                                               !/*et         30*/
      parameter (emass=0.5109990615d0)                                          !/*et         31*/
#include <dedx/block8.h>                                                        /*!et         32*/
!                                                                               !/*et         33*/
!        compute the eta value remembering to multiply by 10**6,                !/*et         34*/
!        (1000000), inorder to convert to the same units.                       !/*et         35*/
!                                                                               !/*et         36*/
      iz=nint(z)                                                                !/*et         37*/
      eta=emass*1000000.0*betasq/(2.0*eshel(1,1)*(1.0-betasq)*(z-slater         !/*et         38*/
     & (numshl,iz))**2)                                                         !/*et         39*/
!                                                                               !/*et         40*/
!        check the bounds of the eta value, if zero or negative,                !/*et         41*/
!        then an error has resulted, so print a error message out               !/*et         42*/
!        and stop.                                                              !/*et         43*/
!                                                                               !/*et         44*/
      if (eta.le.0.0) stop 'eta is le zero in function eta'                     !/*et         45*/
!                                                                               !/*et         46*/
      return                                                                    !/*et         47*/
      end                                                                       !/*et         48*/

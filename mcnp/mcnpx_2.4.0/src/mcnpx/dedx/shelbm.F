!     $Id: shelbm.F,v 1.5 2002/03/27 18:14:33 gwm Exp $                       !/*sh          1*/
      function shelbm (z,betasq,cm1,cm2,cm3,ptraut)                             !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the m-shell correction               !/*sh          5*/
!                   using the physics discribed in reference:                   !/*sh          6*/
!                                                                               !/*sh          7*/
!                     h. bichsel, "stopping power of m electrons                !/*sh          8*/
!                     for heavy charged particles," physical review a,          !/*sh          9*/
!                     vol. 28, p1147 (1983)                                     !/*sh         10*/
!                                                                               !/*sh         11*/
!        author :   daniel pickens                                              !/*sh         12*/
!                   computer sciences corporation                               !/*sh         13*/
!                   20 may 1984                                                 !/*sh         14*/
!                                                                               !/*sh         15*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         16*/
!                   betasq - the converted kinetic energy to                    !/*sh         17*/
!                            velocity squared.                                  !/*sh         18*/
!                   ptraut  - logical variable, if true then author and         !/*sh         19*/
!                            reference information written to scratch fi        !/*sh         20*/
!                                                                               !/*sh         21*/
!        outputs :  shelbm - the m-shell correction as computed using           !/*sh         22*/
!                            Bichsel method and data.                           !/*sh         23*/
!                   cm1    - s subshell correction.                             !/*sh         24*/
!                   cm2    - p subshell correction.                             !/*sh         25*/
!                   cm3    - d subshell correction.                             !/*sh         26*/
!                                                                               !/*sh         27*/
!        errors :   none.                                                       !/*sh         28*/
!                                                                               !/*sh         29*/
!        compute the correction for each subshell via routines                  !/*sh         30*/
!        subb1m, subb2m, and subb3m. then, compute the total                    !/*sh         31*/
!        shell correction using the values of the subshells.                    !/*sh         32*/
!                                                                               !/*sh         33*/
      logical ptraut                                                            !/*sh         34*/
!                                                                               !/*sh         35*/
      cm1=subb1m(z,betasq)                                                      !/*sh         36*/
      cm2=subb2m(z,betasq)                                                      !/*sh         37*/
      cm3=subb3m(z,betasq)                                                      !/*sh         38*/
!                                                                               !/*sh         39*/
      shelbm=cm1+cm2+cm3                                                        !/*sh         40*/
!                                                                               !/*sh         41*/
!        write out the author and reference information to                      !/*sh         42*/
!        scratch file if ptraut is true.                                        !/*sh         43*/
!                                                                               !/*sh         44*/
      if (ptraut) write (10,10)                                                 !/*sh         45*/
!                                                                               !/*sh         46*/
      return                                                                    !/*sh         47*/
!                                                                               !/*sh         48*/
   10 format ('m-shell - bichsel''s subshell method, physical',                 !/*sh         49*/
     & ' review a, vol. 28, p 1147 (1968).')                                    !/*sh         50*/
      end                                                                       !/*sh         51*/

!     $Id: shlkim.F,v 1.5 2002/03/27 18:14:36 gwm Exp $                       !/*sh          1*/
      function shlkim (z,betasq,cm1,cm2,cm3,ptraut)                             !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the m-shell correction               !/*sh          5*/
!                   using the physics discribed in reference 137:               !/*sh          6*/
!                                                                               !/*sh          7*/
!                     g. khandelwal and e. merzbacher,                          !/*sh          8*/
!                     "stopping power of m electrons,"                          !/*sh          9*/
!                     physical review, vol 144, number 1 (1966)                 !/*sh         10*/
!                                                                               !/*sh         11*/
!                   and in the reference:                                       !/*sh         12*/
!                                                                               !/*sh         13*/
!                     g. khandelwal, "stopping power of m electrons             !/*sh         14*/
!                     for heavy charged particles," unpublished                 !/*sh         15*/
!                     paper, 9 oct. 1982                                        !/*sh         16*/
!                                                                               !/*sh         17*/
!                   and using data supplied in a letter from                    !/*sh         18*/
!                   g. khandelwal to joe janni dated 31 january, 1968.          !/*sh         19*/
!                                                                               !/*sh         20*/
!        author :   daniel pickens                                              !/*sh         21*/
!                   computer sciences corporation                               !/*sh         22*/
!                   30 june 1984                                                !/*sh         23*/
!                                                                               !/*sh         24*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         25*/
!                   betasq - the converted kinetic energy to                    !/*sh         26*/
!                            velocity squared.                                  !/*sh         27*/
!                   ptraut  - logical variable,if true then author and          !/*sh         28*/
!                            reference information written to scratch fi        !/*sh         29*/
!                                                                               !/*sh         30*/
!        outputs :  shlkim - the m-shell correction as computed using           !/*sh         31*/
!                            Khandelwal method and data.                        !/*sh         32*/
!                   cm1    - s subshell correction.                             !/*sh         33*/
!                   cm2    - p subshell correction.                             !/*sh         34*/
!                   cm3    - d subshell correction.                             !/*sh         35*/
!                                                                               !/*sh         36*/
!        errors :   none.                                                       !/*sh         37*/
!                                                                               !/*sh         38*/
#include <dedx/block8.h>                                                        /*!sh         39*/
      logical ptraut                                                            !/*sh         40*/
!                                                                               !/*sh         41*/
!        first, initialize the correction values.                               !/*sh         42*/
!                                                                               !/*sh         43*/
      iz=nint(z)                                                                !/*sh         44*/
      cm1=0.0                                                                   !/*sh         45*/
      cm2=0.0                                                                   !/*sh         46*/
      cm3=0.0                                                                   !/*sh         47*/
      shlkim=0.0                                                                !/*sh         48*/
!                                                                               !/*sh         49*/
!        check to make sure that there are electrons in this                    !/*sh         50*/
!        shell. if there are no electrons, return.                              !/*sh         51*/
!                                                                               !/*sh         52*/
      if ((shel(4,iz).eq.0.0).and.(shel(5,iz).eq.0.0).and.(shel(6,iz).eq        !/*sh         53*/
     & .0.0)) return                                                            !/*sh         54*/
!                                                                               !/*sh         55*/
!        compute the correction for each subshell via routines                  !/*sh         56*/
!        subk1m, subk2m, and subk3m. then, compute the total                    !/*sh         57*/
!        shell correction using the values of the subshells.                    !/*sh         58*/
!                                                                               !/*sh         59*/
      cm1=subk1m(z,betasq)                                                      !/*sh         60*/
      cm2=subk2m(z,betasq)                                                      !/*sh         61*/
      cm3=subk3m(z,betasq)                                                      !/*sh         62*/
!                                                                               !/*sh         63*/
      shlkim=cm1+cm2+cm3                                                        !/*sh         64*/
!                                                                               !/*sh         65*/
!        write out the author and reference information to                      !/*sh         66*/
!        scratch file if ptraut is true.                                        !/*sh         67*/
!                                                                               !/*sh         68*/
      if (ptraut) write (10,10)                                                 !/*sh         69*/
!                                                                               !/*sh         70*/
      return                                                                    !/*sh         71*/
!                                                                               !/*sh         72*/
   10 format ('m-shell - khandelwal''s subshell method, ',                      !/*sh         73*/
     & 'physical review, vol. 144 (1966).')                                     !/*sh         74*/
      end                                                                       !/*sh         75*/

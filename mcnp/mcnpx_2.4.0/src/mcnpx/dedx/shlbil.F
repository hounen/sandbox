!     $Id: shlbil.F,v 1.5 2002/03/27 18:14:36 gwm Exp $                       !/*sh          1*/
      function shlbil (z,betasq,cl1,cl2,ptraut)                                 !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the l-shell                          !/*sh          5*/
!                   correction using the physics discribed in                   !/*sh          6*/
!                   reference 138. in this case each subshell                   !/*sh          7*/
!                   is calculated separately (in routines                       !/*sh          8*/
!                   subb1l and subb2l).                                         !/*sh          9*/
!                                                                               !/*sh         10*/
!                     h. bichsel, "the l-shell correction in                    !/*sh         11*/
!                     stopping power," usc-136-120 (1967).                      !/*sh         12*/
!                                                                               !/*sh         13*/
!        author :   daniel pickens                                              !/*sh         14*/
!                   computer sciences corporation                               !/*sh         15*/
!                   10 june 1984                                                !/*sh         16*/
!                                                                               !/*sh         17*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         18*/
!                   betasq - the converted kinetic energy to                    !/*sh         19*/
!                            velocity squared.                                  !/*sh         20*/
!                   ptraut  - logical variable,if true then author and          !/*sh         21*/
!                            reference information written to scratch fi        !/*sh         22*/
!                                                                               !/*sh         23*/
!        outputs :  shlbil - the l-shell correction as computed using           !/*sh         24*/
!                            Bichsel method and data.                           !/*sh         25*/
!                   cl1    - the l-shell, s-subshell correction.                !/*sh         26*/
!                   cl2    - the l-shell, p-subshell correction.                !/*sh         27*/
!                                                                               !/*sh         28*/
!        errors :   none.                                                       !/*sh         29*/
!                                                                               !/*sh         30*/
#include <dedx/block8.h>                                                        /*!sh         31*/
      logical ptraut                                                            !/*sh         32*/
!                                                                               !/*sh         33*/
!        first initialize the shell and subshell corrections to zero.           !/*sh         34*/
!                                                                               !/*sh         35*/
      iz=nint(z)                                                                !/*sh         36*/
      cl1=0.0                                                                   !/*sh         37*/
      cl2=0.0                                                                   !/*sh         38*/
      shlbil=0.0                                                                !/*sh         39*/
!                                                                               !/*sh         40*/
!        check to make sure that there are electrons in this                    !/*sh         41*/
!        shell. if there are no electrons, return.                              !/*sh         42*/
!                                                                               !/*sh         43*/
      if ((shel(2,iz).eq.0.0).and.(shel(3,iz).eq.0.0)) return                   !/*sh         44*/
!                                                                               !/*sh         45*/
!        compute the correction for each subshell via routines                  !/*sh         46*/
!        subb1l and subb2l. the values of c1 and c2 are the correction          !/*sh         47*/
!        per total subshell.                                                    !/*sh         48*/
!                                                                               !/*sh         49*/
      cl1=subb1l(z,betasq)                                                      !/*sh         50*/
      cl2=subb2l(z,betasq)                                                      !/*sh         51*/
!                                                                               !/*sh         52*/
!        add the two subshell correction values to obtain the                   !/*sh         53*/
!        total correction of the l shell.                                       !/*sh         54*/
!                                                                               !/*sh         55*/
      shlbil=cl1+cl2                                                            !/*sh         56*/
!                                                                               !/*sh         57*/
!        write out the author and reference information to                      !/*sh         58*/
!        scratch file if ptraut is true.                                        !/*sh         59*/
!                                                                               !/*sh         60*/
      if (ptraut) write (10,10)                                                 !/*sh         61*/
!                                                                               !/*sh         62*/
      return                                                                    !/*sh         63*/
!                                                                               !/*sh         64*/
   10 format                                                                    !/*sh         65*/
     & ('l-shell - bichsel''s subshell method, usc-136-120 (1967).')            !/*sh         66*/
      end                                                                       !/*sh         67*/

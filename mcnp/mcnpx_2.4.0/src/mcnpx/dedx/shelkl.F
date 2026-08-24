!     $Id: shelkl.F,v 1.5 2002/03/27 18:14:34 gwm Exp $                       !/*sh          1*/
      function shelkl (z,betasq,cl1,cl2,ptraut)                                 !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the l-shell correction               !/*sh          5*/
!                   using the physics discribed in reference 141:               !/*sh          6*/
!                                                                               !/*sh          7*/
!                     g. khandelwal, "shell corrections for k-                  !/*sh          8*/
!                     and l- electrons," nuclear physics, vol. a116,            !/*sh          9*/
!                     p 97 (1968).                                              !/*sh         10*/
!                                                                               !/*sh         11*/
!                   and in the reference:                                       !/*sh         12*/
!                                                                               !/*sh         13*/
!                     g. khandelwal, "stopping power of k and                   !/*sh         14*/
!                     l electrons," physical review a., vol 5,                  !/*sh         15*/
!                     number 26 (november 1982).                                !/*sh         16*/
!                                                                               !/*sh         17*/
!        author :   daniel pickens                                              !/*sh         18*/
!                   computer sciences corporation                               !/*sh         19*/
!                   13 may 1984                                                 !/*sh         20*/
!                                                                               !/*sh         21*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         22*/
!                   betasq - the converted kinetic energy to                    !/*sh         23*/
!                            velocity squared.                                  !/*sh         24*/
!                   ptraut - flag specifying to write out author                !/*sh         25*/
!                            and reference information to scratch file.         !/*sh         26*/
!                                                                               !/*sh         27*/
!        outputs :  shelkl - the l-shell correction as computed using           !/*sh         28*/
!                            Khandelwal method and data.                        !/*sh         29*/
!                   cl1    - the l-shell, s-subshell correction.                !/*sh         30*/
!                   cl2    - the l-shell, p-subshell correction.                !/*sh         31*/
!                                                                               !/*sh         32*/
!        errors :   none.                                                       !/*sh         33*/
!                                                                               !/*sh         34*/
#include <dedx/block8.h>                                                        /*!sh         35*/
      logical ptraut                                                            !/*sh         36*/
!                                                                               !/*sh         37*/
!        first initialize the shell and subshell corrections to zero.           !/*sh         38*/
!                                                                               !/*sh         39*/
      iz=nint(z)                                                                !/*sh         40*/
      cl1=0.0                                                                   !/*sh         41*/
      cl2=0.0                                                                   !/*sh         42*/
      shelkl=0.0                                                                !/*sh         43*/
!                                                                               !/*sh         44*/
!        check to make sure that there are electrons in this                    !/*sh         45*/
!        shell. if there are no electrons, return.                              !/*sh         46*/
!                                                                               !/*sh         47*/
      if ((shel(2,iz).eq.0.0).and.(shel(3,iz).eq.0.0)) return                   !/*sh         48*/
!                                                                               !/*sh         49*/
!        compute the correction for each subshell via routines                  !/*sh         50*/
!        subk1l and subk2l. the values of c1 and c2 are the correction          !/*sh         51*/
!        per total subshell.                                                    !/*sh         52*/
!                                                                               !/*sh         53*/
      cl1=subk1l(z,betasq)                                                      !/*sh         54*/
      cl2=subk2l(z,betasq)                                                      !/*sh         55*/
!                                                                               !/*sh         56*/
!        add the two election correction values to obtain the                   !/*sh         57*/
!        correction for the total correction of the l shell.                    !/*sh         58*/
!                                                                               !/*sh         59*/
      shelkl=cl1+cl2                                                            !/*sh         60*/
!                                                                               !/*sh         61*/
!        write out the author and reference information to                      !/*sh         62*/
!        scratch file if ptraut is true.                                        !/*sh         63*/
!                                                                               !/*sh         64*/
      if (ptraut) write (10,10)                                                 !/*sh         65*/
!                                                                               !/*sh         66*/
      return                                                                    !/*sh         67*/
!                                                                               !/*sh         68*/
   10 format ('l-shell - khandelwal''s subshell method,',                       !/*sh         69*/
     & ' nuclear physics, vol. a116, p 97 (1968).')                             !/*sh         70*/
      end                                                                       !/*sh         71*/

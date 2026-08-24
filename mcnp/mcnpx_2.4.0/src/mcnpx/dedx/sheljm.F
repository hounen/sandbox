!     $Id: sheljm.F,v 1.5 2002/03/27 18:14:34 gwm Exp $                       !/*sh          1*/
      function sheljm (z,betasq,cm1,cm2,cm3,ptraut)                             !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the m-shell correction               !/*sh          5*/
!                   using the janni method. this is the original method         !/*sh          6*/
!                   of computing the m shell correction. the computation        !/*sh          7*/
!                   is a individual subshell method using scaling based         !/*sh          8*/
!                   on the janni l shell method.                                !/*sh          9*/
!                                                                               !/*sh         10*/
!        author :   joe janni - original                                        !/*sh         11*/
!                   daniel pickens - revised                                    !/*sh         12*/
!                     computer sciences corporation                             !/*sh         13*/
!                     30 june 1984                                              !/*sh         14*/
!                                                                               !/*sh         15*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         16*/
!                   betasq - the converted kinetic energy to                    !/*sh         17*/
!                            velocity squared.                                  !/*sh         18*/
!                   ptraut - variable specifying to write out author            !/*sh         19*/
!                            and reference information to scratch file.         !/*sh         20*/
!                                                                               !/*sh         21*/
!        outputs :  sheljm - the m-shell correction as computed using           !/*sh         22*/
!                            Janni method.                                      !/*sh         23*/
!                   cm1    - s subshell correction.                             !/*sh         24*/
!                   cm2    - p subshell correction.                             !/*sh         25*/
!                   cm3    - d subshell correction.                             !/*sh         26*/
!                                                                               !/*sh         27*/
!        errors :   none.                                                       !/*sh         28*/
!                                                                               !/*sh         29*/
      logical ptraut                                                            !/*sh         30*/
#include <dedx/block8.h>                                                        /*!sh         31*/
!                                                                               !/*sh         32*/
      j=nint(z)                                                                 !/*sh         33*/
      cm1=0.0                                                                   !/*sh         34*/
      cm2=0.0                                                                   !/*sh         35*/
      cm3=0.0                                                                   !/*sh         36*/
      sheljm=0.0                                                                !/*sh         37*/
      if (z.lt.25.0) return                                                     !/*sh         38*/
!                                                                               !/*sh         39*/
!        set the m-shell scaling factor.                                        !/*sh         40*/
!                                                                               !/*sh         41*/
      fudge=1.6                                                                 !/*sh         42*/
      beta=sqrt(betasq)                                                         !/*sh         43*/
!                                                                               !/*sh         44*/
      cm1=subshl(z,beta,fudge,shel(4,j),shel(2,j),shel(3,j),eshel(4,j)          !/*sh         45*/
     & ,eshel(2,j))                                                             !/*sh         46*/
!                                                                               !/*sh         47*/
      if (cm1.lt.0.0) cm1=1.5*cm1                                               !/*sh         48*/
!                                                                               !/*sh         49*/
      cm2=subshl(z,beta,fudge,shel(5,j),shel(2,j),shel(3,j),eshel(5,j)          !/*sh         50*/
     & ,eshel(3,j))                                                             !/*sh         51*/
!                                                                               !/*sh         52*/
      if (cm2.lt.0.0) cm2=1.5*cm2                                               !/*sh         53*/
!                                                                               !/*sh         54*/
      cm3=subshl(z,beta,fudge,shel(6,j),shel(2,j),shel(3,j),eshel(6,j)          !/*sh         55*/
     & ,eshel(3,j))                                                             !/*sh         56*/
!                                                                               !/*sh         57*/
      if (cm3.lt.0.0) cm3=1.5*cm3                                               !/*sh         58*/
!                                                                               !/*sh         59*/
      sheljm=cm1+cm2+cm3                                                        !/*sh         60*/
!                                                                               !/*sh         61*/
!        write out the author and reference information to                      !/*sh         62*/
!        scratch file if ptraut is true.                                        !/*sh         63*/
!                                                                               !/*sh         64*/
      if (ptraut) write (10,10)                                                 !/*sh         65*/
!                                                                               !/*sh         66*/
      return                                                                    !/*sh         67*/
!                                                                               !/*sh         68*/
   10 format ('m-shell - janni''s subshell method.')                            !/*sh         69*/
      end                                                                       !/*sh         70*/

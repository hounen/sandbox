!     $Id: sheljp.F,v 1.5 2002/03/27 18:14:34 gwm Exp $                       !/*sh          1*/
      function sheljp (z,bsqp,c1,c2,c3,ptraut)                                  !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the p-shell correction               !/*sh          5*/
!                   using the janni method. this is the original method         !/*sh          6*/
!                   of computing the p shell correction. the computation        !/*sh          7*/
!                   is a individual subshell method using scaling based         !/*sh          8*/
!                   on the janni l shell method.                                !/*sh          9*/
!                                                                               !/*sh         10*/
!        author :   joe janni - original                                        !/*sh         11*/
!                   daniel pickens - revised                                    !/*sh         12*/
!                     computer sciences corporation                             !/*sh         13*/
!                     30 june 1984                                              !/*sh         14*/
!                                                                               !/*sh         15*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         16*/
!                   bsqp   - the converted kinetic energy to                    !/*sh         17*/
!                            velocity squared.                                  !/*sh         18*/
!                   ptraut - logical variable,if true then author and           !/*sh         19*/
!                            reference information written to scratch fi        !/*sh         20*/
!                                                                               !/*sh         21*/
!        outputs :  sheljp - the p-shell correction as computed using           !/*sh         22*/
!                            Janni method.                                      !/*sh         23*/
!                   c1     - s subshell correction.                             !/*sh         24*/
!                   c2     - p subshell correction.                             !/*sh         25*/
!                   c3     - d subshell correction.                             !/*sh         26*/
!                                                                               !/*sh         27*/
!        errors :   none.                                                       !/*sh         28*/
!                                                                               !/*sh         29*/
#include <dedx/block8.h>                                                        /*!sh         30*/
      logical ptraut                                                            !/*sh         31*/
!                                                                               !/*sh         32*/
      c1=0.0                                                                    !/*sh         33*/
      c2=0.0                                                                    !/*sh         34*/
      c3=0.0                                                                    !/*sh         35*/
      sheljp=0.0                                                                !/*sh         36*/
      iz=z                                                                      !/*sh         37*/
      if (iz.lt.82) return                                                      !/*sh         38*/
!                                                                               !/*sh         39*/
      fudge=1.0                                                                 !/*sh         40*/
      beta=sqrt(bsqp)                                                           !/*sh         41*/
!                                                                               !/*sh         42*/
      c1=subshl(z,beta,fudge,shel(15,iz),shel(2,iz),shel(3,iz),eshel(15         !/*sh         43*/
     & ,iz),eshel(2,iz))                                                        !/*sh         44*/
!                                                                               !/*sh         45*/
      if (shel(16,iz).gt.0.0) c2=subshl(z,beta,fudge,shel(16,iz),shel(2         !/*sh         46*/
     & ,iz),shel(3,iz),eshel(16,iz),eshel(3,iz))                                !/*sh         47*/
!                                                                               !/*sh         48*/
      if (shel(17,iz).gt.0.0) c3=subshl(z,beta,fudge,shel(17,iz),shel(2         !/*sh         49*/
     & ,iz),shel(3,iz),eshel(17,iz),eshel(3,iz))                                !/*sh         50*/
!                                                                               !/*sh         51*/
      sheljp=c1+c2+c3                                                           !/*sh         52*/
!                                                                               !/*sh         53*/
!        write out the author and reference information to                      !/*sh         54*/
!        scratch file if ptraut is true.                                        !/*sh         55*/
!                                                                               !/*sh         56*/
      if (ptraut) write (10,10)                                                 !/*sh         57*/
!                                                                               !/*sh         58*/
      return                                                                    !/*sh         59*/
!                                                                               !/*sh         60*/
   10 format ('p-shell - janni''s subshell method.')                            !/*sh         61*/
      end                                                                       !/*sh         62*/

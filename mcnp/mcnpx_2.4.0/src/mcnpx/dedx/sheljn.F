!     $Id: sheljn.F,v 1.5 2002/03/27 18:14:34 gwm Exp $                       !/*sh          1*/
      function sheljn (z,bsqp,c1,c2,c3,c4,ptraut)                               !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the n-shell correction               !/*sh          5*/
!                   using the janni method. this is the original method         !/*sh          6*/
!                   of computing the n shell correction. the computation        !/*sh          7*/
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
!                   ptraut  - logical varaible, if true then author and         !/*sh         19*/
!                            reference information written to scratch fi        !/*sh         20*/
!                                                                               !/*sh         21*/
!        outputs :  sheljn - the n-shell correction as computed using           !/*sh         22*/
!                            Janni method.                                      !/*sh         23*/
!                   c1     - s subshell correction.                             !/*sh         24*/
!                   c2     - p subshell correction.                             !/*sh         25*/
!                   c3     - d subshell correction.                             !/*sh         26*/
!                   c4     - f subshell correction.                             !/*sh         27*/
!                                                                               !/*sh         28*/
!        errors :   none.                                                       !/*sh         29*/
!                                                                               !/*sh         30*/
#include <dedx/block8.h>                                                        /*!sh         31*/
      logical ptraut                                                            !/*sh         32*/
!                                                                               !/*sh         33*/
      iz=nint(z)                                                                !/*sh         34*/
      c1=0.0                                                                    !/*sh         35*/
      c2=0.0                                                                    !/*sh         36*/
      c3=0.0                                                                    !/*sh         37*/
      c4=0.0                                                                    !/*sh         38*/
      sheljn=0.0                                                                !/*sh         39*/
      if (iz.le.26) return                                                      !/*sh         40*/
!                                                                               !/*sh         41*/
      fudge=1.8                                                                 !/*sh         42*/
      beta=sqrt(bsqp)                                                           !/*sh         43*/
!                                                                               !/*sh         44*/
      if ((iz.ge.26).and.(iz.le.36)) then                                       !/*sh         45*/
!                                                                               !/*sh         46*/
        sheljn=(shel(4,iz)/8.0)*1.0e-4*z*(1.0-bsqp)/bsqp                        !/*sh         47*/
      else                                                                      !/*sh         48*/
!                                                                               !/*sh         49*/
        c1=subshl(z,beta,fudge,shel(7,iz),shel(2,iz),shel(3,iz),eshel(7         !/*sh         50*/
     &  ,iz),eshel(2,iz))                                                       !/*sh         51*/
!                                                                               !/*sh         52*/
        c2=subshl(z,beta,fudge,shel(8,iz),shel(2,iz),shel(3,iz),eshel(8         !/*sh         53*/
     &  ,iz),eshel(3,iz))                                                       !/*sh         54*/
!                                                                               !/*sh         55*/
        if (shel(9,iz).gt.0.0) c3=subshl(z,beta,fudge,shel(9,iz),shel(2         !/*sh         56*/
     &  ,iz),shel(3,iz),eshel(9,iz),eshel(3,iz))                                !/*sh         57*/
!                                                                               !/*sh         58*/
        if (shel(10,iz).gt.0.0) c4=subshl(z,beta,fudge,shel(10,iz),shel(        !/*sh         59*/
     &  2,iz),shel(3,iz),eshel(10,iz),eshel(3,iz))                              !/*sh         60*/
!                                                                               !/*sh         61*/
        sheljn=c1+c2+c3+c4                                                      !/*sh         62*/
!                                                                               !/*sh         63*/
      endif                                                                     !/*sh         64*/
!                                                                               !/*sh         65*/
!        write out the author and reference information to                      !/*sh         66*/
!        scratch file if ptraut is true.                                        !/*sh         67*/
!                                                                               !/*sh         68*/
      if (ptraut) write (10,10)                                                 !/*sh         69*/
!                                                                               !/*sh         70*/
      return                                                                    !/*sh         71*/
!                                                                               !/*sh         72*/
   10 format ('n-shell - janni''s subshell method.')                            !/*sh         73*/
      end                                                                       !/*sh         74*/

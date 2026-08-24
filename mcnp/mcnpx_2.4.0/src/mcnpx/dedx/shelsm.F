!     $Id: shelsm.F,v 1.4 2002/03/27 18:14:35 gwm Exp $                       !/*sh          1*/
      function shelsm (z,betasq,cm1,cm2,cm3,ptraut)                             !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the m-shell correction               !/*sh          5*/
!                   using the scaled method. this method involves               !/*sh          6*/
!                   scaling the bichsel l shell correction to produce           !/*sh          7*/
!                   the m shell correction. the physics in this                 !/*sh          8*/
!                   routine is described in reference 133:                      !/*sh          9*/
!                                                                               !/*sh         10*/
!                     h. bichsel, "higher shell corrections in stopping         !/*sh         11*/
!                     power," univ of southern california linear                !/*sh         12*/
!                     accelerator group technical report no. 3 (1961).          !/*sh         13*/
!                                                                               !/*sh         14*/
!        author :   daniel pickens                                              !/*sh         15*/
!                   computer sciences corporation                               !/*sh         16*/
!                   15 september 1984                                           !/*sh         17*/
!                                                                               !/*sh         18*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         19*/
!                   betasq - the converted kinetic energy to                    !/*sh         20*/
!                            velocity squared.                                  !/*sh         21*/
!                   ptraut - logical variable, if true then author and          !/*sh         22*/
!                            reference information written to scratch fi        !/*sh         23*/
!                                                                               !/*sh         24*/
!        outputs :  shelsm - the m-shell correction as computed using           !/*sh         25*/
!                            scaling method.                                    !/*sh         26*/
!                   cm1    - s subshell correction.                             !/*sh         27*/
!                   cm2    - p subshell correction.                             !/*sh         28*/
!                   cm3    - d subshell correction.                             !/*sh         29*/
!                                                                               !/*sh         30*/
!        errors :   none.                                                       !/*sh         31*/
!                                                                               !/*sh         32*/
#include <dedx/block8.h>                                                        /*!sh         33*/
      logical ptraut                                                            !/*sh         34*/
!                                                                               !/*sh         35*/
!        initialize the shell and subshell corrections to zero.                 !/*sh         36*/
!                                                                               !/*sh         37*/
      iz=nint(z)                                                                !/*sh         38*/
      cm1=0.0                                                                   !/*sh         39*/
      cm2=0.0                                                                   !/*sh         40*/
      cm3=0.0                                                                   !/*sh         41*/
      shelsm=0.0                                                                !/*sh         42*/
!                                                                               !/*sh         43*/
!        set the m-shell scaling factor.                                        !/*sh         44*/
!                                                                               !/*sh         45*/
      fudge=1.6                                                                 !/*sh         46*/
!                                                                               !/*sh         47*/
!        compute each of the subshell corrections separately, using             !/*sh         48*/
!        the scaling method based upon the bichsel l shell.                     !/*sh         49*/
!                                                                               !/*sh         50*/
!        the s subshell is first. compute the ratio, verify that                !/*sh         51*/
!        electrons actually in the subshell, if not then bypass                 !/*sh         52*/
!        computation.                                                           !/*sh         53*/
!                                                                               !/*sh         54*/
      if ((eshel(2,iz).ne.0.0).and.(eshel(4,iz).ne.0.0)) then                   !/*sh         55*/
        ratio=eshel(2,iz)/eshel(4,iz)                                           !/*sh         56*/
!                                                                               !/*sh         57*/
!        then compute the scaled beta squared value.                            !/*sh         58*/
!                                                                               !/*sh         59*/
        bsqscl=(betasq*ratio)/fudge                                             !/*sh         60*/
!                                                                               !/*sh         61*/
!        check scaled beta squared value and then compute the                   !/*sh         62*/
!        actual m shell, s subshell correction.                                 !/*sh         63*/
!                                                                               !/*sh         64*/
        if (bsqscl.le.0.99) cm1=(shel(4,iz)/shel(2,iz))*(1.0-betasq)            !/*sh         65*/
     &  *subb1l(z,bsqscl)/(1.0-bsqscl)                                          !/*sh         66*/
!                                                                               !/*sh         67*/
        if (cm1.lt.0.0) cm1=1.5*cm1                                             !/*sh         68*/
      endif                                                                     !/*sh         69*/
!                                                                               !/*sh         70*/
!        the p subshell is next. compute the ratio, verify that                 !/*sh         71*/
!        electrons actually in the subshell, if not then bypass                 !/*sh         72*/
!        computation.                                                           !/*sh         73*/
!                                                                               !/*sh         74*/
      if ((eshel(3,iz).ne.0.0).and.(eshel(5,iz).ne.0.0)) then                   !/*sh         75*/
        ratio=eshel(3,iz)/eshel(5,iz)                                           !/*sh         76*/
!                                                                               !/*sh         77*/
!        then compute the scaled beta squared value.                            !/*sh         78*/
!                                                                               !/*sh         79*/
        bsqscl=(betasq*ratio)/fudge                                             !/*sh         80*/
!                                                                               !/*sh         81*/
!        check scaled beta squared value and then compute the                   !/*sh         82*/
!        actual m shell, p subshell correction.                                 !/*sh         83*/
!                                                                               !/*sh         84*/
        if (bsqscl.le.0.99) cm2=(shel(5,iz)/shel(3,iz))*(1.0-betasq)            !/*sh         85*/
     &  *subb2l(z,bsqscl)/(1.0-bsqscl)                                          !/*sh         86*/
!                                                                               !/*sh         87*/
        if (cm2.lt.0.0) cm2=1.5*cm2                                             !/*sh         88*/
      endif                                                                     !/*sh         89*/
!                                                                               !/*sh         90*/
!        the d subshell is last. compute the ratio, verify that                 !/*sh         91*/
!        electrons actually in the subshell, if not then bypass                 !/*sh         92*/
!        computation.                                                           !/*sh         93*/
!                                                                               !/*sh         94*/
      if ((eshel(3,iz).ne.0.0).and.(eshel(6,iz).ne.0.0)) then                   !/*sh         95*/
        ratio=eshel(3,iz)/eshel(6,iz)                                           !/*sh         96*/
!                                                                               !/*sh         97*/
!        then compute the scaled beta squared value.                            !/*sh         98*/
!                                                                               !/*sh         99*/
        bsqscl=(betasq*ratio)/fudge                                             !/*sh        100*/
!                                                                               !/*sh        101*/
!        check scaled beta squared value and then compute the                   !/*sh        102*/
!        actual m shell, d subshell correction.                                 !/*sh        103*/
!                                                                               !/*sh        104*/
        if (bsqscl.le.0.99) cm3=(shel(6,iz)/shel(3,iz))*(1.0-betasq)            !/*sh        105*/
     &  *subb2l(z,bsqscl)/(1.0-bsqscl)                                          !/*sh        106*/
!                                                                               !/*sh        107*/
        if (cm3.lt.0.0) cm3=1.5*cm3                                             !/*sh        108*/
      endif                                                                     !/*sh        109*/
!                                                                               !/*sh        110*/
!        compute the total scaled m shell by adding up the subshell             !/*sh        111*/
!        corrections.                                                           !/*sh        112*/
!                                                                               !/*sh        113*/
      shelsm=cm1+cm2+cm3                                                        !/*sh        114*/
!                                                                               !/*sh        115*/
!        write out the author and reference information to                      !/*sh        116*/
!        scratch file if ptraut is true.                                        !/*sh        117*/
!                                                                               !/*sh        118*/
      if (ptraut) write (10,10)                                                 !/*sh        119*/
!                                                                               !/*sh        120*/
      return                                                                    !/*sh        121*/
!                                                                               !/*sh        122*/
   10 format ('m-shell - scaled subshell method.')                              !/*sh        123*/
      end                                                                       !/*sh        124*/

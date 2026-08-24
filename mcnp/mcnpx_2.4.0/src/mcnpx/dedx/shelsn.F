!     $Id: shelsn.F,v 1.4 2002/03/27 18:14:35 gwm Exp $                       !/*sh          1*/
      function shelsn (z,betasq,cn1,cn2,cn3,cn4,ptraut)                         !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the n-shell correction               !/*sh          5*/
!                   using the scaled method. this method involves               !/*sh          6*/
!                   scaling the bichsel m shell correction to produce           !/*sh          7*/
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
!                   ptraut  - logical variable, if true then author and         !/*sh         22*/
!                            reference information written to scratch fi        !/*sh         23*/
!                                                                               !/*sh         24*/
!        outputs :  shelsn - the n-shell correction as computed using           !/*sh         25*/
!                            scaling method.                                    !/*sh         26*/
!                   cn1    - s subshell correction.                             !/*sh         27*/
!                   cn2    - p subshell correction.                             !/*sh         28*/
!                   cn3    - d subshell correction.                             !/*sh         29*/
!                   cn4    - f subshell correction.                             !/*sh         30*/
!                                                                               !/*sh         31*/
!        errors :   none.                                                       !/*sh         32*/
!                                                                               !/*sh         33*/
#include <dedx/block8.h>                                                        /*!sh         34*/
      logical ptraut                                                            !/*sh         35*/
!                                                                               !/*sh         36*/
!        initialize the shell and subshell corrections to zero.                 !/*sh         37*/
!                                                                               !/*sh         38*/
      iz=nint(z)                                                                !/*sh         39*/
      cn1=0.0                                                                   !/*sh         40*/
      cn2=0.0                                                                   !/*sh         41*/
      cn3=0.0                                                                   !/*sh         42*/
      cn4=0.0                                                                   !/*sh         43*/
      shelsn=0.0                                                                !/*sh         44*/
!                                                                               !/*sh         45*/
!        set the n-shell scaling factor.                                        !/*sh         46*/
!                                                                               !/*sh         47*/
      fudge=1.8                                                                 !/*sh         48*/
!                                                                               !/*sh         49*/
!        compute each of the subshell corrections separately, using             !/*sh         50*/
!        the scaling method based upon the bichsel m shell.                     !/*sh         51*/
!                                                                               !/*sh         52*/
!        the s subshell is first. compute the ratio, verify that                !/*sh         53*/
!        electrons actually in the subshell, if not then bypass                 !/*sh         54*/
!        computation.                                                           !/*sh         55*/
!                                                                               !/*sh         56*/
      if ((eshel(4,iz).ne.0.0).and.(eshel(7,iz).ne.0.0)) then                   !/*sh         57*/
        ratio=eshel(4,iz)/eshel(7,iz)                                           !/*sh         58*/
!                                                                               !/*sh         59*/
!        then compute the scaled beta squared value.                            !/*sh         60*/
!                                                                               !/*sh         61*/
        bsqscl=(betasq*ratio)/fudge                                             !/*sh         62*/
!                                                                               !/*sh         63*/
!        check scaled beta squared value and then compute the                   !/*sh         64*/
!        actual n shell, s subshell correction.                                 !/*sh         65*/
!                                                                               !/*sh         66*/
        if (bsqscl.le.0.99) cn1=(shel(7,iz)/shel(4,iz))*(1.0-betasq)            !/*sh         67*/
     &  *subb1m(z,bsqscl)/(1.0-bsqscl)                                          !/*sh         68*/
!                                                                               !/*sh         69*/
        if (cn1.lt.0.0) cn1=1.0*cn1                                             !/*sh         70*/
      endif                                                                     !/*sh         71*/
!                                                                               !/*sh         72*/
!        the p subshell is next. compute the ratio, verify that                 !/*sh         73*/
!        electrons actually in the subshell, if not then bypass                 !/*sh         74*/
!        computation.                                                           !/*sh         75*/
!                                                                               !/*sh         76*/
      if ((eshel(5,iz).ne.0.0).and.(eshel(8,iz).ne.0.0)) then                   !/*sh         77*/
        ratio=eshel(5,iz)/eshel(8,iz)                                           !/*sh         78*/
!                                                                               !/*sh         79*/
!        then compute the scaled beta squared value.                            !/*sh         80*/
!                                                                               !/*sh         81*/
        bsqscl=(betasq*ratio)/fudge                                             !/*sh         82*/
!                                                                               !/*sh         83*/
!        check scaled beta squared value and then compute the                   !/*sh         84*/
!        actual n shell, p subshell correction.                                 !/*sh         85*/
!                                                                               !/*sh         86*/
        if (bsqscl.le.0.99) cn2=(shel(8,iz)/shel(5,iz))*(1.0-betasq)            !/*sh         87*/
     &  *subb2m(z,bsqscl)/(1.0-bsqscl)                                          !/*sh         88*/
!                                                                               !/*sh         89*/
        if (cn2.lt.0.0) cn2=1.0*cn2                                             !/*sh         90*/
      endif                                                                     !/*sh         91*/
!                                                                               !/*sh         92*/
!        the d subshell is next. compute the ratio, verify that                 !/*sh         93*/
!        electrons actually in the subshell, if not then bypass                 !/*sh         94*/
!        computation.                                                           !/*sh         95*/
!                                                                               !/*sh         96*/
      if ((eshel(6,iz).ne.0.0).and.(eshel(9,iz).ne.0.0)) then                   !/*sh         97*/
        ratio=eshel(6,iz)/eshel(9,iz)                                           !/*sh         98*/
!                                                                               !/*sh         99*/
!        then compute the scaled beta squared value.                            !/*sh        100*/
!                                                                               !/*sh        101*/
        bsqscl=(betasq*ratio)/fudge                                             !/*sh        102*/
!                                                                               !/*sh        103*/
!        check scaled beta squared value and then compute the                   !/*sh        104*/
!        actual n shell, d subshell correction.                                 !/*sh        105*/
!                                                                               !/*sh        106*/
        if (bsqscl.le.0.99) cn3=(shel(9,iz)/shel(6,iz))*(1.0-betasq)            !/*sh        107*/
     &  *subb3m(z,bsqscl)/(1.0-bsqscl)                                          !/*sh        108*/
!                                                                               !/*sh        109*/
        if (cn3.lt.0.0) cn3=1.0*cn3                                             !/*sh        110*/
      endif                                                                     !/*sh        111*/
!                                                                               !/*sh        112*/
!        the f subshell is last. compute the ratio, verify that                 !/*sh        113*/
!        electrons actually in the subshell, if not then bypass                 !/*sh        114*/
!        computation.                                                           !/*sh        115*/
!                                                                               !/*sh        116*/
      if ((eshel(6,iz).ne.0.0).and.(eshel(10,iz).ne.0.0)) then                  !/*sh        117*/
        ratio=eshel(6,iz)/eshel(10,iz)                                          !/*sh        118*/
!                                                                               !/*sh        119*/
!        then compute the scaled beta squared value.                            !/*sh        120*/
!                                                                               !/*sh        121*/
        bsqscl=(betasq*ratio)/fudge                                             !/*sh        122*/
!                                                                               !/*sh        123*/
!        check scaled beta squared value and then compute the                   !/*sh        124*/
!        actual n shell, f subshell correction.                                 !/*sh        125*/
!                                                                               !/*sh        126*/
        if (bsqscl.le.0.99) cn4=(shel(10,iz)/shel(6,iz))*(1.0-betasq)           !/*sh        127*/
     &  *subb3m(z,bsqscl)/(1.0-bsqscl)                                          !/*sh        128*/
!                                                                               !/*sh        129*/
        if (cn4.lt.0.0) cn4=1.0*cn4                                             !/*sh        130*/
      endif                                                                     !/*sh        131*/
!                                                                               !/*sh        132*/
!        compute the total scaled n shell by adding up the subshell             !/*sh        133*/
!        corrections.                                                           !/*sh        134*/
!                                                                               !/*sh        135*/
      shelsn=cn1+cn2+cn3+cn4                                                    !/*sh        136*/
!                                                                               !/*sh        137*/
!        write out the author and reference information to                      !/*sh        138*/
!        scratch file if ptraut flag set.                                       !/*sh        139*/
!                                                                               !/*sh        140*/
      if (ptraut) write (10,10)                                                 !/*sh        141*/
!                                                                               !/*sh        142*/
      return                                                                    !/*sh        143*/
!                                                                               !/*sh        144*/
   10 format ('n-shell - scaled subshell method.')                              !/*sh        145*/
      end                                                                       !/*sh        146*/

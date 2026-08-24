!     $Id: subshl.F,v 1.4 2002/03/27 18:14:38 gwm Exp $                       !/*su          1*/
      function subshl (z,beta,fudge,shel,shel2,shel3,eshel,esheln)              !/*su          2*/
#include <lcs/double.h>                                                         /*!su          3*/
!                                                                               !/*su          4*/
!        purpose : this routine computes the subshell correction values.        !/*su          5*/
!                  beta is squared and compared against a set maximum           !/*su          6*/
!                  value. if the squared beta is greater than the maximu        !/*su          7*/
!                  then return with the correction set to 0.0.                  !/*su          8*/
!                  if the squared beta is less than the maximum,                !/*su          9*/
!                  then the actual subshl calculation is done. the physi        !/*su         10*/
!                  in this routine is discribed in reference 133:               !/*su         11*/
!                                                                               !/*su         12*/
!                    h. bichsel, "higher shell corrections in stopping          !/*su         13*/
!                    power," univ of southern california linear                 !/*su         14*/
!                    accelerator group technical report no. 3 (1961).           !/*su         15*/
!                                                                               !/*su         16*/
!        author :  daniel pickens                                               !/*su         17*/
!                  computer sciences corp.                                      !/*su         18*/
!                  27 march 1983                                                !/*su         19*/
!                                                                               !/*su         20*/
!        inputs :  z      - the atomic number of the element.                   !/*su         21*/
!                  beta   - the converted kinetic energy to velocity.           !/*su         22*/
!                  fudge  -                                                     !/*su         23*/
!                  shel   - the number of elections in the                      !/*su         24*/
!                           specific subshell.                                  !/*su         25*/
!                  shel2  - the number of electrons in the l shell,             !/*su         26*/
!                           s subshell of the particular element.               !/*su         27*/
!                  shel3  - the number of electrons in the l shell,             !/*su         28*/
!                           p subshell of the particular element.               !/*su         29*/
!                  eshel  - the energy value of the specific                    !/*su         30*/
!                           subshell electrons.                                 !/*su         31*/
!                  esheln - the energy value of the l shell, p subshell         !/*su         32*/
!                           electrons for the particular element.               !/*su         33*/
!                                                                               !/*su         34*/
!        outputs : subshl - the calculated subshell correction value.           !/*su         35*/
!                                                                               !/*su         36*/
!        errors :  none.                                                        !/*su         37*/
!                                                                               !/*su         38*/
!        init print flag to zero so that the call to sheljl                     !/*su         39*/
!        does not write a comment line out saying :                             !/*su         40*/
!                                                                               !/*su         41*/
!          "l-shell - janni's total shell method used".                         !/*su         42*/
!                                                                               !/*su         43*/
      logical ptraut                                                            !/*su         44*/
!                                                                               !/*su         45*/
      iz=nint(z)                                                                !/*su         46*/
      ptraut=.false.                                                            !/*su         47*/
!                                                                               !/*su         48*/
!        initialize the routine output to 0.0 first.                            !/*su         49*/
!                                                                               !/*su         50*/
      subshl=0.0                                                                !/*su         51*/
!                                                                               !/*su         52*/
!        compute the ratio first, check for a zero in the denominator.          !/*su         53*/
!                                                                               !/*su         54*/
      ratio=0.0                                                                 !/*su         55*/
      if (eshel.ne.0.0) ratio=esheln/eshel                                      !/*su         56*/
!                                                                               !/*su         57*/
!         compute the scaled beta squared value that will be used               !/*su         58*/
!         throughout this routine.                                              !/*su         59*/
!                                                                               !/*su         60*/
      bsqscl=(beta*beta*ratio)/fudge                                            !/*su         61*/
!                                                                               !/*su         62*/
!        check for maximum scaled beta squared value, if greater than or        !/*su         63*/
!        equal to this value, then return.                                      !/*su         64*/
!                                                                               !/*su         65*/
      if (bsqscl.gt.0.99) return                                                !/*su         66*/
!                                                                               !/*su         67*/
!        calculate the correction value.                                        !/*su         68*/
!                                                                               !/*su         69*/
      subshl=(shel/(shel2+shel3))*(1.0-beta*beta)*sheljl(z,bsqscl,cl1           !/*su         70*/
     & ,cl2,ptraut)/(1.0-bsqscl)                                                !/*su         71*/
!                                                                               !/*su         72*/
      return                                                                    !/*su         73*/
      end                                                                       !/*su         74*/

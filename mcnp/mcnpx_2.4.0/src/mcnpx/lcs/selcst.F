!     $Id: selcst.F,v 1.5 2002/03/27 18:15:23 gwm Exp $                       !/*se          1*/
      subroutine selcst (atarg,p_scat,csthcm)                                   !/*se          2*/
!                                                                               !/*se          3*/
! concerning the implementation, please follow the instructions noted           !/*se          4*/
! in the hermes report (page 117 ff.). please note also, that the               !/*se          5*/
! common/comon/ has been rearranged into a set of common blocks, which          !/*se          6*/
! are marked with the comment %comon.. in the program text.                     !/*se          7*/
!                                                                               !/*se          8*/
! g.sterzenbach                                                                 !/*se          9*/
!                                                                               !/*se         10*/
!--------------------------------------------------------                       !/*se         11*/
! subname= selcst                       b.colborn                               !/*se         12*/
! SELCST calls...  RTOR%  SQRT%  SELJ1                                          !/*se         13*/
! SELCST called by...   TEST                                                    !/*se         14*/
!--------------------------------------------------------                       !/*se         15*/
#include <lcs/double.h>                                                         /*!se         16*/
      a13=atarg**dpth                                                           !/*se         17*/
      denom=dp2*(dp7095m6*a13*p_scat+dp1)**2                                    !/*se         18*/
      x2max=dp2*denom                                                           !/*se         19*/
      call selj1 (x2,x2max)                                                     !/*se         20*/
      csthcm=dp1-(x2/denom)                                                     !/*se         21*/
      return                                                                    !/*se         22*/
      end                                                                       !/*se         23*/

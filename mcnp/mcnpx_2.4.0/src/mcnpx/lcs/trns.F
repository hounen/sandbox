!     $Id: trns.F,v 1.4 2002/03/27 18:15:25 gwm Exp $                         !/*tr          1*/
      function trns (e,j,ipar,ia)                                               !/*tr          2*/
#include <lcs/double.h>                                                         /*!tr          3*/
#include <lcs/phtcom.h>                                                         /*!tr          4*/
!.....                                                                          !/*tr          5*/
!     TRNS computes log base 10 of transition probability                       !/*tr          6*/
!.....                                                                          !/*tr          7*/
      temp=dp3*log10(e)                                                         !/*tr          8*/
      x=c1*cra(ia)*e                                                            !/*tr          9*/
      if (j.gt.1) temp=temp+log10(fj(j))+(2*j-2)*(log10(x)-dp1)                 !/*tr         10*/
      fpar=dph*(1+ipar)                                                         !/*tr         11*/
      q=c2/cra(ia)                                                              !/*tr         12*/
      temp=temp+log10(fpar+10.0*q**2+0.01*fj(j+1)*x**2/fj(j))                   !/*tr         13*/
      trns=temp                                                                 !/*tr         14*/
      return                                                                    !/*tr         15*/
      end                                                                       !/*tr         16*/

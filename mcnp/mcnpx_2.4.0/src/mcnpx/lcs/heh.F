!     $Id: heh.F,v 1.4 2002/03/27 18:15:06 gwm Exp $                          !/*he          1*/
      subroutine heh (itinc,eoprs,it,ef,alp,bet,gam,wt,nofas)                   !/*he          2*/
! HEH        calls...  SQRT  NUCNUC  PINUC                                      !/*he          3*/
!     HEH    called by...   HYD                                                 !/*he          4*/
#include <lcs/double.h>                                                         /*!he          5*/
      dimension it(1), ef(1), alp(1), bet(1), gam(1), wt(1)                     !/*he          6*/
      it1=itinc+1                                                               !/*he          7*/
      einc=eoprs/1.d3                                                           !/*he          8*/
      rm=.94d0                                                                  !/*he          9*/
      if (it1.gt.2) rm=.139d0                                                   !/*he         10*/
      p0=sqrt(einc*einc+dp2*rm*einc)                                            !/*he         11*/
      if (it1.gt.2) go to 10                                                    !/*he         12*/
      call nucnuc (einc,p0,nofas,it(1),ef(1),alp(1),bet(1),gam(1),wt(1)         !/*he         13*/
     & ,it1)                                                                    !/*he         14*/
      go to 20                                                                  !/*he         15*/
   10 call pinuc (einc,p0,nofas,it(1),ef(1),alp(1),bet(1),gam(1),wt(1)          !/*he         16*/
     & ,it1)                                                                    !/*he         17*/
   20 return                                                                    !/*he         18*/
      end                                                                       !/*he         19*/

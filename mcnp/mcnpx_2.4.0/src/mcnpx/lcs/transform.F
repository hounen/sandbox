!     $Id: transform.F,v 1.4 2002/03/27 18:15:25 gwm Exp $                    !/*tr          1*/
      subroutine transform (u,v,w)                                              !/*tr          2*/
#include <lcs/double.h>                                                         /*!tr          3*/
#include <lcs/lptS.h>                                                           /*!tr          4*/
#include <lcs/rucom.h>                                                          /*!tr          5*/
!=======================================================================        !/*tr          6*/
!------ set up transformation to lab coordinates                                !/*tr          7*/
!----- used in DATAHI, DATALO, and ERUP                                         !/*tr          8*/
      rt=u*u+v*v                                                                !/*tr          9*/
      if (rt.gt.dp0) then                                                       !/*tr         10*/
        rt=sqrt(rt)                                                             !/*tr         11*/
        costh=w                                                                 !/*tr         12*/
        sinth=rt                                                                !/*tr         13*/
        cosphi=u/rt                                                             !/*tr         14*/
        sinphi=v/rt                                                             !/*tr         15*/
      else                                                                      !/*tr         16*/
        costh=sign(dp1,w)                                                       !/*tr         17*/
        sinth=dp0                                                               !/*tr         18*/
        cosphi=dp1                                                              !/*tr         19*/
        sinphi=dp0                                                              !/*tr         20*/
      endif                                                                     !/*tr         21*/
      return                                                                    !/*tr         22*/
      end                                                                       !/*tr         23*/

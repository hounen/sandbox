!     $Id: trafo.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                        !/*tr          1*/
      subroutine trafo (gam,bgam,cx,cy,cz,cod,cof,sif,p,ecm,pl,cxl,cyl          !/*tr          2*/
     & ,czl,el)                                                                 !/*tr          3*/
!                                                                     *         !/*tr          4*/
!---------------------------------------------------------------------*         !/*tr          5*/
!                                                                     *         !/*tr          6*/
#include <lcs/double.h>                                                         /*!tr          7*/
#include <fluka89/flprm1.h>                                                     /*!tr          8*/
#include <fluka89/flprm2.h>                                                     /*!tr          9*/
#include <lcs/inout.h>                                                          /*!tr         10*/
      sid=sqrt(1.d0-cod*cod)                                                    !/*tr         11*/
      plx=p*sid*cof                                                             !/*tr         12*/
      ply=p*sid*sif                                                             !/*tr         13*/
      pcmz=p*cod                                                                !/*tr         14*/
      plz=gam*pcmz+bgam*ecm                                                     !/*tr         15*/
      pl=sqrt(plx*plx+ply*ply+plz*plz)                                          !/*tr         16*/
      el=gam*ecm+bgam*pcmz                                                      !/*tr         17*/
      coz=plz/pl                                                                !/*tr         18*/
      siz=sqrt(1.d0-coz**2)                                                     !/*tr         19*/
      call ttrans (cx,cy,cz,coz,siz,sif,cof,cxl,cyl,czl)                        !/*tr         20*/
      return                                                                    !/*tr         21*/
      end                                                                       !/*tr         22*/

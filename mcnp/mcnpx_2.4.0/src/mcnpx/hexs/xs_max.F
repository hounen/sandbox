!     $Id: xs_max.F,v 1.4 2002/03/27 18:14:51 gwm Exp $                       !/*xs          1*/
      subroutine xs_max (ie,ityp,a,fmf_,xs_abs,xs_els,xs_tot)                   !/*xs          2*/
#include <lcs/double.h>                                                         /*!xs          3*/
#include <lcs/lptS.h>                                                           /*!xs          4*/
#include <lcs/xsmax.h>                                                          /*!xs          5*/
      dimension imxs(lpt)                                                       !/*xs          6*/
      data imxs /1,2,3,0,5,0,0,10,11,12,13,0,6,0,0,7,8,9,0,0,0,0/               !/*xs          7*/
      i=imxs(ityp)                                                              !/*xs          8*/
      j=nint(a)                                                                 !/*xs          9*/
      if (i.eq.0) then                                                          !/*xs         10*/
        xs_abs=dp0                                                              !/*xs         11*/
        xs_els=dp0                                                              !/*xs         12*/
        xs_tot=dp0                                                              !/*xs         13*/
      elseif (a.eq.dp1) then                                                    !/*xs         14*/
        xs_abs=fmf_*xmaxa(i,j)                                                  !/*xs         15*/
        xs_els=fmf_*xmaxs(i,j)                                                  !/*xs         16*/
        xs_tot=fmf_*xmaxt(i,j)                                                  !/*xs         17*/
      elseif (ie.eq.0.or.ityp.eq.1.and.ie.le.1.or.ityp.gt.2.and.ie.le.2)        !/*xs         18*/
     & then                                                                     !/*xs         19*/
        xs_abs=fmf_*xmaxa(i,j)                                                  !/*xs         20*/
        xs_els=dp0                                                              !/*xs         21*/
        xs_tot=xs_abs                                                           !/*xs         22*/
      else                                                                      !/*xs         23*/
        xs_abs=fmf_*xmaxa(i,j)                                                  !/*xs         24*/
        xs_els=fmf_*xmaxs(i,j)                                                  !/*xs         25*/
        xs_tot=fmf_*xmaxt(i,j)                                                  !/*xs         26*/
      endif                                                                     !/*xs         27*/
      xs_tot=xs_abs+xs_els                                                      !/*xs         28*/
      return                                                                    !/*xs         29*/
      end                                                                       !/*xs         30*/

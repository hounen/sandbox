!     $Id: impuls.F,v 1.4 2002/03/27 18:14:45 gwm Exp $                       !/*im          1*/
      subroutine impuls (he,hma,hps,hpx,hpy,hpz,lt,ll,b3)                       !/*im          2*/
!                                                                     *         !/*im          3*/
!---------------------------------------------------------------------*         !/*im          4*/
!                                                                     *         !/*im          5*/
#include <lcs/double.h>                                                         /*!im          6*/
#include <fluka89/flprm1.h>                                                     /*!im          7*/
#include <fluka89/flprm2.h>                                                     /*!im          8*/
#include <lcs/inout.h>                                                          /*!im          9*/
      esmax=he-hma                                                              !/*im         10*/
   10 continue                                                                  !/*im         11*/
      if (esmax.le.0.05d0) then                                                 !/*im         12*/
        hps=sqrt(he**2-hma**2)                                                  !/*im         13*/
        hpz=0.d0                                                                !/*im         14*/
      else                                                                      !/*im         15*/
        x=rang()                                                                !/*im         16*/
        y=rang()                                                                !/*im         17*/
        es=-2.d0/(b3**2)*log(x*y)                                               !/*im         18*/
        if (es.gt.esmax) go to 10                                               !/*im         19*/
        hps=es**2+2.d0*es*hma                                                   !/*im         20*/
        if (ll.eq.0) y1=1.d0                                                    !/*im         21*/
        if (ll.eq.1) y1=-1.d0                                                   !/*im         22*/
        hpz=sqrt(he**2-hma**2-hps)*y1                                           !/*im         23*/
        hps=sqrt(hps)                                                           !/*im         24*/
      endif                                                                     !/*im         25*/
      call sfecfe (sfe,cfe)                                                     !/*im         26*/
      sip=sfe                                                                   !/*im         27*/
      cop=cfe                                                                   !/*im         28*/
      hpx=hps*cop                                                               !/*im         29*/
      hpy=hps*sip                                                               !/*im         30*/
      if (lt.eq.0) go to 20                                                     !/*im         31*/
      write (io,30) hps,hpx,hpy,hpz,he,hma,ll                                   !/*im         32*/
   20 continue                                                                  !/*im         33*/
      return                                                                    !/*im         34*/
!                                                                               !/*im         35*/
   30 format (1h0,19hps,px,py,pz,e,m,ll=,6f8.4,i3)                              !/*im         36*/
      end                                                                       !/*im         37*/

!     $Id: sliq.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                         !/*sl          1*/
      function sliq (ac,ab,zc,zb,xnc,xnb)                                       !/*sl          2*/
#include <lcs/double.h>                                                         /*!sl          3*/
      xac=ac**0.3333333d0                                                       !/*sl          4*/
      xab=ab**0.3333333d0                                                       !/*sl          5*/
      t1=(xnc-zc)**2/ac                                                         !/*sl          6*/
      t2=(xnb-zb)**2/ab                                                         !/*sl          7*/
      t3=(xnc-zc)**2/(ac*xac)                                                   !/*sl          8*/
      t4=(xnb-zb)**2/(ab*xab)                                                   !/*sl          9*/
      t5=zc**2/xac                                                              !/*sl         10*/
      t6=zb**2/xab                                                              !/*sl         11*/
      t7=zc**2/ac                                                               !/*sl         12*/
      t8=zb**2/ab                                                               !/*sl         13*/
      sliq=15.68d0*(ac-ab)-28.07d0*(t1-t2)-18.56d0*(xac**2-xab**2)+33.22        !/*sl         14*/
     & d0*(t3-t4)-0.717d0*(t5-t6)+1.211d0*(t7-t8)                               !/*sl         15*/
      return                                                                    !/*sl         16*/
      end                                                                       !/*sl         17*/

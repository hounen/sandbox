!     $Id: xsecl.F,v 1.4 2002/03/27 18:15:27 gwm Exp $                        !/*xs          1*/
      function xsecl (id,itp,ec)                                                !/*xs          2*/
! XSEC                                                                          !/*xs          3*/
!     XSEC   called by...   PCOL                                                !/*xs          4*/
#include <lcs/double.h>                                                         /*!xs          5*/
#include <lcs/tpcrs.h>                                                          /*!xs          6*/
!   id =   1          2         3         4                                     !/*xs          7*/
!      sngl prod  dble prod  elastic  exchange                                  !/*xs          8*/
!  itp =   1          2         3         4                                     !/*xs          9*/
!        prot       neut       pi+       pi-                                    !/*xs         10*/
      et=eth(id,itp)                                                            !/*xs         11*/
      lc=locx(id,itp)                                                           !/*xs         12*/
! --- Condition applies only to hydrogen.                                       !/*xs         13*/
      if (ec.lt.et.or.lc.le.0) then                                             !/*xs         14*/
         xsecl=dp0                                                              !/*xs         15*/
      else                                                                      !/*xs         16*/
         i=int((ec-et)/dexs+dp1)                                                !/*xs         17*/
         e=real(i-1)*dexs+et                                                    !/*xs         18*/
         temp=cs(i+lc)                                                          !/*xs         19*/
         xsecl=temp+(ec-e)/dexs*(cs(i+1+lc)-temp)                               !/*xs         20*/
      endif                                                                     !/*xs         21*/
      return                                                                    !/*xs         22*/
      end                                                                       !/*xs         23*/

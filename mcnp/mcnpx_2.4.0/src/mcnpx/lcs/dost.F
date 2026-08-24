!     $Id: dost.F,v 1.4 2002/03/27 18:15:00 gwm Exp $                         !/*do          1*/
      function dost (i,z)                                                       !/*do          2*/
#include <lcs/double.h>                                                         /*!do          3*/
#include <lcs/lptS.h>                                                           /*!do          4*/
#include <lcs/ercom.h>                                                          /*!do          5*/
      if (z-70.d0) 30,10,10                                                     !/*do          6*/
   10 dost=tz0(i,7)                                                             !/*do          7*/
   20 return                                                                    !/*do          8*/
   30 if (z-10.d0) 40,40,50                                                     !/*do          9*/
   40 dost=tz0(i,1)                                                             !/*do         10*/
      go to 20                                                                  !/*do         11*/
   50 n=.1d0*z+dp1                                                              !/*do         12*/
      x=10*n                                                                    !/*do         13*/
      x=(x-z)*.1d0                                                              !/*do         14*/
      dost=x*tz0(i,n-1)+(dp1-x)*tz0(i,n)                                        !/*do         15*/
      go to 20                                                                  !/*do         16*/
      end                                                                       !/*do         17*/

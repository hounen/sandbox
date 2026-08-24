!     $Id: rou16.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rou16 (t)                                                      !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
      dimension t(6426)                                                         !/*ro          4*/
#include <lcs/btcom.h>                                                          /*!ro          5*/
      call mud (snt,inpt)                                                       !/*ro          6*/
      call crdet (51,t(1),rlke)                                                 !/*ro          7*/
      i2=0                                                                      !/*ro          8*/
      cst=crdt(2)-abs(snt*(crdt(2)-crdt(1)))                                    !/*ro          9*/
      if (i3-2) 10,30,40                                                        !/*ro         10*/
   10 i3=-1                                                                     !/*ro         11*/
   20 return                                                                    !/*ro         12*/
   30 i3=0                                                                      !/*ro         13*/
      go to 20                                                                  !/*ro         14*/
   40 i3=1                                                                      !/*ro         15*/
      go to 20                                                                  !/*ro         16*/
      end                                                                       !/*ro         17*/

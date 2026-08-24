!     $Id: rou12.F,v 1.4 2002/03/27 18:15:20 gwm Exp $                        !/*ro          1*/
      subroutine rou12                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      i3=0                                                                      !/*ro          5*/
      call azirn (sopc,sops)                                                    !/*ro          6*/
      call coll (0)                                                             !/*ro          7*/
      if (col(15)) 10,30,10                                                     !/*ro          8*/
   10 i3=-1                                                                     !/*ro          9*/
   20 return                                                                    !/*ro         10*/
   30 if (ke) 10,50,40                                                          !/*ro         11*/
   40 com=(e(4)-dncms)/rcpmv                                                    !/*ro         12*/
      go to 60                                                                  !/*ro         13*/
   50 call cole4                                                                !/*ro         14*/
   60 i1=-1                                                                     !/*ro         15*/
      value1=com                                                                !/*ro         16*/
      if (pt(14)-dp2) 70,20,20                                                  !/*ro         17*/
   70 i3=1                                                                      !/*ro         18*/
      go to 20                                                                  !/*ro         19*/
      end                                                                       !/*ro         20*/

!     $Id: fsgels.F,v 1.4 2002/03/27 18:15:04 gwm Exp $                       !/*fs          1*/
      function fsgels (ee,mell,esxl,esc,iemax,ie0)                              !/*fs          2*/
#include <lcs/double.h>                                                         /*!fs          3*/
      dimension esxl(iemax,1), esc(1)                                           !/*fs          4*/
      ie=ie0                                                                    !/*fs          5*/
      if (ie.eq.0) go to 10                                                     !/*fs          6*/
      if (ie.eq.iemax) go to 20                                                 !/*fs          7*/
      temp1=(ee-esc(ie))/(esc(ie+1)-esc(ie))                                    !/*fs          8*/
      temp2=esxl(ie,mell)                                                       !/*fs          9*/
      fsgels=temp1*(esxl(ie+1,mell)-temp2)+temp2                                !/*fs         10*/
      go to 30                                                                  !/*fs         11*/
   10 ie=1                                                                      !/*fs         12*/
   20 continue                                                                  !/*fs         13*/
      fsgels=esxl(ie,mell)                                                      !/*fs         14*/
   30 continue                                                                  !/*fs         15*/
      return                                                                    !/*fs         16*/
      end                                                                       !/*fs         17*/

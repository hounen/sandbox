!     $Id: fixe.F,v 1.4 2002/03/27 18:13:59 gwm Exp $                         !/*fi          1*/
      subroutine fixe (ergb,nerg)                                               !/*fi          2*/
#include <htape3x/com32b.h>                                                     /*!fi          3*/
      dimension ergb(nerg)                                                      !/*fi          4*/
      if (ergb(nerg).ne.dp0) go to 80                                           !/*fi          5*/
      if (ergb(1).lt.dp0.and.ergb(2).gt.dp0) go to 60                           !/*fi          6*/
      if (ergb(1).gt.dp0.and.ergb(2).eq.dp0) go to 40                           !/*fi          7*/
      do 10 i=1,nerg                                                            !/*fi          8*/
      j=nerg+1-i                                                                !/*fi          9*/
      if (ergb(j).gt.dp0) go to 20                                              !/*fi         10*/
   10 continue                                                                  !/*fi         11*/
      call errorf ('fixe.1  ')                                                  !/*fi         12*/
   20 continue                                                                  !/*fi         13*/
      dell=ergb(j)-ergb(j-1)                                                    !/*fi         14*/
      jj=j+1                                                                    !/*fi         15*/
      do 30 i=jj,nerg                                                           !/*fi         16*/
   30 ergb(i)=ergb(i-1)+dell                                                    !/*fi         17*/
      go to 80                                                                  !/*fi         18*/
   40 continue                                                                  !/*fi         19*/
      do 50 i=2,nerg                                                            !/*fi         20*/
   50 ergb(i)=ergb(i-1)+ergb(1)                                                 !/*fi         21*/
      go to 80                                                                  !/*fi         22*/
   60 continue                                                                  !/*fi         23*/
      dell=ergb(1)                                                              !/*fi         24*/
      ergb(nerg)=ergb(2)                                                        !/*fi         25*/
      nn=nerg-1                                                                 !/*fi         26*/
      do 70 i=1,nn                                                              !/*fi         27*/
      j=nerg-i                                                                  !/*fi         28*/
   70 ergb(i)=ergb(nerg)*10.0**(dell*j)                                         !/*fi         29*/
   80 continue                                                                  !/*fi         30*/
      return                                                                    !/*fi         31*/
      end                                                                       !/*fi         32*/

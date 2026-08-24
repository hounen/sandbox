!     $Id: fix.F,v 1.5 2002/03/27 18:16:24 gwm Exp $                          !/*fi          1*/
      subroutine fix (base,y0,yy,nerg)                                          !/*fi          2*/
#if defined(HTPLOT) || defined(XSPLOT)                                          /*!fi          3*/
#include <htape3x/com32b.h>                                                     /*!fi          4*/
      real yy                                                                   !/*fi          5*/
      dimension y0(1), yy(1)                                                    !/*fi          6*/
      base=huge_float                                                           !/*fi          7*/
      do 10 j=1,nerg                                                            !/*fi          8*/
      if (y0(j).le.dp0) go to 10                                                !/*fi          9*/
      base=min(base,y0(j))                                                      !/*fi         10*/
   10 continue                                                                  !/*fi         11*/
      if (base.eq.huge_float) go to 40                                          !/*fi         12*/
      temp=log10(base)                                                          !/*fi         13*/
      jj=int(temp)-2                                                            !/*fi         14*/
      base=10.0**jj                                                             !/*fi         15*/
      entry fix1(base,y0,yy,nerg)                                               !/*fi         16*/
      do 20 j=1,nerg                                                            !/*fi         17*/
   20 y0(j)=max(base,y0(j))                                                     !/*fi         18*/
      yy(1)=base                                                                !/*fi         19*/
      yy(2*nerg+2)=base                                                         !/*fi         20*/
      do 30 i=1,nerg                                                            !/*fi         21*/
      j=2*i                                                                     !/*fi         22*/
      yy(j)=y0(i)                                                               !/*fi         23*/
      yy(j+1)=y0(i)                                                             !/*fi         24*/
   30 continue                                                                  !/*fi         25*/
      return                                                                    !/*fi         26*/
   40 continue                                                                  !/*fi         27*/
      base=-1.0                                                                 !/*fi         28*/
      return                                                                    !/*fi         29*/
#endif                                                                          /*!fi         30*/
      end                                                                       !/*fi         31*/

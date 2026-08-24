!     $Id: binin.F,v 1.4 2002/03/27 18:16:21 gwm Exp $                        !/*bi          1*/
      subroutine binin (k,l)                                                    !/*bi          2*/
#include <htape3x/com32b.h>                                                     /*!bi          3*/
#include <htape3x/big.h>                                                        /*!bi          4*/
#include <trx/gross.h>                                                          /*!bi          5*/
      read (30) k,l                                                             !/*bi          6*/
      read (30) iref                                                            !/*bi          7*/
      read (30) (izao(i),i=1,k)                                                 !/*bi          8*/
      read (30) (ipt(i),i=1,k)                                                  !/*bi          9*/
      read (30) (elo(i),i=1,l)                                                  !/*bi         10*/
      read (30) (isp(i),i=1,l)                                                  !/*bi         11*/
      read (30) (idk(i),i=1,l)                                                  !/*bi         12*/
      read (30) (tlev(i),i=1,l)                                                 !/*bi         13*/
      m=idk(l)                                                                  !/*bi         14*/
      read (30) (pmode(i),i=1,m)                                                !/*bi         15*/
      read (30) (lfin(i),i=1,m)                                                 !/*bi         16*/
      read (30,end=10) (lcl(i),i=1,l)                                           !/*bi         17*/
      return                                                                    !/*bi         18*/
   10 continue                                                                  !/*bi         19*/
      do 20 i=1,l                                                               !/*bi         20*/
   20 lcl(i)=0                                                                  !/*bi         21*/
      return                                                                    !/*bi         22*/
!                                                                               !/*bi         23*/
      end                                                                       !/*bi         24*/

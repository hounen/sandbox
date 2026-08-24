!     $Id: binout.F,v 1.4 2002/03/27 18:16:21 gwm Exp $                       !/*bi          1*/
      subroutine binout (k,l)                                                   !/*bi          2*/
#include <htape3x/com32b.h>                                                     /*!bi          3*/
#include <htape3x/big.h>                                                        /*!bi          4*/
#include <trx/gross.h>                                                          /*!bi          5*/
      write (30) k,l                                                            !/*bi          6*/
      write (30) iref                                                           !/*bi          7*/
      write (30) (izao(i),i=1,k)                                                !/*bi          8*/
      write (30) (ipt(i),i=1,k)                                                 !/*bi          9*/
      write (30) (elo(i),i=1,l)                                                 !/*bi         10*/
      write (30) (isp(i),i=1,l)                                                 !/*bi         11*/
      write (30) (idk(i),i=1,l)                                                 !/*bi         12*/
      write (30) (tlev(i),i=1,l)                                                !/*bi         13*/
      m=idk(l)                                                                  !/*bi         14*/
      write (30) (pmode(i),i=1,m)                                               !/*bi         15*/
      write (30) (lfin(i),i=1,m)                                                !/*bi         16*/
      write (30) (lcl(i),i=1,l)                                                 !/*bi         17*/
      return                                                                    !/*bi         18*/
      end                                                                       !/*bi         19*/

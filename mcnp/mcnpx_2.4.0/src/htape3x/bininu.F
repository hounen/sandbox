!     $Id: bininu.F,v 1.4 2002/03/27 18:13:56 gwm Exp $                       !/*bi          1*/
      subroutine bininu (k,l)                                                   !/*bi          2*/
! PHTLIB input routine for HTAPE3 use.                                          !/*bi          3*/
#include <htape3x/com32b.h>                                                     /*!bi          4*/
#include <htape3x/big.h>                                                        /*!bi          5*/
      read (30) k,l                                                             !/*bi          6*/
      read (30) iref                                                            !/*bi          7*/
      read (30) (izao(i),i=1,k)                                                 !/*bi          8*/
      read (30) (ipt(i),i=1,k)                                                  !/*bi          9*/
      read (30) (elo(i),i=1,l)                                                  !/*bi         10*/
      read (30) (isp(i),i=1,l)                                                  !/*bi         11*/
      read (30) (idk(i),i=1,l)                                                  !/*bi         12*/
      read (30) (tlev(i),i=1,l)                                                 !/*bi         13*/
!                                                                               !/*bi         14*/
!     Skip PMODE and LFIN records                                               !/*bi         15*/
      read (30) dumm                                                            !/*bi         16*/
      read (30) dumm                                                            !/*bi         17*/
!                                                                               !/*bi         18*/
      read (30,end=10) (lcl(i),i=1,l)                                           !/*bi         19*/
      return                                                                    !/*bi         20*/
   10 continue                                                                  !/*bi         21*/
      do 20 i=1,l                                                               !/*bi         22*/
   20 lcl(i)=0                                                                  !/*bi         23*/
      return                                                                    !/*bi         24*/
!                                                                               !/*bi         25*/
      end                                                                       !/*bi         26*/

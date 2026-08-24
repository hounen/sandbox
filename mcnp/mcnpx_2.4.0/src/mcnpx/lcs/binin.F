!     $Id: binin.F,v 1.6 2002/06/14 17:05:42 jxh Exp $                          !/*bi          1*/
      subroutine binin (k,l)                                                    !/*bi          2*/
#include <lcs/double.h>                                                         /*!bi          3*/
#include <lcs/inout.h>                                                          /*!bi          4*/
#include <lcs/charla.h>                                                         /*!bi          5*/
#include <lcs/phtdat.h>                                                         /*!bi          6*/
#include <lcs/phtcom.h>                                                         /*!bi          7*/
      data lu /29/                                                              !/*bi          8*/
!                                                                               !/*bi          9*/
! The open either succeeds or exits with an error.                              !/*bi         10*/
!                                                                               !/*bi         11*/
      call openlcsdata (lu, "binin", "phtlib", 1)                               !/*bi         12*/
      read (lu) k,l                                                             !/*bi         14*/
      read (lu) iref                                                            !/*bi         15*/
      read (lu) (izao(i),i=1,k)                                                 !/*bi         16*/
      read (lu) (ipt(i),i=1,k)                                                  !/*bi         17*/
      read (lu) (elo(i),i=1,l)                                                  !/*bi         18*/
      read (lu) (isp(i),i=1,l)                                                  !/*bi         19*/
      read (lu) (idk(i),i=1,l)                                                  !/*bi         20*/
      read (lu) (tlev(i),i=1,l)                                                 !/*bi         21*/
      m=idk(l)                                                                  !/*bi         22*/
      read (lu) (pmode(i),i=1,m)                                                !/*bi         23*/
      read (lu) (lfin(i),i=1,m)                                                 !/*bi         24*/
      close (lu)                                                                !/*bi         25*/
      return                                                                    !/*bi         26*/
!                                                                               !/*bi         27*/
      end                                                                       !/*bi         28*/

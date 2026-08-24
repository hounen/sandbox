!     $Id: hebd.F,v 1.4 2002/03/27 18:15:06 gwm Exp $                         !/*he          1*/
      block data hebd                                                           !/*he          2*/
! BTBD                   called by...   SKALE                                   !/*he          3*/
#include <lcs/double.h>                                                         /*!he          4*/
#include <lcs/mcm.h>                                                            /*!he          5*/
      data count0, count1, count2, count3, count4, count5, tarmat /7*dp0        !/*he          6*/
     & /                                                                        !/*he          7*/
      data amornt, amrnst, estart, zkz, ernprt, ernt, check7 /7*dp0/            !/*he          8*/
      data einct, anumbe, yield, avere /40*dp0/                                 !/*he          9*/
      data btot, btotpr, check8 /3*dp0/, nttme, nmlteo /2*0/                    !/*he         10*/
      data fug /2*dp0,3*dp1/                                                    !/*he         11*/
      data resmas /940.075d0,940.075d0,139.89d0,135.143d0,139.89d0/             !/*he         12*/
      end                                                                       !/*he         13*/

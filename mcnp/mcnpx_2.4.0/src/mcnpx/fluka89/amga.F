!     $Id: amga.F,v 1.4 2002/03/27 18:14:39 gwm Exp $                         !/*am          1*/
      function amga (it)                                                        !/*am          2*/
!                                                                     *         !/*am          3*/
!---------------------------------------------------------------------*         !/*am          4*/
!                                                                     *         !/*am          5*/
#include <lcs/double.h>                                                         /*!am          6*/
#include <fluka89/flprm1.h>                                                     /*!am          7*/
#include <fluka89/flprm2.h>                                                     /*!am          8*/
#include <lcs/inout.h>                                                          /*!am          9*/
#include <fluka89/ablts2.h>                                                     /*!am         10*/
#include <fluka89/gamrd2.h>                                                     /*!am         11*/
      dimension gasuni(14)                                                      !/*am         12*/
      data gasuni /-1.d0,-.98d0,-.95d0,-.87d0,-.72d0,-.48d0,-.17d0,.17d0        !/*am         13*/
     & ,.48d0,.72d0,.87d0,.95d0,.98d0,1.d0/                                     !/*am         14*/
      data gauno /2.352d0/                                                      !/*am         15*/
      data gaunon /2.4d0/                                                       !/*am         16*/
      data io1 /14/                                                             !/*am         17*/
      data nstab /23/                                                           !/*am         18*/
      i=1                                                                       !/*am         19*/
      if (it.le.0) go to 30                                                     !/*am         20*/
      if (it.le.nstab) go to 20                                                 !/*am         21*/
      dgauni=gauno*gaunon/(io1-1.d0)                                            !/*am         22*/
      vv=rang()                                                                 !/*am         23*/
      vv=vv*2.d0-1.d0+1.d-16                                                    !/*am         24*/
   10 continue                                                                  !/*am         25*/
      vo=gasuni(i)                                                              !/*am         26*/
      i=i+1                                                                     !/*am         27*/
      v1=gasuni(i)                                                              !/*am         28*/
      if (vv.gt.v1) go to 10                                                    !/*am         29*/
      uniga=dgauni*(i-2.d0+(vv-vo+1.d-16)/(v1-vo)-(io1-1.d0)*.5d0)              !/*am         30*/
      dam=ga(it)*uniga/gauno                                                    !/*am         31*/
      aam=amm(it)+dam                                                           !/*am         32*/
      amga=aam                                                                  !/*am         33*/
      return                                                                    !/*am         34*/
   20 continue                                                                  !/*am         35*/
      amga=amm(it)                                                              !/*am         36*/
      return                                                                    !/*am         37*/
   30 continue                                                                  !/*am         38*/
      amga=0.d0                                                                 !/*am         39*/
      return                                                                    !/*am         40*/
      end                                                                       !/*am         41*/

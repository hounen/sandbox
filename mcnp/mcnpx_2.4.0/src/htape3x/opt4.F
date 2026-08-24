!     $Id: opt4.F,v 1.5 2002/03/27 18:14:01 gwm Exp $                         !/*op          1*/
      subroutine opt4 (hold1,nerg,nerg1)                                        !/*op          2*/
#ifdef LCS                                                                      /*!op          3*/
#include <htape3x/com32b.h>                                                     /*!op          4*/
#include <htape3x/label.h>                                                      /*!op          5*/
#include <htape3x/contrl.h>                                                     /*!op          6*/
#include <htape3x/bndy.h>                                                       /*!op          7*/
#include <htape3x/ctconv.h>                                                     /*!op          8*/
      dimension hold1(nerg1,1)                                                  !/*op          9*/
      dimension nc4(8)                                                          !/*op         10*/
      data nc4 /0,1,0,1,1,1,1,0/                                                !/*op         11*/
!----                                                                           !/*op         12*/
! tally track length estimate for the flux-                                     !/*op         13*/
!     neutrons only with no time binning                                        !/*op         14*/
!----                                                                           !/*op         15*/
      if (int(tip).ne.1) go to 60                                               !/*op         16*/
      if (ncol.eq.1) go to 60                                                   !/*op         17*/
   10 continue                                                                  !/*op         18*/
      ia=bold                                                                   !/*op         19*/
      do 20 is=1,nparm                                                          !/*op         20*/
      if (ia.eq.lparm(is)) go to 30                                             !/*op         21*/
   20 continue                                                                  !/*op         22*/
      go to 60                                                                  !/*op         23*/
   30 continue                                                                  !/*op         24*/
      l=is                                                                      !/*op         25*/
      if (lbunch) l=1                                                           !/*op         26*/
      do 40 ie=1,nerg                                                           !/*op         27*/
      if (ec.le.ergb(ie)) go to 50                                              !/*op         28*/
   40 continue                                                                  !/*op         29*/
      go to 60                                                                  !/*op         30*/
   50 continue                                                                  !/*op         31*/
      temp=(xc-x)**2+(yc-y)**2+(zc-z)**2                                        !/*op         32*/
      del=wt*sqrt(temp)                                                         !/*op         33*/
      if (ifeng) del=del*ec                                                     !/*op         34*/
      if (itmult.gt.0) del=del*ec                                               !/*op         35*/
      if (irsp.gt.0) del=del*resp(ec)                                           !/*op         36*/
      if (irsp.lt.0) del=del/resp(ec)                                           !/*op         37*/
      hold1(ie,l)=hold1(ie,l)+del                                               !/*op         38*/
      if (lfe) hold1(nerg1,l)=hold1(nerg1,l)+del                                !/*op         39*/
   60 continue                                                                  !/*op         40*/
      call srchnp (nc4)                                                         !/*op         41*/
      if (ncol.le.0) go to 70                                                   !/*op         42*/
      if (nocas.eq.nocas0) go to 10                                             !/*op         43*/
   70 continue                                                                  !/*op         44*/
#endif                                                                          /*!op         45*/
      return                                                                    !/*op         46*/
      end                                                                       !/*op         47*/

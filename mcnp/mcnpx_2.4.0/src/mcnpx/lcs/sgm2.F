!     $Id: sgm2.F,v 1.4 2002/03/27 18:15:23 gwm Exp $                         !/*sg          1*/
      subroutine sgm2 (it,em,i,e,s)                                             !/*sg          2*/
! SGM        calls...  ERRORF  ALOG  EXP                                        !/*sg          3*/
!     SGM    called by...   HYD                                                 !/*sg          4*/
#include <lcs/double.h>                                                         /*!sg          5*/
#include <lcs/inout.h>                                                          /*!sg          6*/
#include <lcs/tpcrs.h>                                                          /*!sg          7*/
      dimension ppnp(9,2), enrgy(9)                                             !/*sg          8*/
      data enrgy /1.d0,3.d0,5.d0,8.d0,10.d0,12.5d0,15.d0,17.5d0,20.d0/          !/*sg          9*/
      data ppnp /1.20d0,.890d0,.620d0,.392d0,.310d0,.250d0,.208d0,.178d0        !/*sg         10*/
     & ,.155d0,4.25d0,2.28d0,1.62d0,1.14d0,.940d0,.765d0,.645d0,.555d0,.        !/*sg         11*/
     & 480/                                                                     !/*sg         12*/
      data iemx /9/                                                             !/*sg         13*/
!                                                                               !/*sg         14*/
      if (em.lt.2.d1.and.it.le.2) go to 10                                      !/*sg         15*/
      i=int(em/dexs+dp1)                                                        !/*sg         16*/
      e=real(i-1)*dexs                                                          !/*sg         17*/
      if (it.le.2) then                                                         !/*sg         18*/
        i=min(i,175)                                                            !/*sg         19*/
        s=fnpsg(it,i)+(em-e)/dexs*(fnpsg(it,i+1)-fnpsg(it,i))                   !/*sg         20*/
      else                                                                      !/*sg         21*/
        i=min(i,125)                                                            !/*sg         22*/
        s=pipsg(it-2,i)+(em-e)/dexs*(pipsg(it-2,i+1)-pipsg(it-2,i))             !/*sg         23*/
      endif                                                                     !/*sg         24*/
      return                                                                    !/*sg         25*/
!** low energy (.lt. 20 mev) p-p of n-p xsects for elast. scatt. with h.        !/*sg         26*/
   10 continue                                                                  !/*sg         27*/
      do 20 ie=2,iemx                                                           !/*sg         28*/
      if (em.le.enrgy(ie)) go to 30                                             !/*sg         29*/
   20 continue                                                                  !/*sg         30*/
      ie=iemx                                                                   !/*sg         31*/
   30 continue                                                                  !/*sg         32*/
      s=log(ppnp(ie-1,it))+(em-enrgy(ie-1))/(enrgy(ie)-enrgy(ie-1))*(log        !/*sg         33*/
     & (ppnp(ie,it))-log(ppnp(ie-1,it)))                                        !/*sg         34*/
      s=exp(s)*1.0d-24                                                          !/*sg         35*/
!                                                                               !/*sg         36*/
      return                                                                    !/*sg         37*/
      end                                                                       !/*sg         38*/

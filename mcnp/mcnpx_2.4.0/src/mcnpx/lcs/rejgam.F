!     $Id: rejgam.F,v 1.4 2002/03/27 18:15:20 gwm Exp $                       !/*re          1*/
      subroutine rejgam (e)                                                     !/*re          2*/
#include <lcs/double.h>                                                         /*!re          3*/
#include <lcs/phtcom.h>                                                         /*!re          4*/
! ..  save nacc, nrej                                                           !/*re          5*/
   10 continue                                                                  !/*re          6*/
      call conprb (e,e11,iia)                                                   !/*re          7*/
      prob=psjsum*finv                                                          !/*re          8*/
      ptemp=prob                                                                !/*re          9*/
      pp=rang()                                                                 !/*re         10*/
      if (pp.ge.ptemp) go to 40                                                 !/*re         11*/
      pp=psjsum*rang()                                                          !/*re         12*/
      do 20 i=1,nstat                                                           !/*re         13*/
      if (pp.le.psj(i)) go to 30                                                !/*re         14*/
   20 continue                                                                  !/*re         15*/
      i=nstat                                                                   !/*re         16*/
   30 continue                                                                  !/*re         17*/
      isp0=mod(iia,2)-1+2*i                                                     !/*re         18*/
      temp=10.0*(c2/cra(iia))**2                                                !/*re         19*/
      if (rang()*(dp1+temp).gt.temp) ipar0=-ipar0                               !/*re         20*/
      iccc=1                                                                    !/*re         21*/
      nacc=nacc+1                                                               !/*re         22*/
      go to 60                                                                  !/*re         23*/
   40 iccc=0                                                                    !/*re         24*/
      nrej=nrej+1                                                               !/*re         25*/
      if (irep.eq.0) go to 60                                                   !/*re         26*/
      e11=skeme(e,eminn)                                                        !/*re         27*/
      if (e11.ge.eminn) go to 50                                                !/*re         28*/
      call expirx(1,'REJGAM','e11.lt.eminn')                                    !/*re         29*/
   50 continue                                                                  !/*re         30*/
      go to 10                                                                  !/*re         31*/
   60 return                                                                    !/*re         32*/
!                                                                               !/*re         33*/
      end                                                                       !/*re         34*/

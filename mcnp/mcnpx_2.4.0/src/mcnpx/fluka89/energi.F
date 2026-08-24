!     $Id: energi.F,v 1.4 2002/03/27 18:14:42 gwm Exp $                       !/*en          1*/
      subroutine energi (it,ll,lt,iv,re,hma,he,e0,a1)                           !/*en          2*/
!                                                                     *         !/*en          3*/
!---------------------------------------------------------------------*         !/*en          4*/
!                                                                     *         !/*en          5*/
#include <lcs/double.h>                                                         /*!en          6*/
#include <fluka89/flprm1.h>                                                     /*!en          7*/
#include <fluka89/flprm2.h>                                                     /*!en          8*/
#include <lcs/inout.h>                                                          /*!en          9*/
#include <fluka89/finpr1.h>                                                     /*!en         10*/
      dimension iv(*), re(*)                                                    !/*en         11*/
      i=it                                                                      !/*en         12*/
      j=it-1                                                                    !/*en         13*/
      hma=amf(i)                                                                !/*en         14*/
      if (it.eq.1) rx=e0                                                        !/*en         15*/
      if (it.gt.1) rx=re(j)                                                     !/*en         16*/
      x=rang()                                                                  !/*en         17*/
      ivy=iv(i)                                                                 !/*en         18*/
      go to (10,20,20,10,10,10,20,20,10,10), ivy                                !/*en         19*/
   10 ax=(1.d0-a1)/(3.d0*a1)                                                    !/*en         20*/
      c=1.d0/((1.d0-a1)*(1.d0-hma/rx)+3.d0*.5d0*a1*(1.d0-hma/rx)**2)            !/*en         21*/
      eta=-ax+sqrt(ax**2+x*2.d0/(3.d0*a1*c))                                    !/*en         22*/
      go to 30                                                                  !/*en         23*/
   20 cy=1.d0/((1.d0-a1)*(1.d0-hma/rx)+a1*(1.d0-hma/rx)**3)                     !/*en         24*/
      by=x/(2.d0*a1*cy)                                                         !/*en         25*/
      bz=((1.d0-a1)/(3.d0*a1))**3                                               !/*en         26*/
      bv=by+sqrt(by**2+bz)                                                      !/*en         27*/
      bu=by-sqrt(by**2+bz)                                                      !/*en         28*/
      if (bu.lt.0.d0) bu1=abs(bu)                                               !/*en         29*/
      bu3=bu1**(1.d0/3.d0)                                                      !/*en         30*/
      bv3=bv**(1.d0/3.d0)                                                       !/*en         31*/
      if (bu.lt.0.d0) bu3=-bu3                                                  !/*en         32*/
      eta=bu3+bv3                                                               !/*en         33*/
      go to 30                                                                  !/*en         34*/
   30 continue                                                                  !/*en         35*/
      re(i)=eta*rx                                                              !/*en         36*/
      he=(1.d0-eta)*rx                                                          !/*en         37*/
      hef(i)=he                                                                 !/*en         38*/
      return                                                                    !/*en         39*/
      end                                                                       !/*en         40*/

!     $Id: coulmb.F,v 1.4 2002/03/27 18:14:58 gwm Exp $                       !/*co          1*/
      subroutine coulmb (e,z,a,iflag)                                           !/*co          2*/
#include <lcs/double.h>                                                         /*!co          3*/
#include <lcs/lptS.h>                                                           /*!co          4*/
#include <lcs/rucom.h>                                                          /*!co          5*/
      dimension rpart(lpt), rcoul(260)                                          !/*co          6*/
! ..  save rpart, rcoul                                                         !/*co          7*/
      data c2 /.00516d0/, c3 /.007165d0/                                        !/*co          8*/
      iflag=0                                                                   !/*co          9*/
      if (charge(ityp).gt.dp0) then                                             !/*co         10*/
        rc=rcoul(int(a))+rpart(ityp)                                            !/*co         11*/
        ec=1.44*z*charge(ityp)/rc                                               !/*co         12*/
        amp=pmass(ityp)                                                         !/*co         13*/
        amt=938.d0*a                                                            !/*co         14*/
        eps=amt*e/(amt+amp)                                                     !/*co         15*/
        temp=sqrt(amp/e)                                                        !/*co         16*/
        eta=c2*z*charge(ityp)*temp                                              !/*co         17*/
        ro=c3*rc*eps*temp                                                       !/*co         18*/
        prob=clmb1(ro,eta,ml)                                                   !/*co         19*/
        if (rang().gt.prob) iflag=1                                             !/*co         20*/
      endif                                                                     !/*co         21*/
      return                                                                    !/*co         22*/
      entry setcoul                                                             !/*co         23*/
      do 10 i=1,260                                                             !/*co         24*/
      am=i                                                                      !/*co         25*/
      rcoul(i)=1.2d0*am**dpth                                                   !/*co         26*/
   10 continue                                                                  !/*co         27*/
      do 20 i=1,lpt                                                             !/*co         28*/
   20 rpart(i)=dp0                                                              !/*co         29*/
      rpart(8)=rcoul(2)                                                         !/*co         30*/
      rpart(9)=rcoul(3)                                                         !/*co         31*/
      rpart(10)=rcoul(3)                                                        !/*co         32*/
      rpart(11)=rcoul(4)                                                        !/*co         33*/
      return                                                                    !/*co         34*/
      end                                                                       !/*co         35*/

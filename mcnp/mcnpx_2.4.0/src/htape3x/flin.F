!     $Id: flin.F,v 1.4 2002/03/27 18:13:59 gwm Exp $                         !/*fl          1*/
      function flin (a1,z1,er0)                                                 !/*fl          2*/
#include <htape3x/com32b.h>                                                     /*!fl          3*/
#include <htape3x/label.h>                                                      /*!fl          4*/
#include <htape3x/contrl.h>                                                     /*!fl          5*/
      common /label2/ geosig(260), crsc(600), den(lmel), tden(lmt)              !/*fl          6*/
      common /save3/ t1, t2, fk, eps, g, temp, er                               !/*fl          7*/
      temp=dp0                                                                  !/*fl          8*/
      er=1.0d+06*er0                                                            !/*fl          9*/
      t1=exp(dp2*log(z1)/dp3)                                                   !/*fl         10*/
      nelm=nel(nmed)                                                            !/*fl         11*/
      do 10 l=1,nelm                                                            !/*fl         12*/
      a2=a(mel(nmed)+l-1)                                                       !/*fl         13*/
      z2=zz(mel(nmed)+l-1)                                                      !/*fl         14*/
      t2=exp(dp2*log(z2)/dp3)                                                   !/*fl         15*/
      q=(dph*(t1+t2))**0.75d0                                                   !/*fl         16*/
      fk=0.133745d0*t1*sqrt(z2/a1)*(a1+a2)**2/(dp4*a1*a2*q)                     !/*fl         17*/
      eps=0.03252d0*a2*er/((a1+a2)*z1*z2*sqrt(t1+t1))                           !/*fl         18*/
      g=eps+0.40244d0*eps**0.75d0+3.4008d0*eps**0.1666666667d0                  !/*fl         19*/
      temp=temp+den(mel(nmed)+l-1)/(dp1+fk*g)                                   !/*fl         20*/
   10 continue                                                                  !/*fl         21*/
      flin=temp/tden(nmed)                                                      !/*fl         22*/
      return                                                                    !/*fl         23*/
      end                                                                       !/*fl         24*/

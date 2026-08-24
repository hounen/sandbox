!     $Id: xlamb.F,v 1.4 2002/03/27 18:14:48 gwm Exp $                        !/*xl          1*/
      double precision function xlamb (x,y,z)                                   !/*xl          2*/
!                                                                     *         !/*xl          3*/
!---------------------------------------------------------------------*         !/*xl          4*/
!                                                                     *         !/*xl          5*/
#include <lcs/double.h>                                                         /*!xl          6*/
#include <fluka89/flprm1.h>                                                     /*!xl          7*/
#include <fluka89/flprm2.h>                                                     /*!xl          8*/
#include <lcs/inout.h>                                                          /*!xl          9*/
!*ca idgb                                                                       !/*xl         10*/
#include <fluka89/gamrd1.h>                                                     /*!xl         11*/
#include <fluka89/drei1.h>                                                      /*!xl         12*/
      yz=y-z                                                                    !/*xl         13*/
      xlamb=x*x-2.d0*x*(y+z)+yz*yz                                              !/*xl         14*/
      xlam=xlamb                                                                !/*xl         15*/
!L    if (idgb.le.0) go to 10                                                   !/*xl         16*/
!L    if (xlamb.gt.1.d-12) go to 10                                             !/*xl         17*/
!L    write (itty,20)                                                           !/*xl         18*/
!L    write (itty,40) xlam,x,y,z,test                                           !/*xl         19*/
!L    write (itty,30)                                                           !/*xl         20*/
!L 10 continue                                                                  !/*xl         21*/
      if (xlamb.le.0.d0) xlamb=abs(xlamb)                                       !/*xl         22*/
      xlamb=sqrt(xlamb)                                                         !/*xl         23*/
      return                                                                    !/*xl         24*/
!                                                                               !/*xl         25*/
!L 20 format (/,10x,12h xlamb print)                                            !/*xl         26*/
!L 30 format (10x,60(1h*))                                                      !/*xl         27*/
!L 40 format (4e20.8,5hxlamb,/,12f10.5)                                         !/*xl         28*/
      end                                                                       !/*xl         29*/

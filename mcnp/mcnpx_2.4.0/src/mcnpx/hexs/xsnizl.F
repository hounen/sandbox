!     $Id: xsnizl.F,v 1.11 2002/03/27 18:14:51 gwm Exp $                      !/*xs          1*/
      subroutine xsnizl (kproj,atarg,ztarg,ekmev,sigine,sigela)                 !/*xs          2*/
!.....                                                                          !/*xs          3*/
!.....                                                                          !/*xs          4*/
#include <lcs/double.h>                                                         /*!xs          5*/
      parameter (idmaxp=220)                                                    !/*xs          6*/
      parameter (f=0.0199d0)                                                    !/*xs          7*/
#include <fluka89/part1.h>                                                      /*!xs          8*/
!  +-------------------------------------------------------------------*        !/*xs          9*/
!  |                                                                            !/*xs         10*/
      ekin=0.001d0*ekmev                                                        !/*xs         11*/
#ifndef OLDM                                                                    /*!xs         12*/
      ekin=max(ekin,f)                                                          !/*xs         13*/
#endif                                                                          /*!xs         14*/
      pproj=sqrt(ekin*(ekin+dp2*am(kproj)))                                     !/*xs         15*/
      sigine=dp0                                                                !/*xs         16*/
      sigela=dp0                                                                !/*xs         17*/
!  |                                                                            !/*xs         18*/
!  +-------------------------------------------------------------------*        !/*xs         19*/
!  get the reaction cross section only:                                         !/*xs         20*/
      call nizl (kproj,atarg,pproj,sigine,zldum)                                !/*xs         21*/
      return                                                                    !/*xs         22*/
      end                                                                       !/*xs         23*/

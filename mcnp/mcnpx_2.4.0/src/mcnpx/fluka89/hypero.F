!     $Id: hypero.F,v 1.4 2002/03/27 18:14:45 gwm Exp $                       !/*hy          1*/
      subroutine hypero (it,itnucr,sico,plabco)                                 !/*hy          2*/
!                                                                     *         !/*hy          3*/
!---------------------------------------------------------------------*         !/*hy          4*/
!                                                                     *         !/*hy          5*/
#include <lcs/double.h>                                                         /*!hy          6*/
#include <fluka89/flprm1.h>                                                     /*!hy          7*/
#include <fluka89/flprm2.h>                                                     /*!hy          8*/
#include <lcs/inout.h>                                                          /*!hy          9*/
!     integer*2 ihatrs, inutrs                                                  !/*hy         10*/
#include <fluka89/ablts4.h>                                                     /*!hy         11*/
      dimension ihatrs(30), inutrs(30)                                          !/*hy         12*/
      data ihatrs /16*30,25,24,30,16,15,25,8*30/                                !/*hy         13*/
      data inutrs /16*30,8,2,30,8,1,8,8*30/                                     !/*hy         14*/
      itnucr=inutrs(it)                                                         !/*hy         15*/
      ithacr=ihatrs(it)                                                         !/*hy         16*/
      plabco=1.d0                                                               !/*hy         17*/
      ithatr=ithacr                                                             !/*hy         18*/
      if (itnucr.ge.30) ithatr=it                                               !/*hy         19*/
      if (itnucr.ge.30) itnucr=it                                               !/*hy         20*/
      sico=1.d0                                                                 !/*hy         21*/
      if (ithatr.lt.30) sico=42.d0/39.d0                                        !/*hy         22*/
      if (it.eq.21) sico=42.d0/40.d0                                            !/*hy         23*/
      if (ithatr.lt.30) plabco=am(it)/(am(itnucr)+1.d-10)                       !/*hy         24*/
      it=ithatr                                                                 !/*hy         25*/
      return                                                                    !/*hy         26*/
      end                                                                       !/*hy         27*/

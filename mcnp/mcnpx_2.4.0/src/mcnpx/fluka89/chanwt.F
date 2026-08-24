!     $Id: chanwt.F,v 1.4 2002/03/27 18:14:41 gwm Exp $                       !/*ch          1*/
      subroutine chanwt                                                         !/*ch          2*/
!                                                                     *         !/*ch          3*/
!---------------------------------------------------------------------*         !/*ch          4*/
!                                                                     *         !/*ch          5*/
#include <lcs/double.h>                                                         /*!ch          6*/
#include <fluka89/flprm1.h>                                                     /*!ch          7*/
#include <fluka89/flprm2.h>                                                     /*!ch          8*/
#include <lcs/inout.h>                                                          /*!ch          9*/
#include <fluka89/decyc1.h>                                                     /*!ch         10*/
#include <fluka89/part1.h>                                                      /*!ch         11*/
      dimension hwt(idmax9)                                                     !/*ch         12*/
      do 10 j=1,idmax9                                                          !/*ch         13*/
   10 hwt(j)=0.d0                                                               !/*ch         14*/
      do 30 i=1,idmax8                                                          !/*ch         15*/
      ik1=k1(i)                                                                 !/*ch         16*/
      ik2=k2(i)                                                                 !/*ch         17*/
      hv=0.d0                                                                   !/*ch         18*/
      do 20 j=ik1,ik2                                                           !/*ch         19*/
      hv=hv+wt(j)                                                               !/*ch         20*/
      hwt(j)=hv                                                                 !/*ch         21*/
      if (hwt(j)-1.d0.gt.1.d-14) write (itty,50) hwt(j),j,i,ik1                 !/*ch         22*/
   20 continue                                                                  !/*ch         23*/
   30 continue                                                                  !/*ch         24*/
      do 40 j=1,idmax9                                                          !/*ch         25*/
   40 wt(j)=hwt(j)                                                              !/*ch         26*/
      return                                                                    !/*ch         27*/
!                                                                               !/*ch         28*/
   50 format (2x,15h error in hwt =,1f10.5,8h j,i,k1=,3i5)                      !/*ch         29*/
      end                                                                       !/*ch         30*/
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$        !/*ch         31*/

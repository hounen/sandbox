!     $Id: writem.F,v 1.4 2002/03/27 18:15:26 gwm Exp $                       !/*wr          1*/
      subroutine writem (istept,istpt,ws,mode)                                  !/*wr          2*/
! WRITEM calls   IGETD  ERRORF                                                  !/*wr          3*/
! called by SELPT                                                               !/*wr          4*/
#include <lcs/double.h>                                                         /*!wr          5*/
#include <lcs/inout.h>                                                          /*!wr          6*/
      dimension ws(14)                                                          !/*wr          7*/
      dimension icount(2)                                                       !/*wr          8*/
! ..  save icount                                                               !/*wr          9*/
      data icount /2*0/                                                         !/*wr         10*/
      nocas=igetd(1)                                                            !/*wr         11*/
      if (mode.eq.1) then                                                       !/*wr         12*/
        icount(1)=icount(1)+1                                                   !/*wr         13*/
        if (icount(1).gt.20) return                                             !/*wr         14*/
        write (io,10) nocas,istept,istpt,ws(14),(ws(i),i=1,13)                  !/*wr         15*/
      elseif (mode.eq.2) then                                                   !/*wr         16*/
        icount(2)=icount(2)+1                                                   !/*wr         17*/
        if (icount(2).gt.20) return                                             !/*wr         18*/
        write (io,20) nocas,istept,istpt,ws(14),(ws(i),i=1,13)                  !/*wr         19*/
      else                                                                      !/*wr         20*/
        call expirx(1,'WRITEM','mode.ne.1 and mode.ne.2')                       !/*wr         21*/
      endif                                                                     !/*wr         22*/
      return                                                                    !/*wr         23*/
!                                                                               !/*wr         24*/
   10 format (' big jump in selpt NOCAS=',i7,' istept,istpt,ws(14)=',2i5        !/*wr         25*/
     & ,1pe15.5/(' ws:',1p7e13.5))                                              !/*wr         26*/
   20 format (' big jump in selpt NOCAS=',i7,' istepp,istpp,ws(14)=',2i5        !/*wr         27*/
     & ,1pe15.5/(' ws:',1p7e13.5))                                              !/*wr         28*/
      end                                                                       !/*wr         29*/

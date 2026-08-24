!     $Id: timeit.F,v 1.5 2002/03/27 18:16:26 gwm Exp $                       !/*ti          1*/
      subroutine timeit                                                         !/*ti          2*/
!                                                                               !/*ti          3*/
! prints elapsed time since beginning of run                                    !/*ti          4*/
! cpa is initial time point                                                     !/*ti          5*/
!                                                                               !/*ti          6*/
#include <htape3x/com32b.h>                                                     /*!ti          7*/
#include <htape3x/lpt.h>                                                        /*!ti          8*/
#include <htape3x/contrl.h>                                                     /*!ti          9*/
      call secnd (cp2)                                                          !/*ti         10*/
      cp0=cp2-cpa                                                               !/*ti         11*/
      ih=cp0/3600.                                                              !/*ti         12*/
      cp0=cp0-ih*3600.                                                          !/*ti         13*/
      im=cp0/60.                                                                !/*ti         14*/
      ts=cp0-im*60.                                                             !/*ti         15*/
      if (ih.lt.1) go to 10                                                     !/*ti         16*/
      if (.not.ltesting) write (io,30) ih,im,ts                                 !/*ti         17*/
      write (jtty,30) ih,im,ts                                                  !/*ti         18*/
      return                                                                    !/*ti         19*/
   10 if (im.lt.1) go to 20                                                     !/*ti         20*/
      if (.not.ltesting) write (io,40) im,ts                                    !/*ti         21*/
      write (jtty,40) im,ts                                                     !/*ti         22*/
      return                                                                    !/*ti         23*/
   20 if (.not.ltesting) write (io,50) ts                                       !/*ti         24*/
      write (jtty,50) ts                                                        !/*ti         25*/
      return                                                                    !/*ti         26*/
!                                                                               !/*ti         27*/
   30 format (/5x,'execution time=',i3,' hours',i3,' minutes',                  !/*ti         28*/
     & f6.2,' seconds')                                                         !/*ti         29*/
   40 format (/5x,'execution time=',i3,' minutes',f6.2,' seconds')              !/*ti         30*/
   50 format (/5x,'execution time=',f6.2,' seconds')                            !/*ti         31*/
      end                                                                       !/*ti         32*/

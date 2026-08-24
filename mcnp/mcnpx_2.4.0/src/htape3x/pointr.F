!     $Id: pointr.F,v 1.3 2002/03/27 18:14:02 gwm Exp $                       !/*po          1*/
      subroutine pointr (arr,length,idum)                                       !/*po          2*/
#include <htape3x/com32b.h>                                                     /*!po          3*/
#include <htape3x/bulk.h>                                                       /*!po          4*/
      character *6 blank                                                        !/*po          5*/
#include <htape3x/alloc.h>                                                      /*!po          6*/
      data blank /'      '/                                                     !/*po          7*/
      ierr=0                                                                    !/*po          8*/
      istat=idum                                                                !/*po          9*/
      if (length.gt.ndas) then                                                  !/*po         10*/
        ierr=1                                                                  !/*po         11*/
        return                                                                  !/*po         12*/
      endif                                                                     !/*po         13*/
      maxblk=length                                                             !/*po         14*/
      ict=0                                                                     !/*po         15*/
      do 10 i=1,maxblk                                                          !/*po         16*/
   10 blk(i)=dp0                                                                !/*po         17*/
      nup=num                                                                   !/*po         18*/
      do 20 i=1,nup                                                             !/*po         19*/
      names(i)=blank                                                            !/*po         20*/
      len(i)=0                                                                  !/*po         21*/
      iptt(i)=0                                                                 !/*po         22*/
   20 continue                                                                  !/*po         23*/
      iptt(1)=1                                                                 !/*po         24*/
      return                                                                    !/*po         25*/
      end                                                                       !/*po         26*/

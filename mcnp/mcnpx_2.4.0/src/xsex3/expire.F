!     $Id: expire.F,v 1.5 2002/03/27 18:16:24 gwm Exp $                       !/*ex          1*/
      subroutine expire (mm,hs,hm)                                              !/*ex          2*/
!        print a fatal error message and quit.                                  !/*ex          3*/
!        mm:  1 = print history identification if in mcrun;  0 = do not.        !/*ex          4*/
!        hs = the name of the calling subroutine.                               !/*ex          5*/
!        hm = error message.                                                    !/*ex          6*/
!                                                                               !/*ex          7*/
!        tty messages on some systems are limited to 70 characters.             !/*ex          8*/
!        so expire error messages should have only 69 characters.               !/*ex          9*/
!                                                                               !/*ex         10*/
#include <htape3x/com32b.h>                                                     /*!ex         11*/
#include <htape3x/lpt.h>                                                        /*!ex         12*/
#include <htape3x/contrl.h>                                                     /*!ex         13*/
      character hs*(*), hm*(*), hd(2)*70                                        !/*ex         14*/
      data hd /' bad trouble in subroutine         ',' '/                       !/*ex         15*/
!                                                                               !/*ex         16*/
!        set up the names of the subroutine.                                    !/*ex         17*/
      hd(1)(28:34)=hs                                                           !/*ex         18*/
!                                                                               !/*ex         19*/
!        set up the specific error message, hm.                                 !/*ex         20*/
      hd(2)(2:70)=hm                                                            !/*ex         21*/
!                                                                               !/*ex         22*/
!        print the error story.                                                 !/*ex         23*/
      write (jtty,10) (hd(j),j=1,2)                                             !/*ex         24*/
      write (io,10) (hd(j),j=1,2)                                               !/*ex         25*/
!                                                                               !/*ex         26*/
!        quit.                                                                  !/*ex         27*/
      stop                                                                      !/*ex         28*/
!                                                                               !/*ex         29*/
   10 format (/a70/a70)                                                         !/*ex         30*/
      end                                                                       !/*ex         31*/

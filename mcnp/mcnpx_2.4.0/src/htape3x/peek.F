!     $Id: peek.F,v 1.4 2002/03/27 18:14:02 gwm Exp $                         !/*pe          1*/
      subroutine peek (maxa,maxz)                                               !/*pe          2*/
#include <htape3x/com32b.h>                                                     /*!pe          3*/
#include <htape3x/label.h>                                                      /*!pe          4*/
#include <htape3x/contrl.h>                                                     /*!pe          5*/
#include <htape3x/srcwt.h>                                                      /*!pe          6*/
#include <htape3x/com10.h>                                                      /*!pe          7*/
      maxa=0                                                                    !/*pe          8*/
      maxz=0                                                                    !/*pe          9*/
      rewind nhst                                                               !/*pe         10*/
      nhst=nh10                                                                 !/*pe         11*/
      log10=.true.                                                              !/*pe         12*/
      ic0=icase                                                                 !/*pe         13*/
      icase=2                                                                   !/*pe         14*/
      call rednmt                                                               !/*pe         15*/
      icase=ic0                                                                 !/*pe         16*/
      if (ncol.ne.-1) call errorf ('peek.1  ')                                  !/*pe         17*/
      rewind nhst                                                               !/*pe         18*/
      do 20 m=1,mxmat                                                           !/*pe         19*/
      nelm=nel(m)                                                               !/*pe         20*/
      do 10 l=1,nelm                                                            !/*pe         21*/
      iz=zz(mel(m)+l-1)+.01                                                     !/*pe         22*/
      ia=a(mel(m)+l-1)+.01                                                      !/*pe         23*/
      maxz=max(maxz,iz)                                                         !/*pe         24*/
      maxa=max(maxa,ia)                                                         !/*pe         25*/
   10 continue                                                                  !/*pe         26*/
   20 continue                                                                  !/*pe         27*/
      write (io,30) maxa,maxz                                                   !/*pe         28*/
   30 format('0 In PEEK, maxa =',i4,' maxz =',i4)                               !/*pe         29*/
      return                                                                    !/*pe         30*/
      end                                                                       !/*pe         31*/

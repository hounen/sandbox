!     $Id: filden.F,v 1.5 2002/03/27 18:16:24 gwm Exp $                       !/*fi          1*/
      subroutine filden                                                         !/*fi          2*/
#include <htape3x/com32b.h>                                                     /*!fi          3*/
#include <htape3x/label.h>                                                      /*!fi          4*/
#include <htape3x/contrl.h>                                                     /*!fi          5*/
      common /label2/ geosig(260), crsc(600), den(lmel), tden(lmt)              !/*fi          6*/
      do 20 m=1,mxmat                                                           !/*fi          7*/
      nelm=nel(m)                                                               !/*fi          8*/
      tden(m)=dp0                                                               !/*fi          9*/
      do 10 l=1,nelm                                                            !/*fi         10*/
      na=a(mel(m)+l-1)+dph                                                      !/*fi         11*/
      den(mel(m)+l-1)=sigg(1,mel(m)+l-1)/geosig(na)                             !/*fi         12*/
      tden(m)=tden(m)+den(mel(m)+l-1)                                           !/*fi         13*/
   10 continue                                                                  !/*fi         14*/
   20 continue                                                                  !/*fi         15*/
        do 30 m=1,mxmat                                                         !/*fi         16*/
        nelm=nel(m)                                                             !/*fi         17*/
        write (io,40) m                                                         !/*fi         18*/
        write (io,50) (l,zz(mel(m)+l-1),a(mel(m)+l-1),den(mel(m)+l-1),          !/*fi         19*/
     & l=1,nelm)                                                                !/*fi         20*/
   30   continue                                                                !/*fi         21*/
      return                                                                    !/*fi         22*/
!                                                                               !/*fi         23*/
   40 format (1h0,10x,'medium ',i4,13x,                                         !/*fi         24*/
     & 'nuclide no.    z no.         a no.         atom density ')              !/*fi         25*/
   50 format (35x,i8,1p3d15.4)                                                  !/*fi         26*/
      end                                                                       !/*fi         27*/

!     $Id: respin.F,v 1.5 2002/03/27 18:14:03 gwm Exp $                       !/*re          1*/
      subroutine respin                                                         !/*re          2*/
!------ response function input                                                 !/*re          3*/
#include <htape3x/com32b.h>                                                     /*!re          4*/
#include <htape3x/label.h>                                                      /*!re          5*/
#include <htape3x/contrl.h>                                                     /*!re          6*/
#include <htape3x/cresp.h>                                                      /*!re          7*/
      character *6 isub                                                         !/*re          8*/
      data isub /'respin'/                                                      !/*re          9*/
      do 10 i=1,mresp                                                           !/*re         10*/
   10 eresp(i)=-0.79342569                                                      !/*re         11*/
      read (in,*) eresp                                                         !/*re         12*/
      read (in,*) fresp                                                         !/*re         13*/
      read (in,*) iresp                                                         !/*re         14*/
      do 20 nresp=1,mresp                                                       !/*re         15*/
      if (eresp(nresp).eq.-0.79342569) go to 30                                 !/*re         16*/
   20 continue                                                                  !/*re         17*/
      nresp=mresp+1                                                             !/*re         18*/
   30 nresp=nresp-1                                                             !/*re         19*/
      write (io,50) (eresp(i),i=1,nresp)                                        !/*re         20*/
      do 40 i=2,nresp                                                           !/*re         21*/
      if (eresp(i).le.eresp(i-1)) call expire (0,isub,                          !/*re         22*/
     & 'energies not monotonically inc.')                                       !/*re         23*/
   40 continue                                                                  !/*re         24*/
      write (io,60) (fresp(i),i=1,nresp)                                        !/*re         25*/
      write (io,70) (iresp(i),i=1,nresp-1)                                      !/*re         26*/
      return                                                                    !/*re         27*/
!                                                                               !/*re         28*/
   50 format ('0 response energy bin boundaries:'/10(1pd13.5))                  !/*re         29*/
   60 format ('0 response function values:'/10(1pd13.5))                        !/*re         30*/
   70 format ('0 response function interpolation laws:'/10(i13))                !/*re         31*/
      end                                                                       !/*re         32*/

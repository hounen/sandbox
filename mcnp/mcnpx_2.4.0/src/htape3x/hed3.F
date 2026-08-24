!     $Id: hed3.F,v 1.4 2002/03/27 18:13:59 gwm Exp $                         !/*he          1*/
      subroutine hed3 (nlo,nhi,k,fact,tlly,tlsq,nerg1,ndim2)                    !/*he          2*/
#include <htape3x/com32b.h>                                                     /*!he          3*/
#include <htape3x/lpt.h>                                                        /*!he          4*/
#include <htape3x/contrl.h>                                                     /*!he          5*/
#include <htape3x/bndy.h>                                                       /*!he          6*/
#include <htape3x/kpt.h>                                                        /*!he          7*/
      dimension tlly(nerg1,ndim2,1), tlsq(nerg1,ndim2,1)                        !/*he          8*/
      write (io,50) (kpart(l),l=nlo,nhi)                                        !/*he          9*/
      nup=nerg1-2                                                               !/*he         10*/
      iflag=0                                                                   !/*he         11*/
      do 30 j=1,nup                                                             !/*he         12*/
      fact=dnerg(j)/dnparm(k)                                                   !/*he         13*/
      if (j.lt.nup) write (io,60) ergb(j),(fact*tlly(j,k,m),tlsq(j,k,m)         !/*he         14*/
     & ,m=nlo,nhi)                                                              !/*he         15*/
      if (j.ge.nup) write (io,70) (fact*tlly(j,k,m),tlsq(j,k,m),m=nlo           !/*he         16*/
     & ,nhi)                                                                    !/*he         17*/
   30 continue                                                                  !/*he         18*/
      if (nlo.gt.lpt) go to 40                                                  !/*he         19*/
      write (io,80) (fact*tlly(nerg1-1,k,m),tlsq(nerg1-1,k,m),m=nlo,nhi)        !/*he         20*/
      write (io,90) (fact*tlly(nerg1,k,m),tlsq(nerg1,k,m),m=nlo,nhi)            !/*he         21*/
   40 continue                                                                  !/*he         22*/
      return                                                                    !/*he         23*/
!                                                                               !/*he         24*/
   50 format (/5x,'energy',6(7x,a9,2x))                                         !/*he         25*/
   60 format (1x,1pd10.3,6(1pd12.3,1x,0pf5.3))                                  !/*he         26*/
   70 format (4x,'total  ',6(1pd12.3,1x,0pf5.3))                                !/*he         27*/
   80 format (4x,'removal',6(1pd12.3,1x,0pf5.3))                                !/*he         28*/
   90 format (4x,'net    ',6(1pd12.3,1x,0pf5.3))                                !/*he         29*/
      end                                                                       !/*he         30*/

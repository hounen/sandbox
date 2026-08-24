!     $Id: wtin4.F,v 1.4 2002/03/27 18:14:04 gwm Exp $                        !/*wt          1*/
      subroutine wtin4                                                          !/*wt          2*/
!------ read and process isopt=3 type source.                                   !/*wt          3*/
#include <htape3x/com32b.h>                                                     /*!wt          4*/
#include <htape3x/label.h>                                                      /*!wt          5*/
#include <htape3x/contrl.h>                                                     /*!wt          6*/
#include <htape3x/wtpar4.h>                                                     /*!wt          7*/
      do 10 i=1,mring                                                           !/*wt          8*/
      sring2(i)=0                                                               !/*wt          9*/
   10 sprob(i)=0                                                                !/*wt         10*/
      read (in,*) sring2                                                        !/*wt         11*/
      read (in,*) sprob                                                         !/*wt         12*/
      pcum=0                                                                    !/*wt         13*/
      do 20 i=1,mring                                                           !/*wt         14*/
      if (sring2(i).le.dp0) go to 30                                            !/*wt         15*/
      sring2(i)=sring2(i)**2                                                    !/*wt         16*/
   20 pcum=pcum+sprob(i)                                                        !/*wt         17*/
      i=mring+1                                                                 !/*wt         18*/
   30 nring=i-1                                                                 !/*wt         19*/
      if (pcum.gt.dp0) pcum=1.0/pcum                                            !/*wt         20*/
      sprob(1)=sprob(1)*pcum/sring2(1)                                          !/*wt         21*/
      if (nring.lt.2) go to 50                                                  !/*wt         22*/
      do 40 i=2,nring                                                           !/*wt         23*/
   40 sprob(i)=sprob(i)*pcum/(sring2(i)-sring2(i-1))                            !/*wt         24*/
   50 write (io,60) (sring2(i),i=1,nring)                                       !/*wt         25*/
      write (io,70) (sprob(i),i=1,nring)                                        !/*wt         26*/
      return                                                                    !/*wt         27*/
!                                                                               !/*wt         28*/
   60 format ('0 rings squared:'/10(1pd13.5))                                   !/*wt         29*/
   70 format ('0 probabilities:'/10(1pd13.5))                                   !/*wt         30*/
      end                                                                       !/*wt         31*/

!     $Id: tsrcin.F,v 1.5 2002/03/27 18:14:04 gwm Exp $                       !/*ts          1*/
      subroutine tsrcin                                                         !/*ts          2*/
#include <htape3x/com32b.h>                                                     /*!ts          3*/
!------ user defined time probs.                                                !/*ts          4*/
#include <htape3x/label.h>                                                      /*!ts          5*/
#include <htape3x/contrl.h>                                                     /*!ts          6*/
#include <htape3x/ctcon.h>                                                      /*!ts          7*/
      character *6 isub                                                         !/*ts          8*/
      data isub /'tsrcin'/                                                      !/*ts          9*/
      do 10 i=1,mtprb                                                           !/*ts         10*/
   10 tprb(i)=-0.79342569                                                       !/*ts         11*/
      read (in,*) tsrc                                                          !/*ts         12*/
      read (in,*) tprb                                                          !/*ts         13*/
      do 20 ntprb=1,mtprb                                                       !/*ts         14*/
      if (tprb(ntprb).eq.-0.79342569) go to 30                                  !/*ts         15*/
   20 continue                                                                  !/*ts         16*/
      ntprb=mtprb+1                                                             !/*ts         17*/
   30 ntprb=ntprb-1                                                             !/*ts         18*/
      write (io,80) (tsrc(i),i=1,ntprb)                                         !/*ts         19*/
      do 40 i=2,ntprb                                                           !/*ts         20*/
      if (tsrc(i).le.tsrc(i-1)) call expire (0,isub,                            !/*ts         21*/
     & 'times must be monotonically inc.')                                      !/*ts         22*/
   40 continue                                                                  !/*ts         23*/
      tprb(1)=dp0                                                               !/*ts         24*/
      if (tprb(ntprb).eq.1.0.and.ntprb.gt.2) then                               !/*ts         25*/
        do 50 i=ntprb,3,-1                                                      !/*ts         26*/
   50   tprb(i)=max(tprb(i)-tprb(i-1),dp0)                                      !/*ts         27*/
      endif                                                                     !/*ts         28*/
      sum=dp0                                                                   !/*ts         29*/
      do 60 i=2,ntprb                                                           !/*ts         30*/
   60 sum=sum+tprb(i)                                                           !/*ts         31*/
      write (io,90) (tprb(i),i=1,ntprb)                                         !/*ts         32*/
      if (sum.le.dp0) call expire (0,isub,'sum of probs. must be > 0')          !/*ts         33*/
      sum=1.0/sum                                                               !/*ts         34*/
      do 70 i=2,ntprb                                                           !/*ts         35*/
   70 tprb(i)=sum*tprb(i)/(tsrc(i)-tsrc(i-1))                                   !/*ts         36*/
      write (io,100) (tprb(i),i=1,ntprb)                                        !/*ts         37*/
      return                                                                    !/*ts         38*/
!                                                                               !/*ts         39*/
   80 format ('0 tconv time bin boundaries:'/10(1pd13.5))                       !/*ts         40*/
   90 format ('0 tconv time bin probabilities:'/10(1pd13.5))                    !/*ts         41*/
  100 format ('0 tconv time bin densities:'/10(1pd13.5))                        !/*ts         42*/
      end                                                                       !/*ts         43*/

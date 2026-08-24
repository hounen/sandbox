!     $Id: afill0.F,v 1.4 2002/03/27 18:14:52 gwm Exp $                       !/*af          1*/
      subroutine afill0 (nel,zzx,ax,nerr)                                       !/*af          2*/
#include <lcs/double.h>                                                         /*!af          3*/
#include <lcs/inout.h>                                                          /*!af          4*/
      dimension la(101)                                                         !/*af          5*/
      data la /1,3,5,7,8,10,12,14,17,18,21,22,25,26,29,30,34,36,39,42,48        !/*af          6*/
     & ,49,54,56,60,61,65,66,71,73,78,80,85,86,92,94,100,102,106,107,112        !/*af          7*/
     & ,113,120,120,127,128,134,136,144,146,156,158,166,167,176,177,184,        !/*af          8*/
     & 186,190,191,198,198,205,207,214,215,222,223,229,230,237,239,245,2        !/*af          9*/
     & 47,252,254,261,263,269,270,277,279,283,284,284,284,284,284,284,28        !/*af         10*/
     & 4,285,285,288,288,288,288,288,288,288,288,288/                           !/*af         11*/
      iz=zzx                                                                    !/*af         12*/
      if (ax.gt.dp0) go to 10                                                   !/*af         13*/
      num=la(iz+1)-la(iz)                                                       !/*af         14*/
      if (num.le.0) go to 20                                                    !/*af         15*/
      nel=nel+num                                                               !/*af         16*/
      go to 30                                                                  !/*af         17*/
   10 continue                                                                  !/*af         18*/
      nel=nel+1                                                                 !/*af         19*/
      go to 30                                                                  !/*af         20*/
   20 continue                                                                  !/*af         21*/
      write (io,40) zzx                                                         !/*af         22*/
      write (itty,40) zzx                                                       !/*af         23*/
      nerr=nerr+1                                                               !/*af         24*/
   30 continue                                                                  !/*af         25*/
      return                                                                    !/*af         26*/
!                                                                               !/*af         27*/
   40 format (' unable to expand z =',f4.0,' with a = 0.0')                     !/*af         28*/
      end                                                                       !/*af         29*/

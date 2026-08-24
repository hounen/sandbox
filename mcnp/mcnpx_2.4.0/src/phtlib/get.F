!     $Id: get.F,v 1.4 2002/03/27 18:16:22 gwm Exp $                          !/*ge          1*/
      subroutine get (iflag)                                                    !/*ge          2*/
#ifdef LCS                                                                      /*!ge          3*/
#include <htape3x/com32b.h>                                                     /*!ge          4*/
#include <htape3x/work.h>                                                       /*!ge          5*/
      iflag=0                                                                   !/*ge          6*/
      read (10,70,end=60) iza,el,p,s,t,nd                                       !/*ge          7*/
      if (iza/1000.eq.900) go to 60                                             !/*ge          8*/
      ptot=dp0                                                                  !/*ge          9*/
      do 10 i=1,19                                                              !/*ge         10*/
   10 pb(i)=dp0                                                                 !/*ge         11*/
      if (nd.eq.0) go to 50                                                     !/*ge         12*/
      psum=dp0                                                                  !/*ge         13*/
      do 20 n=1,nd                                                              !/*ge         14*/
      read (10,80) idm(n),jza(n),el1(n),pbl(n),q(n)                             !/*ge         15*/
      psum=psum+pbl(n)                                                          !/*ge         16*/
   20 continue                                                                  !/*ge         17*/
      do 30 n=1,nd                                                              !/*ge         18*/
      if (psum.gt.dp0) pbl(n)=pbl(n)/psum                                       !/*ge         19*/
      idk1=idm(n)                                                               !/*ge         20*/
      if (idk1.le.19) pb(idk1)=pb(idk1)+pbl(n)                                  !/*ge         21*/
   30 continue                                                                  !/*ge         22*/
      do 40 i=1,19                                                              !/*ge         23*/
   40 ptot=ptot+pb(i)                                                           !/*ge         24*/
   50 continue                                                                  !/*ge         25*/
      return                                                                    !/*ge         26*/
   60 continue                                                                  !/*ge         27*/
      iflag=1                                                                   !/*ge         28*/
      return                                                                    !/*ge         29*/
!                                                                               !/*ge         30*/
   70 format (i6,d11.4,1x,f3.0,1x,f4.1,d11.4,1x,i2)                             !/*ge         31*/
   80 format (38x,i2,1x,i6,3d11.4)                                              !/*ge         32*/
#endif                                                                          /*!ge         33*/
      end                                                                       !/*ge         34*/

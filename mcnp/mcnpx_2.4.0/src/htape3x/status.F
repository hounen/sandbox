!     $Id: status.F,v 1.4 2002/03/27 18:14:03 gwm Exp $                       !/*st          1*/
      subroutine status (io)                                                    !/*st          2*/
#include <htape3x/com32b.h>                                                     /*!st          3*/
#include <htape3x/label.h>                                                      /*!st          4*/
#include <htape3x/alloc.h>                                                      /*!st          5*/
#include <htape3x/bulk.h>                                                       /*!st          6*/
      if (istat.le.1) return                                                    !/*st          7*/
      write (io,20)                                                             !/*st          8*/
      nsum=0                                                                    !/*st          9*/
      do 10 i=1,ict                                                             !/*st         10*/
      nsum=nsum+len(i)                                                          !/*st         11*/
   10 write (io,30) names(i),len(i),iptt(i)                                     !/*st         12*/
      write (io,40) nsum,maxblk,ndas                                            !/*st         13*/
      return                                                                    !/*st         14*/
!                                                                               !/*st         15*/
   20 format (/' array status:'/)                                               !/*st         16*/
   30 format (5x,a6,i10,' words  pointer =',i8)                                 !/*st         17*/
   40 format (/' words used =',i9,' words allocated =',i9,                      !/*st         18*/
     & ' words available =',i9)                                                 !/*st         19*/
      end                                                                       !/*st         20*/

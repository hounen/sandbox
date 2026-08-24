!     $Id: partin.F,v 1.4 2002/03/27 18:15:13 gwm Exp $                       !/*pa          1*/
      subroutine partin                                                         !/*pa          2*/
#include <lcs/double.h>                                                         /*!pa          3*/
#include <lcs/btcom.h>                                                          /*!pa          4*/
      if (d(4)) 10,20,10                                                        !/*pa          5*/
   10 ipec(10)=ipec(10)+1                                                       !/*pa          6*/
!     no. of inc. particles on reg.1 only                                       !/*pa          7*/
      go to 50                                                                  !/*pa          8*/
   20 if (d(3)) 30,40,30                                                        !/*pa          9*/
   30 ipec(6)=ipec(6)+1                                                         !/*pa         10*/
!     no. of inc. particles on reg.2 only                                       !/*pa         11*/
      go to 50                                                                  !/*pa         12*/
   40 ipec(1)=ipec(1)+1                                                         !/*pa         13*/
!     no. of inc. particles on reg.3 only                                       !/*pa         14*/
   50 do 60 i=1,3                                                               !/*pa         15*/
   60 isw(i)=0                                                                  !/*pa         16*/
!     1=0 when start in rg.3 or rg.4                                            !/*pa         17*/
!     2=0 when in rg.3 not passing through rg.1                                 !/*pa         18*/
!     3=0 when in rg.2 not passing through rg.1                                 !/*pa         19*/
      return                                                                    !/*pa         20*/
      end                                                                       !/*pa         21*/

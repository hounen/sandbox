!     $Id: spac32.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                       !/*sp          1*/
      subroutine spac32 (i)                                                     !/*sp          2*/
#include <lcs/double.h>                                                         /*!sp          3*/
#include <lcs/btcom.h>                                                          /*!sp          4*/
      ex=dp0                                                                    !/*sp          5*/
      i4=5                                                                      !/*sp          6*/
      sign=9.99999d-1*space(i)                                                  !/*sp          7*/
      if (i-31) 10,20,30                                                        !/*sp          8*/
   10 i2=18                                                                     !/*sp          9*/
      i3=3                                                                      !/*sp         10*/
      go to 90                                                                  !/*sp         11*/
   20 i2=22                                                                     !/*sp         12*/
      i3=5                                                                      !/*sp         13*/
      go to 90                                                                  !/*sp         14*/
   30 if (i-41) 40,50,50                                                        !/*sp         15*/
   40 i2=26                                                                     !/*sp         16*/
      i3=7                                                                      !/*sp         17*/
      go to 90                                                                  !/*sp         18*/
   50 i4=3                                                                      !/*sp         19*/
      if (i-42) 60,70,80                                                        !/*sp         20*/
   60 i2=35                                                                     !/*sp         21*/
      i3=13                                                                     !/*sp         22*/
      go to 90                                                                  !/*sp         23*/
   70 i2=37                                                                     !/*sp         24*/
      i3=17                                                                     !/*sp         25*/
      go to 90                                                                  !/*sp         26*/
   80 i2=39                                                                     !/*sp         27*/
      i3=21                                                                     !/*sp         28*/
   90 call bg6b                                                                 !/*sp         29*/
      call signex                                                               !/*sp         30*/
      return                                                                    !/*sp         31*/
      end                                                                       !/*sp         32*/

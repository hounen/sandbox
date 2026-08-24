!     $Id: shxd.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                         !/*sh          1*/
      subroutine shxd                                                           !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
#include <lcs/tpcrs.h>                                                          /*!sh          4*/
      dexs=2.d1                                                                 !/*sh          5*/
      locx(1,1)=995                                                             !/*sh          6*/
      locx(2,1)=1153                                                            !/*sh          7*/
      locx(3,1)=3793                                                            !/*sh          8*/
      locx(4,1)=0                                                               !/*sh          9*/
      locx(1,2)=1283                                                            !/*sh         10*/
      locx(2,2)=1441                                                            !/*sh         11*/
      locx(3,2)=3969                                                            !/*sh         12*/
      locx(4,2)=0                                                               !/*sh         13*/
      locx(1,3)=2009                                                            !/*sh         14*/
      locx(2,3)=0                                                               !/*sh         15*/
      locx(3,3)=3667                                                            !/*sh         16*/
      locx(4,3)=0                                                               !/*sh         17*/
      locx(1,4)=2243                                                            !/*sh         18*/
      locx(2,4)=0                                                               !/*sh         19*/
      locx(3,4)=3541                                                            !/*sh         20*/
      locx(4,4)=3415                                                            !/*sh         21*/
      do 10 it=1,4                                                              !/*sh         22*/
      do 10 id=1,4                                                              !/*sh         23*/
   10 eth(id,it)=dp0                                                            !/*sh         24*/
      do 20 it=1,2                                                              !/*sh         25*/
      eth(1,it)=36.d1                                                           !/*sh         26*/
   20 eth(2,it)=92.d1                                                           !/*sh         27*/
      do 30 it=3,4                                                              !/*sh         28*/
   30 eth(1,it)=18.d1                                                           !/*sh         29*/
      return                                                                    !/*sh         30*/
      end                                                                       !/*sh         31*/

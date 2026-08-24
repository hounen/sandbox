!     $Id: conv.F,v 1.4 2002/03/27 18:16:24 gwm Exp $                         !/*co          1*/
      subroutine conv (line)                                                    !/*co          2*/
#ifdef UNICOS                                                                   /*!co          3*/
#include <htape3x/com32b.h>                                                     /*!co          4*/
      logical digit, start                                                      !/*co          5*/
      character line(80), xline(80), num(80), cm1, c, temp                      !/*co          6*/
      character *80 rep                                                         !/*co          7*/
      equivalence (num(1),rep)                                                  !/*co          8*/
      cm1=' '                                                                   !/*co          9*/
      ii=0                                                                      !/*co         10*/
      jj=0                                                                      !/*co         11*/
      start=.true.                                                              !/*co         12*/
      do 50 i=1,80                                                              !/*co         13*/
      digit=.false.                                                             !/*co         14*/
      c=line(i)                                                                 !/*co         15*/
      if (c.eq.','.and.cm1.eq.'*') then                                         !/*co         16*/
        do 10 j=1,jj                                                            !/*co         17*/
        temp=num(jj+1-j)                                                        !/*co         18*/
   10   num(81-j)=temp                                                          !/*co         19*/
        do 20 j=1,80-jj                                                         !/*co         20*/
   20   num(j)=' '                                                              !/*co         21*/
        read (rep,80) ir                                                        !/*co         22*/
        ii=istart                                                               !/*co         23*/
        do 30 j=1,ir                                                            !/*co         24*/
        ii=ii+1                                                                 !/*co         25*/
   30   xline(ii)=','                                                           !/*co         26*/
        go to 40                                                                !/*co         27*/
      elseif ('0'.le.c.and.c.le.'9') then                                       !/*co         28*/
        digit=.true.                                                            !/*co         29*/
        if (start) istart=ii                                                    !/*co         30*/
        start=.false.                                                           !/*co         31*/
        jj=jj+1                                                                 !/*co         32*/
        num(jj)=c                                                               !/*co         33*/
      endif                                                                     !/*co         34*/
      ii=ii+1                                                                   !/*co         35*/
      xline(ii)=c                                                               !/*co         36*/
      if (c.eq.'*'.or.digit) go to 50                                           !/*co         37*/
   40 start=.true.                                                              !/*co         38*/
      jj=0                                                                      !/*co         39*/
   50 cm1=c                                                                     !/*co         40*/
      if (ii.lt.80) then                                                        !/*co         41*/
        do 60 i=ii+1,80                                                         !/*co         42*/
   60   xline(i)=' '                                                            !/*co         43*/
      endif                                                                     !/*co         44*/
      do 70 i=1,80                                                              !/*co         45*/
   70 line(i)=xline(i)                                                          !/*co         46*/
      return                                                                    !/*co         47*/
!                                                                               !/*co         48*/
   80 format (i80)                                                              !/*co         49*/
#endif                                                                          /*!co         50*/
      end                                                                       !/*co         51*/

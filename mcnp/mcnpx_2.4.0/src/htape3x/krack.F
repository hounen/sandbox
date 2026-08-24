!     $Id: krack.F,v 1.4 2002/03/27 18:14:00 gwm Exp $                        !/*kr          1*/
      subroutine krack                                                          !/*kr          2*/
#include <htape3x/com32b.h>                                                     /*!kr          3*/
#include <htape3x/lpt.h>                                                        /*!kr          4*/
#include <htape3x/contrl.h>                                                     /*!kr          5*/
      character mscrt*6, line*1, ll*1, ll1*1                                    !/*kr          6*/
      dimension line(80)                                                        !/*kr          7*/
      data mscrt /'scrta'/                                                      !/*kr          8*/
      open (iscrt,status='scratch')                                             !/*kr          9*/
      write (io,80)                                                             !/*kr         10*/
      nc=0                                                                      !/*kr         11*/
   10 continue                                                                  !/*kr         12*/
      read (in,90,end=70) line                                                  !/*kr         13*/
      nc=nc+1                                                                   !/*kr         14*/
      write (io,100) nc,line                                                    !/*kr         15*/
      i1=1                                                                      !/*kr         16*/
      i2=80                                                                     !/*kr         17*/
      kk=0                                                                      !/*kr         18*/
! the test is nc.le.3 for use in HETC                                           !/*kr         19*/
      if (nc.le.2) go to 50                                                     !/*kr         20*/
      if (line(1).eq.'$') go to 50                                              !/*kr         21*/
#ifdef UNICOS                                                                   /*!kr         22*/
      call conv (line)                                                          !/*kr         23*/
#endif                                                                          /*!kr         24*/
      do 40 i=1,80                                                              !/*kr         25*/
      ll=line(i)                                                                !/*kr         26*/
      if (ll.ne.' ') kk=1                                                       !/*kr         27*/
      if (ll.ne.'/') go to 30                                                   !/*kr         28*/
      if (i.eq.1) go to 20                                                      !/*kr         29*/
      ll1=line(i-1)                                                             !/*kr         30*/
! This line is needed only for use in HETC                                      !/*kr         31*/
!     if (ll1.eq.'c'.or.ll1.eq.'k') go to 50                                    !/*kr         32*/
   20 continue                                                                  !/*kr         33*/
      write (iscrt,90) (line(j),j=i1,i)                                         !/*kr         34*/
      i1=i+1                                                                    !/*kr         35*/
      kk=0                                                                      !/*kr         36*/
   30 continue                                                                  !/*kr         37*/
   40 continue                                                                  !/*kr         38*/
      if (i1.gt.i2) go to 60                                                    !/*kr         39*/
   50 continue                                                                  !/*kr         40*/
      if (i1.eq.1.or.i1.gt.1.and.kk.ne.0) write (iscrt,90) (line(j),j=i1        !/*kr         41*/
     & ,i2)                                                                     !/*kr         42*/
   60 continue                                                                  !/*kr         43*/
      go to 10                                                                  !/*kr         44*/
   70 continue                                                                  !/*kr         45*/
      close (in)                                                                !/*kr         46*/
      in=iscrt                                                                  !/*kr         47*/
      endfile in                                                                !/*kr         48*/
      rewind in                                                                 !/*kr         49*/
      return                                                                    !/*kr         50*/
!                                                                               !/*kr         51*/
   80 format (//5x,'listing of card input'//)                                   !/*kr         52*/
   90 format (80a1)                                                             !/*kr         53*/
  100 format (' ',i5,5x,80a1)                                                   !/*kr         54*/
      end                                                                       !/*kr         55*/

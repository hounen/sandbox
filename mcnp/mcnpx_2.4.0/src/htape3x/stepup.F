!     $Id: stepup.F,v 1.4 2002/03/27 18:14:03 gwm Exp $                       !/*st          1*/
      subroutine stepup (hf,jt)                                                 !/*st          2*/
!        find a unique name for file hf.                                        !/*st          3*/
!        jt = tty output unit number.                                           !/*st          4*/
#include <htape3x/com32b.h>                                                     /*!st          5*/
      character hf*8, ht*8, ha*1                                                !/*st          6*/
!                                                                               !/*st          7*/
      n=0                                                                       !/*st          8*/
      j=index(hf//' ',' ')-1                                                    !/*st          9*/
      m=26                                                                      !/*st         10*/
      ha='a'                                                                    !/*st         11*/
      if (index('0123456789',hf(j:j)).eq.0) go to 10                            !/*st         12*/
      m=10                                                                      !/*st         13*/
      ha='0'                                                                    !/*st         14*/
   10 if (inqire(hf).eq.0) then                                                 !/*st         15*/
        if (jt.ne.0) write (jt,20) hf                                           !/*st         16*/
        return                                                                  !/*st         17*/
      endif                                                                     !/*st         18*/
      n=n+1                                                                     !/*st         19*/
      if (n.gt.m) call expire (0,'unique','can not create '//hf)                !/*st         20*/
      ht=hf                                                                     !/*st         21*/
      hf(j:j)=char(mod(ichar(ht(j:j))-ichar(ha)+1,m)+ichar(ha))                 !/*st         22*/
      go to 10                                                                  !/*st         23*/
!                                                                               !/*st         24*/
   20 format (1x,'file ',a8,' is created')                                      !/*st         25*/
      end                                                                       !/*st         26*/

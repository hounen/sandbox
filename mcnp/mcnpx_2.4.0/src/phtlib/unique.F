!     $Id: unique.F,v 1.4 2002/03/27 18:16:23 gwm Exp $                       !/*un          1*/
      subroutine unique (hf,jt)                                                 !/*un          2*/
!        find a unique name for file hf.                                        !/*un          3*/
!        jt = tty output unit number.                                           !/*un          4*/
#include <htape3x/com32b.h>                                                     /*!un          5*/
      character hf*8, ht*8, ha*1                                                !/*un          6*/
!                                                                               !/*un          7*/
      n=0                                                                       !/*un          8*/
      j=index(hf//' ',' ')-1                                                    !/*un          9*/
      m=26                                                                      !/*un         10*/
      ha='a'                                                                    !/*un         11*/
      if (index('0123456789',hf(j:j)).eq.0) go to 10                            !/*un         12*/
      m=10                                                                      !/*un         13*/
      ha='0'                                                                    !/*un         14*/
   10 if (inqire(hf).eq.0) return                                               !/*un         15*/
      n=n+1                                                                     !/*un         16*/
      if (n.gt.m) call expire (0,'unique','can not create '//hf)                !/*un         17*/
      ht=hf                                                                     !/*un         18*/
      hf(j:j)=char(mod(ichar(ht(j:j))-ichar(ha)+1,m)+ichar(ha))                 !/*un         19*/
      if (jt.ne.0) write (jt,20) ht,hf                                          !/*un         20*/
      go to 10                                                                  !/*un         21*/
!                                                                               !/*un         22*/
   20 format (1x,a8,' already exists.  ',a8,' is created instead')              !/*un         23*/
      end                                                                       !/*un         24*/

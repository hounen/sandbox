!     $Id: edit14.F,v 1.5 2002/03/27 18:13:57 gwm Exp $                       !/*ed          1*/
      subroutine edit14 (tlly,tlsq)                                             !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/lpt.h>                                                        /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
      dimension tlly(8,nbin), tlsq(8,nbin)                                      !/*ed          6*/
      write (io,20) icase,iopt,titl1,titl2                                      !/*ed          7*/
      do 10 i=1,nbin                                                            !/*ed          8*/
      if (lbunch) then                                                          !/*ed          9*/
        if (jmat.eq.0) write (io,80) (lparm(k),k=1,nparm)                       !/*ed         10*/
        if (jmat.ne.0) write (io,90) (lparm(k),k=1,nparm)                       !/*ed         11*/
      elseif (i.lt.nbin) then                                                   !/*ed         12*/
        if (jmat.eq.0) write (io,30) lparm(i)                                   !/*ed         13*/
        if (jmat.ne.0) write (io,40) lparm(i)                                   !/*ed         14*/
      else                                                                      !/*ed         15*/
        write (io,50)                                                           !/*ed         16*/
      endif                                                                     !/*ed         17*/
      if (lsnprm) write (io,100) dnparm(i)                                      !/*ed         18*/
      f=1.0/dnparm(i)                                                           !/*ed         19*/
      write (io,60) (f*tlly(j,i),tlsq(j,i),j=1,3),f*tlly(6,i),tlsq(6,i)         !/*ed         20*/
   10 write (io,70) (f*tlly(j,i),tlsq(j,i),j=4,5),(f*tlly(j,i),tlsq(j,i)        !/*ed         21*/
     & ,j=7,8)                                                                  !/*ed         22*/
      return                                                                    !/*ed         23*/
!                                                                               !/*ed         24*/
   20 format ('1',20x,'case no.',i3,5x,'option no.',i3//(10x,10a8))             !/*ed         25*/
   30 format (/35x,' gas production in cell',i4)                                !/*ed         26*/
   40 format (/35x,' gas production in material',i4)                            !/*ed         27*/
   50 format (/35x,'total gas production')                                      !/*ed         28*/
   60 format (/12x,'hydrogen',9x,'deuterium',10x,'tritium',11x,'total h'        !/*ed         29*/
     & /5x,4(1pd12.3,1x,0pf5.3))                                                !/*ed         30*/
   70 format (/12x,'helium-3',10x,'helium-4',10x,'total he',9x,                 !/*ed         31*/
     & 'gas(h&he)'/5x,4(1pd12.3,1x,0pf5.3))                                     !/*ed         32*/
   80 format (/10x,' gas production in cells',15i4/(34x,15i4))                  !/*ed         33*/
   90 format (/10x,' gas production in materials',15i4/(38x,15i4))              !/*ed         34*/
  100 format (/35x,'tally divided by',1pd14.5)                                  !/*ed         35*/
      end                                                                       !/*ed         36*/

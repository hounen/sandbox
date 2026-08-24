!     $Id: bcdout.F,v 1.4 2002/03/27 18:16:21 gwm Exp $                       !/*bc          1*/
      subroutine bcdout (nlcl)                                                  !/*bc          2*/
#include <htape3x/com32b.h>                                                     /*!bc          3*/
#include <trx/ioput.h>                                                          /*!bc          4*/
#include <htape3x/big.h>                                                        /*!bc          5*/
#include <trx/gross.h>                                                          /*!bc          6*/
      do 50 i=1,len0-1                                                          !/*bc          7*/
      na=iref(i+1)-iref(i)                                                      !/*bc          8*/
      if (na.le.0) go to 50                                                     !/*bc          9*/
      nlo=iref(i)                                                               !/*bc         10*/
      nup=nlo+na-1                                                              !/*bc         11*/
      do 40 j=nlo,nup                                                           !/*bc         12*/
      ipt0=ipt(j)                                                               !/*bc         13*/
      npt=ipt(j+1)-ipt0                                                         !/*bc         14*/
      write (20,80) izao(j),npt                                                 !/*bc         15*/
      n1=ipt0+npt-1                                                             !/*bc         16*/
      do 30 k=ipt0,n1                                                           !/*bc         17*/
      ndk=idk(k+1)-idk(k)                                                       !/*bc         18*/
      write (20,90) elo(k),isp(k),lcl(k),ndk,tlev(k)                            !/*bc         19*/
      if (ndk) 20,30,10                                                         !/*bc         20*/
   10 continue                                                                  !/*bc         21*/
      llo=idk(k)                                                                !/*bc         22*/
      lhi=llo+ndk-1                                                             !/*bc         23*/
      write (20,60) (pmode(l),lfin(l),l=llo,lhi)                                !/*bc         24*/
      go to 30                                                                  !/*bc         25*/
   20 continue                                                                  !/*bc         26*/
      write (nout,70) izao(j),elo(k),idk(k),idk(k+1)                            !/*bc         27*/
   30 continue                                                                  !/*bc         28*/
   40 continue                                                                  !/*bc         29*/
   50 continue                                                                  !/*bc         30*/
      iend=900000                                                               !/*bc         31*/
      write (20,80) iend,nlcl                                                   !/*bc         32*/
      return                                                                    !/*bc         33*/
!                                                                               !/*bc         34*/
   60 format (5(f12.8,i4))                                                      !/*bc         35*/
   70 format ('bcdout error',i8,1pd12.4,2i8)                                    !/*bc         36*/
   80 format (i8,i7)                                                            !/*bc         37*/
   90 format (1pd12.5,i5,i2,i3,1pd12.5)                                         !/*bc         38*/
      end                                                                       !/*bc         39*/

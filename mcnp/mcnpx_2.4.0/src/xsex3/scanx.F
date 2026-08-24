!     $Id: scanx.F,v 1.10 2002/03/27 18:16:26 gwm Exp $                        !/*sc          1*/
      subroutine scanx                                                          !/*sc          2*/
! preprocessor routine for program XSEX                                         !/*sc          3*/
      use global_parameters, only : kod,ver,loddat                              !/*sca         1*/
#include <htape3x/com32b.h>                                                     /*!sc          4*/
#include <htape3x/label.h>                                                      /*!sc          6*/
#include <htape3x/contrl.h>                                                     /*!sc          7*/
#include <xsex3/charxs.h>                                                       /*!sc          8*/
      character idtm*19,ie*8                                                    !/*sca         3*/
      do 10 i=1,lpt                                                             !/*sc         10*/
   10 ncp(i)=0                                                                  !/*sc         11*/
      ncase=0                                                                   !/*sc         12*/
      nc=0                                                                      !/*sc         13*/
      lerg(1)=0                                                                 !/*sc         14*/
      lang(1)=0                                                                 !/*sc         15*/
      ltyp(1)=0                                                                 !/*sc         16*/
      nsize(1)=0                                                                !/*sc         17*/
   20 continue                                                                  !/*sc         18*/
      read (in,40,end=30) idmm                                                  !/*sc         19*/
      read (in,40,end=30) idmm                                                  !/*sc         20*/
      nc=nc+1                                                                   !/*sc         21*/
      read (in,*,end=30) nerg,nang,dmm,idmm,idmm,idmm,ltest                     !/*sc         22*/
      if ((.not.ltesting).and.(ltest.ne.0)) ltesting = .true.                   !/*sc         23*/
      if (nerg.ne.0) read (in,*,end=30) (dmm,i=1,abs(nerg))                     !/*sc         24*/
      if (nang.ne.0) read (in,*,end=30) (dmm,i=1,abs(nang))                     !/*sc         25*/
      nnerg(nc)=abs(nerg)                                                       !/*sc         26*/
      nnang(nc)=abs(nang)                                                       !/*sc         27*/
      nntype(nc)=lpt1                                                           !/*sc         28*/
      ntype1(nc)=lpt1                                                           !/*sc         29*/
      nerg1(nc)=nnerg(nc)+2                                                     !/*sc         30*/
      nang1(nc)=nnang(nc)+1                                                     !/*sc         31*/
      lerg(nc+1)=lerg(nc)+nerg1(nc)*ndp2                                        !/*sc         32*/
      lang(nc+1)=lang(nc)+nang1(nc)*ndp2                                        !/*sc         33*/
      ltyp(nc+1)=ltyp(nc)+ntype1(nc)                                            !/*sc         34*/
      nsize(nc+1)=nsize(nc)+nerg1(nc)*nang1(nc)*ntype1(nc)*ndp2                 !/*sc         35*/
      ncase=nc                                                                  !/*sc         36*/
      go to 20                                                                  !/*sc         37*/
   30 continue                                                                  !/*sc         38*/
      i3=3                                                                      !/*sc         39*/
      i4=4                                                                      !/*sc         40*/
      nc=ncase+1                                                                !/*sc         41*/
!     maxblk is # of s.p. words                                                 !/*sc         42*/
      maxblk=3*nsize(nc)+lerg(nc)+lang(nc)+ltyp(nc)+3                           !/*sc         43*/
      call chgmem (mdas,lfll,maxblk,ie)                                         !/*sc         44*/
      iptll=3                                                                   !/*sc         45*/
      iptls=iptll+nsize(nc)                                                     !/*sc         46*/
      iphld=iptls+nsize(nc)                                                     !/*sc         47*/
      iperg=iphld+nsize(nc)                                                     !/*sc         48*/
      ipang=iperg+lerg(nc)                                                      !/*sc         49*/
      iptyp=ipang+lang(nc)                                                      !/*sc         50*/
      nup=maxblk-2                                                              !/*sc         51*/
      do 50 i=1,nup                                                             !/*sc         52*/
      iblk(iptll-1+i)=0                                                         !/*sc         53*/
   50 continue                                                                  !/*sc         54*/
      call getidt (idtm)                                                        !/*sc         55*/
      if(.not.ltesting) write (io,60) loddat,idtm                               !/*sc         56*/
      write (jtty,60) loddat,idtm                                               !/*sc         57*/
      write (jtty,70) inxs,outxs,histp                                          !/*sc         58*/
      rewind in                                                                 !/*sc         59*/
      rewind nhst                                                               !/*sc         60*/
      return                                                                    !/*sc         61*/
   60 format (' Running XSEX3 of ',a28,4x,a19)                                  !/*sc         62*/
   70 format (5x,'input file =',a8,5x,'output file =',a8,                       !/*sc         63*/
     & 'history tape =',a8)                                                     !/*sc         64*/
   40 format(a8)                                                                !/*sc         65*/
      end                                                                       !/*sc         66*/

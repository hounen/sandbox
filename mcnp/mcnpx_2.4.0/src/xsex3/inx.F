!     $Id: inx.F,v 1.8 2002/03/27 18:16:25 gwm Exp $                          !/*in          1*/
      subroutine inx (ergb,xmu,itype,nc)                                        !/*in          2*/
! input routine for program XSEX                                                !/*in          3*/
#include <htape3x/com32b.h>                                                     /*!in          4*/
#include <htape3x/label.h>                                                      /*!in          5*/
#include <htape3x/contrl.h>                                                     /*!in          6*/
#include <xsex3/charxs.h>                                                       /*!in          7*/
      dimension ergb(1), xmu(1), itype(1), jtype(lpt)                           !/*in          8*/
      data jtype /5*1,2*0,11*1,4*0/                                             !/*in          9*/
      data conv /360.d0/                                                        !/*in         10*/
      read (in,60,end=50) jtitl1(nc)                                            !/*in         11*/
      read (in,60,end=50) jtitl2(nc)                                            !/*in         12*/
      write (io,70) nc,jtitl1(nc),jtitl2(nc)                                    !/*in         13*/
      fnorm=dp0                                                                 !/*in         14*/
      kplot=0                                                                   !/*in         15*/
      imom=0                                                                    !/*in         16*/
      iyield=0                                                                  !/*in         17*/
      do 10 i=1,lpt                                                             !/*in         18*/
   10 itype(i)=jtype(i)                                                         !/*in         19*/
      read (in,*) nerg,nang,fnorm,kplot,imom,iyield,ltest                       !/*in         20*/
      write (io,80) nerg,nang,fnorm,kplot,imom,iyield                           !/*in         21*/
      if (ltesting) write (io,130) ltest                                        !/*in         22*/
      if (fnorm.eq.dp0) fnorm=1.0                                               !/*in         23*/
      fnorm0(nc)=fnorm                                                          !/*in         24*/
      kplot0(nc)=kplot                                                          !/*in         25*/
      imom0(nc)=imom                                                            !/*in         26*/
      iyeld0(nc)=iyield                                                         !/*in         27*/
      if (nerg.gt.0) then                                                       !/*in         28*/
        read (in,*) (ergb(i),i=1,nerg)                                          !/*in         29*/
        call fixe (ergb,nerg)                                                   !/*in         30*/
      endif                                                                     !/*in         31*/
      ergb(nerg+1)=1.0e+08                                                      !/*in         32*/
      if (nang.gt.0) then                                                       !/*in         33*/
        l_deg=.false.                                                           !/*in         34*/
        read (in,*) (xmu(i),i=1,nang)                                           !/*in         35*/
        if (xmu(1).eq.dp0.and.xmu(nang).eq.dp0) then                            !/*in         36*/
          dell=2.0/nang                                                         !/*in         37*/
          do 20 i=1,nang                                                        !/*in         38*/
   20     xmu(i)=1.0-dell*(nang-i)                                              !/*in         39*/
        endif                                                                   !/*in         40*/
      elseif (nang.lt.0) then                                                   !/*in         41*/
        l_deg=.true.                                                            !/*in         42*/
        nang=-nang                                                              !/*in         43*/
        read (in,*) (xmu(i),i=1,nang)                                           !/*in         44*/
        if (xmu(1).eq.dp0.and.xmu(nang).eq.dp0) then                            !/*in         45*/
          dell=180.0/nang                                                       !/*in         46*/
          do 30 i=1,nang                                                        !/*in         47*/
   30     xmu(i)=dell*(nang-i)                                                  !/*in         48*/
        endif                                                                   !/*in         49*/
        do 40 i=1,nang                                                          !/*in         50*/
        xmu(i)=cos(dppi*xmu(i)/180.d0)                                          !/*in         51*/
   40   continue                                                                !/*in         52*/
      endif                                                                     !/*in         53*/
      if (nang.eq.0) then                                                       !/*in         54*/
        xmu(1)=1.0                                                              !/*in         55*/
        nang=1                                                                  !/*in         56*/
      endif                                                                     !/*in         57*/
      if (imom.eq.0) write (io,90) (ergb(i),i=1,nerg)                           !/*in         58*/
      if (imom.ne.0) write (io,110) (ergb(i),i=1,nerg)                          !/*in         59*/
      write (io,100) (xmu(i),i=1,nang)                                          !/*in         60*/
      write (io,120) (180.d0*acos(xmu(i))/dppi,i=1,nang)                        !/*in         61*/
      return                                                                    !/*in         62*/
   50 continue                                                                  !/*in         63*/
      call errorf ('inx.1   ')                                                  !/*in         64*/
!                                                                               !/*in         65*/
   60 format (a80)                                                              !/*in         66*/
   70 format (1h1,19x,'case',i3//10x,a80//10x,a80)                              !/*in         67*/
   80 format (//10x,'number of energy bins =',i4/10x,                           !/*in         68*/
     & 'number of angular bins =',i4/10x,'normalization constant =',            !/*in         69*/
     & 1pd14.6/10x,'plot control =',i2/10x,'energy/momentum option =',          !/*in         70*/
     & i2/10x,'cross section/yield option =',i2)                                !/*in         71*/
   90 format (/10x,'energy bin boundaries :'/(10x,1p8d13.4))                    !/*in         72*/
  100 format (/10x,'cosine bin boundaries :'/(10x,5f20.15))                     !/*in         73*/
  110 format (/10x,'momentum bin boundaries :'/(10x,1p8d13.4))                  !/*in         74*/
  120 format (/10x,'degree bin boundaries :'/(10x,5f20.15))                     !/*in         75*/
  130 format (10x,'testing flag =',i4)                                          !/*in         76*/
      end                                                                       !/*in         77*/

!     $Id: edx.F,v 1.8 2002/03/27 18:16:24 gwm Exp $                          !/*ed          1*/
      subroutine edx (tlly,tlsq,ndim1,ndim2,dele,dela,ergb,xmu,itype,nc)        !/*ed          2*/
! editing routine for program XSEX                                              !/*ed          3*/
#include <htape3x/com32b.h>                                                     /*!ed          4*/
#include <htape3x/label.h>                                                      /*!ed          5*/
#include <htape3x/contrl.h>                                                     /*!ed          6*/
#include <xsex3/charxs.h>                                                       /*!ed          7*/
      dimension tlly(ndim1,ndim2,1), tlsq(ndim1,ndim2,1), dela(1), dele(        !/*ed          8*/
     & 1), ergb(1), xmu(1), itype(1)                                            !/*ed          9*/
      character llang*7                                                         !/*ed         10*/
      data llang /'mu max='/                                                    !/*ed         11*/
      ncas=maxbch*maxcas                                                        !/*ed         12*/
      fcas=ncas                                                                 !/*ed         13*/
      cnorm=fnorm/fcas                                                          !/*ed         14*/
      dnorm=dp0                                                                 !/*ed         15*/
      if (ncas.gt.1) dnorm=1.0/(fcas-1.0)                                       !/*ed         16*/
      enorm=dnorm*fcas                                                          !/*ed         17*/
      eold=dp0                                                                  !/*ed         18*/
      nhi=nerg+1                                                                !/*ed         19*/
      do 10 i=1,nhi                                                             !/*ed         20*/
      dele(i)=ergb(i)-eold                                                      !/*ed         21*/
      eold=ergb(i)                                                              !/*ed         22*/
   10 continue                                                                  !/*ed         23*/
      aold=-1.0                                                                 !/*ed         24*/
      do 20 i=1,nang                                                            !/*ed         25*/
      dela(i)=twopi*(xmu(i)-aold)                                               !/*ed         26*/
      aold=xmu(i)                                                               !/*ed         27*/
   20 continue                                                                  !/*ed         28*/
      geomx=fcas*cnorm                                                          !/*ed         29*/
!     if (iyield.ne.0) cnorm=cnorm/tlnon                                        !/*ed         30*/
      do 30 k=1,ntype                                                           !/*ed         31*/
      do 30 j=1,ndim2                                                           !/*ed         32*/
      do 30 i=1,ndim1                                                           !/*ed         33*/
      tem=tlly(i,j,k)                                                           !/*ed         34*/
      b=tlsq(i,j,k)                                                             !/*ed         35*/
      if (b.gt.0) then                                                          !/*ed         36*/
        c=abs(tem)                                                              !/*ed         37*/
        b=enorm*b-dnorm*c**2                                                    !/*ed         38*/
        if (b.lt.dp0) b=dp0                                                     !/*ed         39*/
        b=sqrt(b)/c                                                             !/*ed         40*/
        tlsq(i,j,k)=b                                                           !/*ed         41*/
        tem=cnorm*tem                                                           !/*ed         42*/
        if (iyield.ne.0) then                                                   !/*ed         43*/
          if (k.ne.lpt1) then                                                   !/*ed         44*/
            tem=tem/tlnon                                                       !/*ed         45*/
          else                                                                  !/*ed         46*/
            if (tlels.gt.dp0) tem=tem/tlels                                     !/*ed         47*/
          endif                                                                 !/*ed         48*/
        endif                                                                   !/*ed         49*/
        if (j.lt.ndim2) tem=tem/dele(j)                                         !/*ed         50*/
        if (i.lt.ndim1) tem=tem/dela(i)                                         !/*ed         51*/
        tlly(i,j,k)=tem                                                         !/*ed         52*/
      endif                                                                     !/*ed         53*/
   30 continue                                                                  !/*ed         54*/
      do 100 k=1,ntype                                                          !/*ed         55*/
      val1=tlly(ndim1,ndim2,k)                                                  !/*ed         56*/
      sd1=tlsq(ndim1,ndim2,k)                                                   !/*ed         57*/
      if (k.eq.lpt1) then                                                       !/*ed         58*/
        if (iyield.eq.0) write (io,120) nc,kpart(k)                             !/*ed         59*/
        if (iyield.ne.0) write (io,140) nc                                      !/*ed         60*/
      else                                                                      !/*ed         61*/
        if (iyield.eq.0) write (io,110) nc,kpart(k)                             !/*ed         62*/
        if (iyield.ne.0) write (io,130) nc,kpart(k)                             !/*ed         63*/
      endif                                                                     !/*ed         64*/
      if (val1.le.dp0) go to 90                                                 !/*ed         65*/
      nup=nerg+1                                                                !/*ed         66*/
      do 40 j=1,nup                                                             !/*ed         67*/
      jj=nerg-j+2                                                               !/*ed         68*/
      if (tlly(ndim1,jj,k).gt.dp0) go to 50                                     !/*ed         69*/
   40 continue                                                                  !/*ed         70*/
   50 continue                                                                  !/*ed         71*/
      nup0=jj                                                                   !/*ed         72*/
      if (nang.gt.1) then                                                       !/*ed         73*/
        ilo=1                                                                   !/*ed         74*/
   60   continue                                                                !/*ed         75*/
        ihi=min(ilo+6,nang)                                                     !/*ed         76*/
        if (imom.eq.0) write (io,180) (llang,xmu(i),i=ilo,ihi)                  !/*ed         77*/
        if (imom.ne.0) write (io,190) (llang,xmu(i),i=ilo,ihi)                  !/*ed         78*/
        if (nerg.gt.1) then                                                     !/*ed         79*/
          do 70 j=1,nup0                                                        !/*ed         80*/
          write (io,150) ergb(j),(tlly(i,j,k),tlsq(i,j,k),i=ilo,ihi)            !/*ed         81*/
   70     continue                                                              !/*ed         82*/
        endif                                                                   !/*ed         83*/
        if (ihi.lt.nang) then                                                   !/*ed         84*/
          ilo=ilo+7                                                             !/*ed         85*/
          go to 60                                                              !/*ed         86*/
        endif                                                                   !/*ed         87*/
      endif                                                                     !/*ed         88*/
      if (imom.eq.0) write (io,200)                                             !/*ed         89*/
      if (imom.ne.0) write (io,210)                                             !/*ed         90*/
      nup=max(nup0,nang)                                                        !/*ed         91*/
      do 80 i=1,nup                                                             !/*ed         92*/
      if (i.gt.nup0) then                                                       !/*ed         93*/
        write (io,230) xmu(i),180.d0*acos(xmu(i))/dppi,tlly(i,ndim2,k),         !/*ed         94*/
     &  tlsq(i,ndim2,k)                                                         !/*ed         95*/
      elseif (i.gt.nang) then                                                   !/*ed         96*/
        write (io,240) ergb(i),tlly(ndim1,i,k),tlsq(ndim1,i,k)                  !/*ed         97*/
      else                                                                      !/*ed         98*/
        write (io,220) ergb(i),tlly(ndim1,i,k),tlsq(ndim1,i,k),xmu(i)           !/*ed         99*/
     &  ,180.d0*acos(xmu(i))/dppi,tlly(i,ndim2,k),tlsq(i,ndim2,k)               !/*ed        100*/
      endif                                                                     !/*ed        101*/
   80 continue                                                                  !/*ed        102*/
   90 continue                                                                  !/*ed        103*/
      if (k.lt.lpt1) then                                                       !/*ed        104*/
        if (tlnon.gt.dp0) then                                                  !/*ed        105*/
          temp=val1/tlnon                                                       !/*ed        106*/
        else                                                                    !/*ed        107*/
          temp=dp0                                                              !/*ed        108*/
        endif                                                                   !/*ed        109*/
        if (iyield.ne.0) then                                                   !/*ed        110*/
          val1=val1*tlnon                                                       !/*ed        111*/
          temp=temp*tlnon                                                       !/*ed        112*/
        endif                                                                   !/*ed        113*/
        write (io,160) kpart(k),val1,sd1,temp                                   !/*ed        114*/
      else                                                                      !/*ed        115*/
        if (tlels.gt.dp0) then                                                  !/*ed        116*/
          temp=val1/tlels                                                       !/*ed        117*/
          if (iyield.ne.0) then                                                 !/*ed        118*/
            val1=val1*tlels                                                     !/*ed        119*/
            temp=temp*tlels                                                     !/*ed        120*/
          endif                                                                 !/*ed        121*/
        else                                                                    !/*ed        122*/
          val1=dp0                                                              !/*ed        123*/
          sd1=dp0                                                               !/*ed        124*/
          temp=dp0                                                              !/*ed        125*/
        endif                                                                   !/*ed        126*/
        write (io,170) kpart(k),val1,sd1,temp                                   !/*ed        127*/
      endif                                                                     !/*ed        128*/
  100 continue                                                                  !/*ed        129*/
      return                                                                    !/*ed        130*/
!                                                                               !/*ed        131*/
!.......................................................................        !/*ed        132*/
!                                                                               !/*ed        133*/
  110 format (1h1,40x,'case number',i3/1h0,40x,a8,                              !/*ed        134*/
     & ' production cross section')                                             !/*ed        135*/
  120 format (1h1,40x,'case number',i3/1h0,40x,a8,                              !/*ed        136*/
     & ' scattering cross section')                                             !/*ed        137*/
  130 format (1h1,40x,'case number',i3/1h0,40x,a8,' yield')                     !/*ed        138*/
  140 format (1h1,40x,'case number',i3/1h0,40x,'elastic distribution')          !/*ed        139*/
  150 format (1x,1pd9.3,7(1pd11.3,0pf6.3))                                      !/*ed        140*/
  160 format (//25x,'total ',a8,' production cross section =',1pd11.3,0p        !/*ed        141*/
     & f6.3,' yield =',1pd13.5/)                                                !/*ed        142*/
  170 format (//25x,'total ',a8,' scattering cross section =',1pd11.3,0p        !/*ed        143*/
     & f6.3,' yield =',1pd13.5/)                                                !/*ed        144*/
  180 format (/4x,'E(MeV)',7(2x,a7,f8.5))                                       !/*ed        145*/
  190 format (/2x,'P(MeV/c)',7(2x,a7,f8.5))                                     !/*ed        146*/
  200 format (/10x,'E(MeV)',3x,'angle integrated',10x,'mu max',                 !/*ed        147*/
     & 10x,'deg min',4x,'energy integrated')                                    !/*ed        148*/
  210 format (/8x,'P(MeV/c)',3x,'angle integrated',10x,'mu max',                !/*ed        149*/
     & 10x,'deg min',4x,'energy integrated')                                    !/*ed        150*/
  220 format (1pd16.4,1pd13.3,0pf6.3,5x,0pf15.12,f14.7,1pd13.3,0pf6.3)          !/*ed        151*/
  230 format (40x,0pf15.12,f14.7,1pd13.3,0pf6.3)                                !/*ed        152*/
  240 format (1pd16.4,1pd13.3,0pf6.3)                                           !/*ed        153*/
      end                                                                       !/*ed        154*/

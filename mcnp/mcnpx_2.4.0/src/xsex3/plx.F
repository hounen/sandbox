!     $Id: plx.F,v 1.6 2002/03/27 18:16:26 gwm Exp $                          !/*pl          1*/
      subroutine plx (tlly,tlsq,ndim1,ndim2,y0,xx1,xx,yy,ergb,xmu,itype)        !/*pl          2*/
! plotting routine for program XSEX                                             !/*pl          3*/
#ifdef XSPLOT                                                                   /*!pl          4*/
#include <htape3x/com32b.h>                                                     /*!pl          5*/
      real xx1, xx, yy                                                          !/*pl          6*/
#include <htape3x/lpt.h>                                                        /*!pl          7*/
#include <htape3x/contrl.h>                                                     /*!pl          8*/
#include <xsex3/charxs.h>                                                       /*!pl          9*/
!                                                                               !/*pl         10*/
!     XX from 1 to 2*NERG+1                                                     !/*pl         11*/
!     XX1 from 1 to 2*NANG                                                      !/*pl         12*/
!     Y0 from 1 to NERG                                                         !/*pl         13*/
      dimension tlly(ndim1,ndim2,1), tlsq(ndim1,ndim2,1)                        !/*pl         14*/
      dimension xx1(1), xx(1), y0(1), yy(1), ergb(1), xmu(1), itype(1)          !/*pl         15*/
      logical logx, logy                                                        !/*pl         16*/
      character labx*16, laby1*22, laby2*25, prx*11, pra*12, bin*3,             !/*pl         17*/
     & title*37, labx1*26, laby3*15, pri1*17, pri2*18, llang*8                  !/*pl         18*/
      character*11 sct                                                          !/*pl         19*/
      data sct /' scattering'/                                                  !/*pl         20*/
      data llang /'mu max ='/, labx1 /'cosine of scattering angle'/, prx        !/*pl         21*/
     & /' production'/, pra /' angular bin'/, pri1 /' angle-integrated'/        !/*pl         22*/
     & , pri2 /' energy-integrated'/                                            !/*pl         23*/
      logy=.false.                                                              !/*pl         24*/
      logx=.false.                                                              !/*pl         25*/
      if (abs(kplot).le.2) logy=.true.                                          !/*pl         26*/
      if (mod(abs(kplot),2).eq.1) logx=.true.                                   !/*pl         27*/
      if (imom.eq.0) then                                                       !/*pl         28*/
        labx='energy (mev)'                                                     !/*pl         29*/
        if (iyield.eq.0) then                                                   !/*pl         30*/
          laby1='xsec (barns per mev)'                                          !/*pl         31*/
          laby2='xsec (barns per mev-sr)'                                       !/*pl         32*/
        else                                                                    !/*pl         33*/
          laby1='yield per mev'                                                 !/*pl         34*/
          laby2='yield per mev-sr'                                              !/*pl         35*/
        endif                                                                   !/*pl         36*/
      else                                                                      !/*pl         37*/
        labx='momentum (mev/c)'                                                 !/*pl         38*/
        if (iyield.eq.0) then                                                   !/*pl         39*/
          laby1='xsec (barns per mev/c)'                                        !/*pl         40*/
          laby2='xsec (barns per mev/c-sr)'                                     !/*pl         41*/
        else                                                                    !/*pl         42*/
          laby1='yield per mev/c'                                               !/*pl         43*/
          laby2='yield per mev/c-sr'                                            !/*pl         44*/
        endif                                                                   !/*pl         45*/
      endif                                                                     !/*pl         46*/
      if (iyield.eq.0) then                                                     !/*pl         47*/
        laby3='xsec (barns/sr)'                                                 !/*pl         48*/
      else                                                                      !/*pl         49*/
        laby3='yield per sr'                                                    !/*pl         50*/
      endif                                                                     !/*pl         51*/
      write (io,180)                                                            !/*pl         52*/
      nup=2*nerg+2                                                              !/*pl         53*/
      if (logx) then                                                            !/*pl         54*/
        xx(1)=0.3*ergb(1)                                                       !/*pl         55*/
      else                                                                      !/*pl         56*/
        xx(1)=dp0                                                               !/*pl         57*/
      endif                                                                     !/*pl         58*/
      xx(2)=xx(1)                                                               !/*pl         59*/
      do 10 i=1,nerg                                                            !/*pl         60*/
      j=2*i+1                                                                   !/*pl         61*/
      xx(j)=ergb(i)                                                             !/*pl         62*/
   10 xx(j+1)=ergb(i)                                                           !/*pl         63*/
      nup1=2*nang                                                               !/*pl         64*/
      xx1(1)=-1.0                                                               !/*pl         65*/
      xx1(nup1)=xmu(nang)                                                       !/*pl         66*/
      nhi=nang-1                                                                !/*pl         67*/
      do 20 i=1,nhi                                                             !/*pl         68*/
      j=2*i                                                                     !/*pl         69*/
      xx1(j)=xmu(i)                                                             !/*pl         70*/
   20 xx1(j+1)=xmu(i)                                                           !/*pl         71*/
      do 170 l=1,ntype                                                          !/*pl         72*/
      if (tlly(ndim1,ndim2,l).le.dp0) go to 170                                 !/*pl         73*/
      if (l.eq.lpt1) then                                                       !/*pl         74*/
        if (imom.eq.0) then                                                     !/*pl         75*/
          if (iyield.ne.0) then                                                 !/*pl         76*/
            laby1='fraction per mev'                                            !/*pl         77*/
            laby2='fraction per mev-sr'                                         !/*pl         78*/
          endif                                                                 !/*pl         79*/
        else                                                                    !/*pl         80*/
          if (iyield.ne.0) then                                                 !/*pl         81*/
            laby1='fraction per mev/c'                                          !/*pl         82*/
            laby2='fraction per mev/c-sr'                                       !/*pl         83*/
          endif                                                                 !/*pl         84*/
        endif                                                                   !/*pl         85*/
        if (iyield.ne.0) then                                                   !/*pl         86*/
          laby3='fraction per sr'                                               !/*pl         87*/
        endif                                                                   !/*pl         88*/
      endif                                                                     !/*pl         89*/
!     Angle-integrated                                                          !/*pl         90*/
      do 30 i=1,nerg                                                            !/*pl         91*/
   30 y0(i)=tlly(ndim1,i,l)                                                     !/*pl         92*/
      do 40 i=1,nerg                                                            !/*pl         93*/
      nlo=i                                                                     !/*pl         94*/
      if (y0(i).gt.dp0) go to 50                                                !/*pl         95*/
   40 continue                                                                  !/*pl         96*/
      go to 170                                                                 !/*pl         97*/
   50 continue                                                                  !/*pl         98*/
      nlo=max((nlo-1),1)                                                        !/*pl         99*/
      do 60 i=1,nerg                                                            !/*pl        100*/
      ntp=nerg+1-i                                                              !/*pl        101*/
      if (y0(ntp).gt.dp0) go to 70                                              !/*pl        102*/
   60 continue                                                                  !/*pl        103*/
      go to 170                                                                 !/*pl        104*/
   70 continue                                                                  !/*pl        105*/
      if (l.eq.lpt1) then                                                       !/*pl        106*/
        title=kpart(l)//sct//pri1                                               !/*pl        107*/
      else                                                                      !/*pl        108*/
        title=kpart(l)//prx//pri1                                               !/*pl        109*/
      endif                                                                     !/*pl        110*/
      n1=2*nlo-1                                                                !/*pl        111*/
      nup=2*ntp+2                                                               !/*pl        112*/
      n2=nup-n1+1                                                               !/*pl        113*/
      if (logy) then                                                            !/*pl        114*/
        call fix (base,y0,yy,nerg)                                              !/*pl        115*/
        if (base.le.dp0) go to 170                                              !/*pl        116*/
        if (logx) then                                                          !/*pl        117*/
          call hlogxy (xx(n1),yy(n1),n2,labx,laby1,title)                       !/*pl        118*/
        else                                                                    !/*pl        119*/
          call hxlogy (xx(n1),yy(n1),n2,labx,laby1,title)                       !/*pl        120*/
        endif                                                                   !/*pl        121*/
      else                                                                      !/*pl        122*/
        base=dp0                                                                !/*pl        123*/
        call fix1 (base,y0,yy,nerg)                                             !/*pl        124*/
        if (logx) then                                                          !/*pl        125*/
          call hylogx (xx(n1),yy(n1),n2,labx,laby1,title)                       !/*pl        126*/
        else                                                                    !/*pl        127*/
          call hxy (xx(n1),yy(n1),n2,labx,laby1,title)                          !/*pl        128*/
        endif                                                                   !/*pl        129*/
      endif                                                                     !/*pl        130*/
      do 80 i=1,nerg                                                            !/*pl        131*/
   80 y0(i)=(1.0+tlsq(ndim1,i,l))*tlly(ndim1,i,l)                               !/*pl        132*/
      call fix1 (base,y0,yy,nerg)                                               !/*pl        133*/
      call hcvls ('sdash','off')                                                !/*pl        134*/
      call hcurve (xx(n1),yy(n1),n2)                                            !/*pl        135*/
      do 90 i=1,nerg                                                            !/*pl        136*/
   90 y0(i)=(1.0-tlsq(ndim1,i,l))*tlly(ndim1,i,l)                               !/*pl        137*/
      call fix1 (base,y0,yy,nerg)                                               !/*pl        138*/
      call hcurve (xx(n1),yy(n1),n2)                                            !/*pl        139*/
      call hcvls ('solid','off')                                                !/*pl        140*/
      call gpage                                                                !/*pl        141*/
      nplot=nplot+1                                                             !/*pl        142*/
      write (io,190) nplot,icase,title                                          !/*pl        143*/
      if (nang.le.1) go to 170                                                  !/*pl        144*/
!     Energy-integrated                                                         !/*pl        145*/
      do 100 j=1,nang                                                           !/*pl        146*/
      y0(j)=tlly(j,ndim2,l)                                                     !/*pl        147*/
  100 continue                                                                  !/*pl        148*/
      call fix (base,y0,yy,nang)                                                !/*pl        149*/
      if (l.eq.lpt1) then                                                       !/*pl        150*/
        title=kpart(l)//sct//pri2                                               !/*pl        151*/
      else                                                                      !/*pl        152*/
        title=kpart(l)//prx//pri2                                               !/*pl        153*/
      endif                                                                     !/*pl        154*/
      call hxlogy (xx1,yy(2),nup1,labx1,laby3,title)                            !/*pl        155*/
      do 110 j=1,nang                                                           !/*pl        156*/
  110 y0(j)=(1.0+tlsq(j,ndim2,l))*tlly(j,ndim2,l)                               !/*pl        157*/
      call fix1 (base,y0,yy,nang)                                               !/*pl        158*/
      call hcvls ('sdash','off')                                                !/*pl        159*/
      call hcurve (xx1,yy(2),nup1)                                              !/*pl        160*/
      do 120 j=1,nang                                                           !/*pl        161*/
  120 y0(j)=(1.0-tlsq(j,ndim2,l))*tlly(j,ndim2,l)                               !/*pl        162*/
      call fix1 (base,y0,yy,nang)                                               !/*pl        163*/
      call hcurve (xx1,yy(2),nup1)                                              !/*pl        164*/
      call hcvls ('solid','off')                                                !/*pl        165*/
      call gpage                                                                !/*pl        166*/
!                                                                               !/*pl        167*/
      nplot=nplot+1                                                             !/*pl        168*/
      write (io,190) nplot,icase,title                                          !/*pl        169*/
      if (kplot.lt.0) go to 170                                                 !/*pl        170*/
      do 160 j=1,nang                                                           !/*pl        171*/
      write (bin,'(i3)') j                                                      !/*pl        172*/
      if (l.eq.lpt1) then                                                       !/*pl        173*/
        title=kpart(l)//sct//pra//bin                                           !/*pl        174*/
      else                                                                      !/*pl        175*/
        title=kpart(l)//prx//pra//bin                                           !/*pl        176*/
      endif                                                                     !/*pl        177*/
      xmax=dp0                                                                  !/*pl        178*/
      do 130 i=1,nerg                                                           !/*pl        179*/
      xmax=max(xmax,y0(i))                                                      !/*pl        180*/
  130 y0(i)=tlly(j,i,l)                                                         !/*pl        181*/
      if (xmax.le.dp0) go to 160                                                !/*pl        182*/
      if (logy) then                                                            !/*pl        183*/
        call fix (base,y0,yy,nerg)                                              !/*pl        184*/
        if (base.le.dp0) go to 160                                              !/*pl        185*/
        if (logx) then                                                          !/*pl        186*/
          call hlogxy (xx(n1),yy(n1),n2,labx,laby2,title)                       !/*pl        187*/
        else                                                                    !/*pl        188*/
          call hxlogy (xx(n1),yy(n1),n2,labx,laby2,title)                       !/*pl        189*/
        endif                                                                   !/*pl        190*/
      else                                                                      !/*pl        191*/
        base=dp0                                                                !/*pl        192*/
        call fix1 (base,y0,yy,nerg)                                             !/*pl        193*/
        if (logx) then                                                          !/*pl        194*/
          call hylogx (xx(n1),yy(n1),n2,labx,laby2,title)                       !/*pl        195*/
        else                                                                    !/*pl        196*/
          call hxy (xx(n1),yy(n1),n2,labx,laby2,title)                          !/*pl        197*/
        endif                                                                   !/*pl        198*/
      endif                                                                     !/*pl        199*/
      do 140 i=1,nerg                                                           !/*pl        200*/
  140 y0(i)=(1.0+tlsq(j,i,l))*tlly(j,i,l)                                       !/*pl        201*/
      call fix1 (base,y0,yy,nerg)                                               !/*pl        202*/
      call hcvls ('sdash','off')                                                !/*pl        203*/
      call hcurve (xx(n1),yy(n1),n2)                                            !/*pl        204*/
      do 150 i=1,nerg                                                           !/*pl        205*/
  150 y0(i)=(1.0-tlsq(j,i,l))*tlly(j,i,l)                                       !/*pl        206*/
      call fix1 (base,y0,yy,nerg)                                               !/*pl        207*/
      call hcurve (xx(n1),yy(n1),n2)                                            !/*pl        208*/
      call hcvls ('solid','off')                                                !/*pl        209*/
      call gpage                                                                !/*pl        210*/
      nplot=nplot+1                                                             !/*pl        211*/
      write (io,190) nplot,icase,title                                          !/*pl        212*/
  160 continue                                                                  !/*pl        213*/
  170 continue                                                                  !/*pl        214*/
      return                                                                    !/*pl        215*/
!                                                                               !/*pl        216*/
  180 format (1h1,10x,'plot directory')                                         !/*pl        217*/
  190 format (' plot no.',i3,' case no.',i3,5x,a)                               !/*pl        218*/
#endif                                                                          /*!pl        219*/
      end                                                                       !/*pl        220*/

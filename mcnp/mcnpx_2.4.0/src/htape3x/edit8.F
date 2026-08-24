!     $Id: edit8.F,v 1.6 2002/03/27 18:13:58 gwm Exp $                        !/*ed          1*/
      subroutine edit8 (tlly,tlsq,nerg,nerg1,ntim,ntim1,k,fcas)                 !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/lpt.h>                                                        /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/big.h>                                                        /*!ed          6*/
#include <htape3x/big4.h>                                                       /*!ed          7*/
#include <htape3x/bndy.h>                                                       /*!ed          8*/
#include <htape3x/ctconv.h>                                                     /*!ed          9*/
      dimension tlly(ntim1,nerg1,1), tlsq(ntim1,nerg1,1)                        !/*ed         10*/
      dimension f(len2,3), s(len2,3)                                            !/*ed         11*/
#ifndef HTPLOT                                                                  /*!ed         12*/
      data lori /23/                                                            !/*ed         13*/
#endif                                                                          /*!ed         14*/
#ifdef HTPLOT                                                                   /*!ed         15*/
      character laby1*8, labx1*13, labx2*11, title1*20                          !/*ed         16*/
      character*10 title4                                                       !/*ed         17*/
      character title2*19, tmat*4, treg*4, title*47                             !/*ed         18*/
      character title3*20, labx3*14                                             !/*ed         19*/
      character bin*3, aev*10, bev*9, bfs*12, titlex*12                         !/*ed         20*/
      dimension xdp1(55), xdp2(36)                                              !/*ed         21*/
      data xdp1 /2.2494,3.6488,4.8594,6.2938,7.8173,9.4277,11.1349,12.93        !/*ed         22*/
     & 48,14.8185,16.7718,18.7919,20.8762,23.0238,25.2411,27.5181,29.852        !/*ed         23*/
     & 7,32.2373,34.6717,37.1647,39.7231,42.1798,44.5384,46.9254,49.3503        !/*ed         24*/
     & ,51.8047,54.5031,57.4051,60.3593,63.3549,66.4039,69.4959,72.6343,        !/*ed         25*/
     & 75.8241,79.0471,82.3312,85.6536,89.0145,92.4326,95.8914,99.3880,1        !/*ed         26*/
     & 02.9306,106.5240,110.1601,113.8366,117.5540,121.3151,125.1203,128        !/*ed         27*/
     & .9690,132.8626,136.7979,140.7774,144.7970,148.8596,152.9637,157.1        !/*ed         28*/
     & 125/                                                                     !/*ed         29*/
      data xdp2 /1.9629,7.3505,12.2843,17.0651,21.6254,26.1414,30.6695,3        !/*ed         30*/
     & 5.1408,39.5658,43.9733,48.4144,52.8421,57.2467,61.6370,66.0248,70        !/*ed         31*/
     & .4423,74.8544,79.2530,83.6439,88.0368,92.4558,96.8741,101.2820,10        !/*ed         32*/
     & 5.6851,110.0961,114.5270,118.9607,123.3807,127.8015,132.2370,136.        !/*ed         33*/
     & 6851,141.1308,145.5717,150.0134,154.4782,158.9451/                       !/*ed         34*/
      data aev /' post-evap'/, bev /' pre-evap'/, bfs /' pre-fission'/          !/*ed         35*/
      data laby1 /'fraction'/, labx1 /'charge number'/,                         !/*ed         36*/
     & labx2 /'mass number'/, title1 /'isotope distribution'/,                  !/*ed         37*/
     & title2 /'isobar distribution'/, tmat /' mat'/, treg /' reg'/,            !/*ed         38*/
     & title3 /'isotone distribution'/, labx3 /'neutron number'/                !/*ed         39*/
      data title4 /'mass range'/                                                !/*ed         40*/
      data lori /23/                                                            !/*ed         41*/
      titlex=aev                                                                !/*ed         42*/
      if (kopt.eq.1) titlex=bev                                                 !/*ed         43*/
      if (kopt.eq.2) titlex=bfs                                                 !/*ed         44*/
#endif                                                                          /*!ed         45*/
      if (k.le.1.and.kopt.eq.0) then                                            !/*ed         46*/
        call edit8a (fcas,f,s)                                                  !/*ed         47*/
      endif                                                                     !/*ed         48*/
      if (lbunch) then                                                          !/*ed         49*/
        if (jmat.eq.0) write (io,470) (lparm(i),i=1,nparm)                      !/*ed         50*/
        if (jmat.ne.0) write (io,460) (lparm(i),i=1,nparm)                      !/*ed         51*/
        if (ixout.gt.0) then                                                    !/*ed         52*/
          if (jmat.eq.0) write (lori,470) (lparm(i),i=1,nparm)                  !/*ed         53*/
          if (jmat.ne.0) write (lori,460) (lparm(i),i=1,nparm)                  !/*ed         54*/
        endif                                                                   !/*ed         55*/
      elseif (nparm.gt.0) then                                                  !/*ed         56*/
        if (jmat.eq.0) write (io,440) lparm(k)                                  !/*ed         57*/
        if (jmat.ne.0) write (io,430) lparm(k)                                  !/*ed         58*/
        if (ixout.gt.0) then                                                    !/*ed         59*/
          if (jmat.eq.0) write (lori,440) lparm(k)                              !/*ed         60*/
          if (jmat.ne.0) write (lori,430) lparm(k)                              !/*ed         61*/
        endif                                                                   !/*ed         62*/
      else                                                                      !/*ed         63*/
        write (io,310)                                                          !/*ed         64*/
        if (ixout.gt.0) write (lori,310)                                        !/*ed         65*/
      endif                                                                     !/*ed         66*/
      if (kopt.eq.1) write (io,410)                                             !/*ed         67*/
      if (kopt.eq.2) write (io,450)                                             !/*ed         68*/
      fac=1.0/dnparm(k)                                                         !/*ed         69*/
      if (lsnprm) write (io,480) dnparm(k)                                      !/*ed         70*/
      izmax=0                                                                   !/*ed         71*/
      inmax=0                                                                   !/*ed         72*/
      do 30 iz=1,nerg                                                           !/*ed         73*/
#ifdef HTPLOT                                                                   /*!ed         74*/
      y0(iz)=dp0                                                                !/*ed         75*/
#endif                                                                          /*!ed         76*/
      rr=tlly(ntim1,iz,k)                                                       !/*ed         77*/
      if (rr.eq.0.) go to 30                                                    !/*ed         78*/
      iflag=0                                                                   !/*ed         79*/
      izm1=iz-1                                                                 !/*ed         80*/
      izmax=max(izm1,izmax)                                                     !/*ed         81*/
      do 20 inn=1,ntim                                                          !/*ed         82*/
      r=tlly(inn,iz,k)                                                          !/*ed         83*/
      if (r.eq.0.) go to 20                                                     !/*ed         84*/
      inm1=inn-1                                                                !/*ed         85*/
      inmax=max(inm1,inmax)                                                     !/*ed         86*/
!                                                                               !/*ed         87*/
      if (iflag.ne.0) then                                                      !/*ed         88*/
        write (io,320) inm1,r*fac,tlsq(inn,iz,k)                                !/*ed         89*/
      else                                                                      !/*ed         90*/
        iflag=1                                                                 !/*ed         91*/
        write (io,330) izm1,inm1,r*fac,tlsq(inn,iz,k)                           !/*ed         92*/
      endif                                                                     !/*ed         93*/
      if (ixout.le.0) go to 20                                                  !/*ed         94*/
      iza=1000*izm1+(izm1+inm1)                                                 !/*ed         95*/
      kk=jptx(iza)                                                              !/*ed         96*/
      if (kk.le.0) then                                                         !/*ed         97*/
        write (lori,340) izm1,inm1,r*fac,tlsq(inn,iz,k)                         !/*ed         98*/
        go to 20                                                                !/*ed         99*/
      endif                                                                     !/*ed        100*/
      do 10 l=1,3                                                               !/*ed        101*/
      if (f(kk,l).gt.dp0) then                                                  !/*ed        102*/
        a=r*fac*f(kk,l)                                                         !/*ed        103*/
        b=sqrt(tlsq(inn,iz,k)**2+s(kk,l)**2)                                    !/*ed        104*/
        if (l.eq.1) then                                                        !/*ed        105*/
          write (lori,340) izm1,inm1,a,b                                        !/*ed        106*/
        else                                                                    !/*ed        107*/
          write (lori,350) izm1,inm1,l-1,a,b                                    !/*ed        108*/
        endif                                                                   !/*ed        109*/
      endif                                                                     !/*ed        110*/
   10 continue                                                                  !/*ed        111*/
!                                                                               !/*ed        112*/
   20 continue                                                                  !/*ed        113*/
      write (io,360) rr*fac,tlsq(ntim1,iz,k)                                    !/*ed        114*/
#ifdef HTPLOT                                                                   /*!ed        115*/
      y0(iz)=rr*fac                                                             !/*ed        116*/
#endif                                                                          /*!ed        117*/
   30 continue                                                                  !/*ed        118*/
#ifdef HTPLOT                                                                   /*!ed        119*/
      if (kplot.eq.0) go to 90                                                  !/*ed        120*/
      if (lbunch.or.nparm.eq.0) then                                            !/*ed        121*/
        title=title1//titlex                                                    !/*ed        122*/
      else                                                                      !/*ed        123*/
        write (bin,'(i3)') lparm(k)                                             !/*ed        124*/
        if (jmat.ne.0.and.nparm.gt.0) title=title1//titlex//tmat//bin           !/*ed        125*/
        if (jmat.eq.0.and.nparm.gt.0) title=title1//titlex//treg//bin           !/*ed        126*/
      endif                                                                     !/*ed        127*/
      do 40 i=1,nerg                                                            !/*ed        128*/
      nup=nerg+1-i                                                              !/*ed        129*/
      if (y0(nup).gt.dp0) go to 50                                              !/*ed        130*/
   40 continue                                                                  !/*ed        131*/
   50 continue                                                                  !/*ed        132*/
      do 60 i=1,262                                                             !/*ed        133*/
      j=2*i-1                                                                   !/*ed        134*/
      xx(j)=real(i)-1.5                                                         !/*ed        135*/
   60 xx(j+1)=xx(j)                                                             !/*ed        136*/
      nup=min(nup+1,nerg)                                                       !/*ed        137*/
      do 70 i=1,nerg                                                            !/*ed        138*/
      nlo=i                                                                     !/*ed        139*/
      if (y0(i).gt.dp0) go to 80                                                !/*ed        140*/
   70 continue                                                                  !/*ed        141*/
   80 continue                                                                  !/*ed        142*/
      n1=2*nlo-1                                                                !/*ed        143*/
      n2=2*nup-n1+1                                                             !/*ed        144*/
      call fix (base,y0,yy,nup)                                                 !/*ed        145*/
      call hxlogy (xx(n1),yy(n1),n2,labx1,laby1,title)                          !/*ed        146*/
      call gpage                                                                !/*ed        147*/
      nplot=nplot+1                                                             !/*ed        148*/
      write (io,300) nplot,icase,title                                          !/*ed        149*/
   90 continue                                                                  !/*ed        150*/
#endif                                                                          /*!ed        151*/
      iflag=0                                                                   !/*ed        152*/
      do 110 inn=1,ntim                                                         !/*ed        153*/
#ifdef HTPLOT                                                                   /*!ed        154*/
      y0(inn)=dp0                                                               !/*ed        155*/
#endif                                                                          /*!ed        156*/
      r=tlly(inn,nerg1,k)                                                       !/*ed        157*/
      if (r.eq.0.) go to 110                                                    !/*ed        158*/
      inm1=inn-1                                                                !/*ed        159*/
#ifdef HTPLOT                                                                   /*!ed        160*/
      y0(inn)=r*fac                                                             !/*ed        161*/
#endif                                                                          /*!ed        162*/
      if (iflag.eq.0) go to 100                                                 !/*ed        163*/
      write (io,320) inm1,r*fac,tlsq(inn,nerg1,k)                               !/*ed        164*/
      go to 110                                                                 !/*ed        165*/
  100 continue                                                                  !/*ed        166*/
      iflag=1                                                                   !/*ed        167*/
      write (io,370) inm1,r*fac,tlsq(inn,nerg1,k)                               !/*ed        168*/
  110 continue                                                                  !/*ed        169*/
#ifdef HTPLOT                                                                   /*!ed        170*/
      if (kplot.eq.0) go to 160                                                 !/*ed        171*/
      if (lbunch.or.nparm.eq.0) then                                            !/*ed        172*/
        title=title3//titlex                                                    !/*ed        173*/
      else                                                                      !/*ed        174*/
        if (jmat.ne.0.and.nparm.gt.0) title=title3//titlex//tmat//bin           !/*ed        175*/
        if (jmat.eq.0.and.nparm.gt.0) title=title3//titlex//treg//bin           !/*ed        176*/
      endif                                                                     !/*ed        177*/
      do 120 i=1,ntim                                                           !/*ed        178*/
      nup=ntim+1-i                                                              !/*ed        179*/
      if (y0(nup).gt.dp0) go to 130                                             !/*ed        180*/
  120 continue                                                                  !/*ed        181*/
  130 continue                                                                  !/*ed        182*/
      nup=min(nup+1,ntim)                                                       !/*ed        183*/
      do 140 i=1,ntim                                                           !/*ed        184*/
      nlo=i                                                                     !/*ed        185*/
      if (y0(i).gt.dp0) go to 150                                               !/*ed        186*/
  140 continue                                                                  !/*ed        187*/
  150 continue                                                                  !/*ed        188*/
      n1=2*nlo-1                                                                !/*ed        189*/
      n2=2*nup-n1+1                                                             !/*ed        190*/
      call fix (base,y0,yy,nup)                                                 !/*ed        191*/
      call hxlogy (xx(n1),yy(n1),n2,labx3,laby1,title)                          !/*ed        192*/
      call gpage                                                                !/*ed        193*/
      nplot=nplot+1                                                             !/*ed        194*/
      write (io,300) nplot,icase,title                                          !/*ed        195*/
  160 continue                                                                  !/*ed        196*/
#endif                                                                          /*!ed        197*/
      write (io,390)                                                            !/*ed        198*/
      iam=ntim+nerg-2                                                           !/*ed        199*/
      do 180 ia=1,iam                                                           !/*ed        200*/
#ifdef HTPLOT                                                                   /*!ed        201*/
      if (ia.le.260) y0(ia)=0                                                   !/*ed        202*/
#endif                                                                          /*!ed        203*/
      tl=dp0                                                                    !/*ed        204*/
      ts=dp0                                                                    !/*ed        205*/
      iz1=max(1,ia-ntim+2)                                                      !/*ed        206*/
      iz2=min(ia+1,nerg)                                                        !/*ed        207*/
      do 170 iz=iz1,iz2                                                         !/*ed        208*/
      inn=ia-iz+2                                                               !/*ed        209*/
      a=tlly(inn,iz,k)                                                          !/*ed        210*/
      if (a.le.0.) go to 170                                                    !/*ed        211*/
      tl=tl+a                                                                   !/*ed        212*/
      b=a*tlsq(inn,iz,k)                                                        !/*ed        213*/
      ts=ts+b*b                                                                 !/*ed        214*/
  170 continue                                                                  !/*ed        215*/
      if (tl.eq.0.) go to 180                                                   !/*ed        216*/
      tss=sqrt(ts)/tl                                                           !/*ed        217*/
      write (io,400) ia,fac*tl,tss                                              !/*ed        218*/
#ifdef HTPLOT                                                                   /*!ed        219*/
      y0(ia)=fac*tl                                                             !/*ed        220*/
#endif                                                                          /*!ed        221*/
  180 continue                                                                  !/*ed        222*/
#ifdef HTPLOT                                                                   /*!ed        223*/
      if (kplot.eq.0) go to 290                                                 !/*ed        224*/
      if (lbunch.or.nparm.eq.0) then                                            !/*ed        225*/
        title=title2//titlex                                                    !/*ed        226*/
      else                                                                      !/*ed        227*/
        if (jmat.ne.0.and.nparm.gt.0) title=title2//titlex//tmat//bin           !/*ed        228*/
        if (jmat.eq.0.and.nparm.gt.0) title=title2//titlex//treg//bin           !/*ed        229*/
      endif                                                                     !/*ed        230*/
      nup0=min(260,iam)                                                         !/*ed        231*/
      do 190 i=1,nup0                                                           !/*ed        232*/
      nup=nup0+1-i                                                              !/*ed        233*/
      if (y0(nup).gt.dp0) go to 200                                             !/*ed        234*/
  190 continue                                                                  !/*ed        235*/
  200 continue                                                                  !/*ed        236*/
      nup=min(nup+1,nup0)                                                       !/*ed        237*/
      do 210 i=1,nup0                                                           !/*ed        238*/
      nlo=i                                                                     !/*ed        239*/
      if (y0(i).gt.dp0) go to 220                                               !/*ed        240*/
  210 continue                                                                  !/*ed        241*/
  220 continue                                                                  !/*ed        242*/
      n1=2*nlo-1                                                                !/*ed        243*/
      n2=2*nup-n1+1                                                             !/*ed        244*/
      call fix (base,y0,yy,nup)                                                 !/*ed        245*/
      call hxlogy (xx(n1+2),yy(n1),n2,labx2,laby1,title)                        !/*ed        246*/
      call gpage                                                                !/*ed        247*/
      nplot=nplot+1                                                             !/*ed        248*/
      write (io,300) nplot,icase,title                                          !/*ed        249*/
      if (lbunch.or.nparm.eq.0) then                                            !/*ed        250*/
        title=title4//titlex                                                    !/*ed        251*/
      else                                                                      !/*ed        252*/
        write (bin,'(i3)') lparm(k)                                             !/*ed        253*/
        if (jmat.ne.0.and.nparm.gt.0) title=title4//titlex//tmat//bin           !/*ed        254*/
        if (jmat.eq.0.and.nparm.gt.0) title=title4//titlex//treg//bin           !/*ed        255*/
      endif                                                                     !/*ed        256*/
      call hcvls ('noline','off')                                               !/*ed        257*/
      xx(1)=0.                                                                  !/*ed        258*/
      xx(2)=min(160,inmax+5)                                                    !/*ed        259*/
      xxmax=xx(2)                                                               !/*ed        260*/
      yy(1)=0.                                                                  !/*ed        261*/
      yy(2)=min(100,izmax+5)                                                    !/*ed        262*/
      yymax=yy(2)                                                               !/*ed        263*/
      call hxy (xx(1),yy(1),2,'N','Z',title4)                                   !/*ed        264*/
      call hcvls ('solid','off')                                                !/*ed        265*/
      do 230 i=1,55                                                             !/*ed        266*/
      yy(i)=2*i+1                                                               !/*ed        267*/
      xx(i)=xdp1(i)                                                             !/*ed        268*/
      if (yy(i).gt.yymax.or.xx(1).gt.xxmax) go to 240                           !/*ed        269*/
      nn=i                                                                      !/*ed        270*/
  230 continue                                                                  !/*ed        271*/
  240 continue                                                                  !/*ed        272*/
      call hcurve (xx(1),yy(1),nn)                                              !/*ed        273*/
      do 250 i=1,36                                                             !/*ed        274*/
      yy(i)=2*i                                                                 !/*ed        275*/
      xx(i)=xdp2(i)                                                             !/*ed        276*/
      if (yy(i).gt.yymax.or.xx(1).gt.xxmax) go to 260                           !/*ed        277*/
      nn=i                                                                      !/*ed        278*/
  250 continue                                                                  !/*ed        279*/
  260 continue                                                                  !/*ed        280*/
      call hcurve (xx(1),yy(1),nn)                                              !/*ed        281*/
      do 280 iz=1,nerg                                                          !/*ed        282*/
      rr=tlly(ntim1,iz,k)                                                       !/*ed        283*/
      if (rr.eq.0.) go to 280                                                   !/*ed        284*/
      nn=0                                                                      !/*ed        285*/
      izm1=iz-1                                                                 !/*ed        286*/
      do 270 inn=1,ntim                                                         !/*ed        287*/
      r=tlly(inn,iz,k)                                                          !/*ed        288*/
      if (r.eq.0.) go to 270                                                    !/*ed        289*/
      inm1=inn-1                                                                !/*ed        290*/
      nn=nn+1                                                                   !/*ed        291*/
      xx(nn)=inm1                                                               !/*ed        292*/
      yy(nn)=izm1                                                               !/*ed        293*/
  270 continue                                                                  !/*ed        294*/
      if (nn.eq.0) go to 280                                                    !/*ed        295*/
      call hcvls ('noline','off')                                               !/*ed        296*/
      call hcvmrk ('plus',nn,'off')                                             !/*ed        297*/
      call hcurve (xx(1),yy(1),nn)                                              !/*ed        298*/
      call hcvls ('default','off')                                              !/*ed        299*/
      call hcvmrk ('default',nn,'off')                                          !/*ed        300*/
  280 continue                                                                  !/*ed        301*/
      call gpage                                                                !/*ed        302*/
  290 continue                                                                  !/*ed        303*/
#endif                                                                          /*!ed        304*/
      write (io,380) fac*tlly(ntim1,nerg1,k),tlsq(ntim1,nerg1,k)                !/*ed        305*/
      write (io,420) lowaz,ihiaz                                                !/*ed        306*/
      if (lbunch.or.kopt.eq.0.and.k.ge.nparm) then                              !/*ed        307*/
        call edit8b (fcas,f,s)                                                  !/*ed        308*/
      endif                                                                     !/*ed        309*/
      return                                                                    !/*ed        310*/
!                                                                               !/*ed        311*/
  300 format ('0plot no.',i3,' case no.',i3,5x,a)                               !/*ed        312*/
  310 format (21x,'distribution of residual nuclei')                            !/*ed        313*/
  320 format (13x,'n =',i4,1pd14.5,0pf7.4)                                      !/*ed        314*/
  330 format (/5x,'z =',i3,'  n =',i4,1pd14.5,0pf7.4)                           !/*ed        315*/
  340 format (5x,'z =',i3,'  n =',i4,1pd14.5,0pf7.4)                            !/*ed        316*/
  350 format (5x,'z =',i3,'  n =',i4,i2,1pd12.5,0pf7.4)                         !/*ed        317*/
  360 format (13x,'all n  ',1pd14.5,0pf7.4)                                     !/*ed        318*/
  370 format (/5x,'all z   n =',i4,1pd14.5,0pf7.4)                              !/*ed        319*/
  380 format (/5x,'mean weight of residual nuclei per cascade',1pd14.5,0        !/*ed        320*/
     & pf7.4)                                                                   !/*ed        321*/
  390 format (/5x,'summary by mass number')                                     !/*ed        322*/
  400 format (13x,'a =',i4,1pd14.5,0pf7.4)                                      !/*ed        323*/
  410 format (/26x,'(before evaporation)')                                      !/*ed        324*/
  420 format (/5x,'low a/z =',i5,'    high a/z =',i5)                           !/*ed        325*/
  430 format (21x,'distribution of residual nuclei in material',i4)             !/*ed        326*/
  440 format (21x,'distribution of residual nuclei in cell',i4)                 !/*ed        327*/
  450 format (/26x,'(before fission)')                                          !/*ed        328*/
  460 format (10x,'distribution of residual nuclei in materials',13i4/(5        !/*ed        329*/
     & 4x,13i4))                                                                !/*ed        330*/
  470 format (10x,'distribution of residual nuclei in cells',13i4/(50x,1        !/*ed        331*/
     & 3i4))                                                                    !/*ed        332*/
  480 format (/35x,'tally divided by',1pd14.5)                                  !/*ed        333*/
      end                                                                       !/*ed        334*/

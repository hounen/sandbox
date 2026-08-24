!     $Id: edit.F,v 1.4 2002/03/27 18:13:57 gwm Exp $                         !/*ed          1*/
      subroutine edit (tlly,tlsq,cov,tlfsq,nerg,nerg1,ntim,ntim1)               !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/srcwt.h>                                                      /*!ed          4*/
      character time*8                                                          !/*ed          5*/
#include <htape3x/label.h>                                                      /*!ed          6*/
#include <htape3x/contrl.h>                                                     /*!ed          7*/
#include <htape3x/bndy.h>                                                       /*!ed          8*/
#include <htape3x/comm7.h>                                                      /*!ed          9*/
#include <htape3x/ctconv.h>                                                     /*!ed         10*/
#include <htape3x/charht.h>                                                     /*!ed         11*/
      dimension tlly(ntim1,nerg1,1), tlsq(ntim1,nerg1,1)                        !/*ed         12*/
      dimension cov(ntim1,nerg1,1)                                              !/*ed         13*/
      dimension tlfsq(ntim1,nerg1,1)                                            !/*ed         14*/
#include <htape3x/kpt.h>                                                        /*!ed         15*/
      data time /'time='/                                                       !/*ed         16*/
      ncas=maxbch*maxcas                                                        !/*ed         17*/
      fcas=ncas                                                                 !/*ed         18*/
      cnorm=1.0/fcas                                                            !/*ed         19*/
      if (irs.gt.0.and.fwtcum.gt.dp0) cnorm=1.0/fwtcum                          !/*ed         20*/
      dnorm=dp0                                                                 !/*ed         21*/
      if (ncas.gt.1) dnorm=1.0/(fcas-1.0)                                       !/*ed         22*/
      enorm=dnorm*fcas                                                          !/*ed         23*/
      if (fwton) then                                                           !/*ed         24*/
        fwta=fwtcum/fcas                                                        !/*ed         25*/
        fwtb=sqrt(max(dnorm*(fwscum/fcas-fwta**2),dp0))                         !/*ed         26*/
        write (io,450) fwta,fwtb                                                !/*ed         27*/
        write (jtty,450) fwta,fwtb                                              !/*ed         28*/
      endif                                                                     !/*ed         29*/
      if (irs.le.0) then                                                        !/*ed         30*/
        fwscum=fcas                                                             !/*ed         31*/
        fwtcum=fcas                                                             !/*ed         32*/
      endif                                                                     !/*ed         33*/
      nup=nbin*ntype                                                            !/*ed         34*/
      do 10 k=1,nup                                                             !/*ed         35*/
      do 10 j=1,nerg1                                                           !/*ed         36*/
      do 10 i=1,ntim1                                                           !/*ed         37*/
!REP                03/30/95                                                    !/*ed         38*/
      b=tlsq(i,j,k)                                                             !/*ed         39*/
      c=tlly(i,j,k)                                                             !/*ed         40*/
      d=tlfsq(i,j,k)                                                            !/*ed         41*/
!     if (iopt.eq.3.and.c.le.dp0) go to 10                                      !/*ed         42*/
      if (iopt.eq.7) then                                                       !/*ed         43*/
        b=enorm*(b+fwscum*(cnorm*c)**2-dp2*d*cnorm*c)                           !/*ed         44*/
        if (b.lt.dp0) b=dp0                                                     !/*ed         45*/
        b=cnorm*fnorm*sqrt(b)                                                   !/*ed         46*/
        c=cnorm*fnorm*c                                                         !/*ed         47*/
      else                                                                      !/*ed         48*/
        if (b.gt.dp0) then                                                      !/*ed         49*/
!         b=enorm*(b/c**2+fwscum*cnorm**2-dp2*d*cnorm/c)                        !/*ed         50*/
!         b=sqrt(abs(b))                                                        !/*ed         51*/
          if (c.ne.dp0) then                                                    !/*ed         52*/
            b=enorm*(b/c**2+fwscum*cnorm**2-dp2*d*cnorm/c)                      !/*ed         53*/
            b=sqrt(abs(b))                                                      !/*ed         54*/
          else                                                                  !/*ed         55*/
            b=-10.0                                                             !/*ed         56*/
          endif                                                                 !/*ed         57*/
        endif                                                                   !/*ed         58*/
        if (iopt.eq.11.or.iopt.eq.12) then                                      !/*ed         59*/
          c=cnorm*c                                                             !/*ed         60*/
        else                                                                    !/*ed         61*/
          c=cnorm*fnorm*c                                                       !/*ed         62*/
        endif                                                                   !/*ed         63*/
      endif                                                                     !/*ed         64*/
      tlsq(i,j,k)=b                                                             !/*ed         65*/
      tlly(i,j,k)=c                                                             !/*ed         66*/
!REP                03/30/95                                                    !/*ed         67*/
   10 continue                                                                  !/*ed         68*/
      if (iopt.eq.5) call pcov5 (tlly,tlfsq,cov,nerg1,nup,enorm,cnorm)          !/*ed         69*/
      if (iopt.eq.11.or.iopt.eq.12) call pcov11 (tlly,cov,tlfsq,nerg1           !/*ed         70*/
     & ,nup,enorm,cnorm)                                                        !/*ed         71*/
      if (iopt.eq.3) go to 270                                                  !/*ed         72*/
      if (iopt.eq.14) go to 280                                                 !/*ed         73*/
      do 260 l=1,ntype                                                          !/*ed         74*/
      if (iopt.ne.13) go to 20                                                  !/*ed         75*/
      write (io,320) icase,iopt,titl1,titl2                                     !/*ed         76*/
      call edit13 (tlly,tlsq,nerg,nerg1,ntim,ntim1,l)                           !/*ed         77*/
      go to 260                                                                 !/*ed         78*/
   20 continue                                                                  !/*ed         79*/
      if (iopt.le.2.or.(iopt.ge.9.and.iopt.le.12)) then                         !/*ed         80*/
        if (lbunch) nop=1                                                       !/*ed         81*/
        if (.not.lbunch) nop=max(nparm,1)                                       !/*ed         82*/
      else                                                                      !/*ed         83*/
        nop=nbin                                                                !/*ed         84*/
      endif                                                                     !/*ed         85*/
      lori=23                                                                   !/*ed         86*/
      if (ixout.gt.0.and.iopt.eq.8) call uopen (jtty,lori,opt8a,'f')            !/*ed         87*/
      if (ixout.gt.0.and.iopt.eq.15) call uopen (jtty,lori,opt15a,'f')          !/*ed         88*/
      do 250 k=1,nop                                                            !/*ed         89*/
      write (io,320) icase,iopt,titl1,titl2                                     !/*ed         90*/
      if (iopt.eq.15) go to 125                                                 !/*ed         91*/
      if (iopt.eq.16) go to 127                                                 !/*ed         92*/
      if (iopt.gt.12) go to 170                                                 !/*ed         93*/
      go to (30,40,50,80,90,100,110,120,130,140,150,160), iopt                  !/*ed         94*/
   30 continue                                                                  !/*ed         95*/
      call edit1 (tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                          !/*ed         96*/
      go to 250                                                                 !/*ed         97*/
   40 continue                                                                  !/*ed         98*/
      call edit2 (tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                          !/*ed         99*/
      go to 250                                                                 !/*ed        100*/
   50 continue                                                                  !/*ed        101*/
      fac=1.0/dnparm(k)                                                         !/*ed        102*/
      if (l.ge.11) go to 70                                                     !/*ed        103*/
      if (l.ge.6) go to 60                                                      !/*ed        104*/
      if (kopt.ge.2) go to 250                                                  !/*ed        105*/
      if (lbunch) then                                                          !/*ed        106*/
        if (jmat.eq.0) write (io,480) kpart(l),(lparm(i),i=1,nparm)             !/*ed        107*/
        if (jmat.eq.1) write (io,490) kpart(l),(lparm(i),i=1,nparm)             !/*ed        108*/
      elseif (k.lt.nbin) then                                                   !/*ed        109*/
        if (jmat.eq.0) write (io,390) kpart(l),lparm(k)                         !/*ed        110*/
        if (jmat.eq.1) write (io,400) kpart(l),lparm(k)                         !/*ed        111*/
      else                                                                      !/*ed        112*/
        write (io,410) kpart(l)                                                 !/*ed        113*/
      endif                                                                     !/*ed        114*/
      go to 180                                                                 !/*ed        115*/
   60 continue                                                                  !/*ed        116*/
      if (kopt.eq.1.or.kopt.eq.3) go to 250                                     !/*ed        117*/
      m=l-5                                                                     !/*ed        118*/
      if (lbunch) then                                                          !/*ed        119*/
        if (jmat.eq.0) write (io,460) kpart(m),(lparm(i),i=1,nparm)             !/*ed        120*/
        if (jmat.eq.1) write (io,470) kpart(m),(lparm(i),i=1,nparm)             !/*ed        121*/
      elseif (k.lt.nbin) then                                                   !/*ed        122*/
        if (jmat.eq.0) write (io,360) kpart(m),lparm(k)                         !/*ed        123*/
        if (jmat.eq.1) write (io,380) kpart(m),lparm(k)                         !/*ed        124*/
      else                                                                      !/*ed        125*/
        write (io,370) kpart(m)                                                 !/*ed        126*/
      endif                                                                     !/*ed        127*/
      go to 180                                                                 !/*ed        128*/
   70 continue                                                                  !/*ed        129*/
      if (kopt.eq.1.or.kopt.eq.2) go to 250                                     !/*ed        130*/
      m=l-11                                                                    !/*ed        131*/
      if (lbunch) then                                                          !/*ed        132*/
        if (jmat.eq.0) write (io,500) kpart(m),(lparm(i),i=1,nparm)             !/*ed        133*/
        if (jmat.eq.1) write (io,510) kpart(m),(lparm(i),i=1,nparm)             !/*ed        134*/
      elseif (k.lt.nbin) then                                                   !/*ed        135*/
        if (jmat.eq.0) write (io,420) kpart(m),lparm(k)                         !/*ed        136*/
        if (jmat.eq.1) write (io,430) kpart(m),lparm(k)                         !/*ed        137*/
      else                                                                      !/*ed        138*/
        write (io,440) kpart(m)                                                 !/*ed        139*/
      endif                                                                     !/*ed        140*/
      go to 180                                                                 !/*ed        141*/
   80 continue                                                                  !/*ed        142*/
! track length neutron flux                                                     !/*ed        143*/
      call edit4 (tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                          !/*ed        144*/
      go to 250                                                                 !/*ed        145*/
   90 continue                                                                  !/*ed        146*/
! option 5 - residual masses                                                    !/*ed        147*/
      call edit5 (tlly,tlsq,cov,nerg,nerg1,k)                                   !/*ed        148*/
      go to 250                                                                 !/*ed        149*/
  100 continue                                                                  !/*ed        150*/
      call edit6 (tlly,tlsq,ntim,ntim1,k)                                       !/*ed        151*/
      go to 250                                                                 !/*ed        152*/
  110 continue                                                                  !/*ed        153*/
      call edit7 (tlly,tlsq,ntim,ntim1,k)                                       !/*ed        154*/
      if (k.eq.nop) write (io,530) balmax,nocasm                                !/*ed        155*/
      go to 250                                                                 !/*ed        156*/
  120 continue                                                                  !/*ed        157*/
! distribution of residual nuclei                                               !/*ed        158*/
      call edit8 (tlly,tlsq,nerg,nerg1,ntim,ntim1,k,fcas)                       !/*ed        159*/
      go to 250                                                                 !/*ed        160*/
  125 continue                                                                  !/*ed        161*/
! option 15 - isotopic collision rates                                          !/*ed        162*/
      call edit15 (tlly,tlsq,nerg,nerg1,ntim,ntim1,k)                           !/*ed        163*/
      go to 250                                                                 !/*ed        164*/
  127 continue                                                                  !/*ed        165*/
! option 16 - recoil energy spectra                                             !/*ed        166*/
      call edit16 (tlly,tlsq,nerg,nerg1,ntim,k)                                 !/*ed        167*/
      go to 250                                                                 !/*ed        168*/
  130 continue                                                                  !/*ed        169*/
      call edit9 (tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                          !/*ed        170*/
      go to 250                                                                 !/*ed        171*/
  140 continue                                                                  !/*ed        172*/
      call edit10 (tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                         !/*ed        173*/
      go to 250                                                                 !/*ed        174*/
  150 continue                                                                  !/*ed        175*/
! pulse shape analysis                                                          !/*ed        176*/
      call edit11 (tlly,tlsq,cov,nerg,nerg1,l,k)                                !/*ed        177*/
      go to 250                                                                 !/*ed        178*/
  160 continue                                                                  !/*ed        179*/
! pulse shape analysis with window                                              !/*ed        180*/
      call edit12 (tlly,tlsq,cov,nerg,nerg1,l,k)                                !/*ed        181*/
      go to 250                                                                 !/*ed        182*/
  170 continue                                                                  !/*ed        183*/
      fac=1.0                                                                   !/*ed        184*/
      if (ntyp1.gt.0) write (io,330) itip(l),k                                  !/*ed        185*/
      if (ntyp1.le.0) write (io,340) k                                          !/*ed        186*/
  180 continue                                                                  !/*ed        187*/
      if (lsnprm.and.iopt.eq.3) write (io,520) dnparm(k)                        !/*ed        188*/
      m=k+nbin*(l-1)                                                            !/*ed        189*/
      if (ntim.le.1) go to 220                                                  !/*ed        190*/
      ilo=1                                                                     !/*ed        191*/
  190 continue                                                                  !/*ed        192*/
      ihi=min(ilo+4,ntim)                                                       !/*ed        193*/
      write (io,310) (time,timb(i),i=ilo,ihi)                                   !/*ed        194*/
      nup=ihi-ilo+1                                                             !/*ed        195*/
      if (nerg.le.1) go to 210                                                  !/*ed        196*/
      do 200 j=1,nerg                                                           !/*ed        197*/
      fac1=fac*dnerg(j)                                                         !/*ed        198*/
      write (io,290) ergb(j),(fac1*dntim(i)*tlly(i,j,m),tlsq(i,j,m),i           !/*ed        199*/
     & =ilo,ihi)                                                                !/*ed        200*/
  200 continue                                                                  !/*ed        201*/
  210 continue                                                                  !/*ed        202*/
      j=nerg1                                                                   !/*ed        203*/
      write (io,300) (fac*dntim(i)*tlly(i,j,m),tlsq(i,j,m),i=ilo,ihi)           !/*ed        204*/
      if (ihi.ge.ntim) go to 220                                                !/*ed        205*/
      ilo=ilo+5                                                                 !/*ed        206*/
      go to 190                                                                 !/*ed        207*/
  220 continue                                                                  !/*ed        208*/
      write (io,350)                                                            !/*ed        209*/
      i=ntim1                                                                   !/*ed        210*/
      if (nerg.le.1) go to 240                                                  !/*ed        211*/
      do 230 j=1,nerg                                                           !/*ed        212*/
      write (io,290) ergb(j),fac*tlly(i,j,m),tlsq(i,j,m)                        !/*ed        213*/
  230 continue                                                                  !/*ed        214*/
  240 continue                                                                  !/*ed        215*/
      j=nerg1                                                                   !/*ed        216*/
      val1=tlly(i,j,m)                                                          !/*ed        217*/
      sd1=tlsq(i,j,m)                                                           !/*ed        218*/
      write (io,300) fac*val1,sd1                                               !/*ed        219*/
  250 continue                                                                  !/*ed        220*/
  260 continue                                                                  !/*ed        221*/
#ifndef ORIHET                                                                  /*!ed        222*/
      return                                                                    !/*ed        223*/
#endif                                                                          /*!ed        224*/
  270 continue                                                                  !/*ed        225*/
      call edit3 (tlly,tlsq,nerg1,nbin)                                         !/*ed        226*/
      return                                                                    !/*ed        227*/
  280 call edit14 (tlly,tlsq)                                                   !/*ed        228*/
      return                                                                    !/*ed        229*/
!                                                                               !/*ed        230*/
  290 format (1pd12.4,5(1pd14.5,0pf7.4))                                        !/*ed        231*/
  300 format (7x,'total',5(1pd14.5,0pf7.4))                                     !/*ed        232*/
  310 format (/6x,'energy',5(3x,a6,1pd12.3))                                    !/*ed        233*/
  320 format ('1',20x,'case no.',i3,5x,'option no.',i3//(10x,10a8))             !/*ed        234*/
  330 format (/21x,'particle type',i3,5x,'bin no.',i4)                          !/*ed        235*/
  340 format (/21x,'bin no.',i4)                                                !/*ed        236*/
  350 format (/6x,'energy',6x,'time total')                                     !/*ed        237*/
  360 format (/21x,a8,' evaporation in cell',i4)                                !/*ed        238*/
  370 format (/21x,'total ',a8,' evaporation')                                  !/*ed        239*/
  380 format (/21x,a8,' evaporation in material',i4)                            !/*ed        240*/
  390 format (/21x,a8,' from cascade in cell',i4)                               !/*ed        241*/
  400 format (/21x,a8,' from cascade in material',i4)                           !/*ed        242*/
  410 format (/21x,'total ',a8,' from cascade')                                 !/*ed        243*/
  420 format (/21x,a8,' total production in cell',i4)                           !/*ed        244*/
  430 format (/21x,a8,' total production in material',i4)                       !/*ed        245*/
  440 format (/21x,'total ',a8,' total production')                             !/*ed        246*/
  450 format (///' mean source weight adjustment =',d13.5,'  +/-',d13.5)        !/*ed        247*/
  460 format (/10x,a8,' evaporation in cells         ',15i4/(48x,15i4))         !/*ed        248*/
  470 format (/10x,a8,' evaporation in materials     ',15i4/(48x,15i4))         !/*ed        249*/
  480 format (/10x,a8,' from cascade in cells        ',15i4/(48x,15i4))         !/*ed        250*/
  490 format (/10x,a8,' from cascade in materials    ',15i4/(48x,15i4))         !/*ed        251*/
  500 format (/10x,a8,' total production in cells    ',15i4/(48x,15i4))         !/*ed        252*/
  510 format (/10x,a8,' total production in materials',15i4/(48x,15i4))         !/*ed        253*/
  520 format (/35x,'tally divided by',1pd14.5)                                  !/*ed        254*/
  530 format (/' maximum mass-energy imbalance',1pd12.3,' in cascade',i9        !/*ed        255*/
     & )                                                                        !/*ed        256*/
      end                                                                       !/*ed        257*/

!     $Id: islink.F,v 1.5 2002/06/14 17:05:42 jxh Exp $                         !/*is          1*/
      subroutine islink (bb,csa,ixa,ixq,kcan,lkcan,mb,nprts,voc,xmasss          !/*is          2*/
     & ,ex,expr,pit,pir)                                                        !/*is          3*/
! ISLINK calls   SQRT  ENERGY  BOOST  MOMENT  IGETD  ERRORF                     !/*is          4*/
! called by ISEDT                                                               !/*is          5*/
!     ==================================================================        !/*is          6*/
!   kind(hetc)                aa(isabel)                                        !/*is          7*/
!                                                                               !/*is          8*/
!   proton    0                   1.0                                           !/*is          9*/
!   neutron   1                   2.0                                           !/*is         10*/
!   pi+       2                   3.0                                           !/*is         11*/
!   pi0       3                   4.0                                           !/*is         12*/
!   pi-       4                   5.0                                           !/*is         13*/
!   mu+       5                    -                                            !/*is         14*/
!   mu-       6                    -                                            !/*is         15*/
!   d         7                    -                                            !/*is         16*/
!   t         8                    -                                            !/*is         17*/
!   he-3      9                    -                                            !/*is         18*/
!   alpha    10                    -                                            !/*is         19*/
!   photon   11                    -                                            !/*is         20*/
!   k+       12                   7.0                                           !/*is         21*/
!   k0-long  13                 (k0,k0=8.0)                                     !/*is         22*/
!   k0-short 14                 (k0,k0=8.0)                                     !/*is         23*/
!   k-       15                   9.0                                           !/*is         24*/
!   p-bar    16                  -1.0                                           !/*is         25*/
!   n-bar    17                  -2.0                                           !/*is         26*/
!                                                                               !/*is         27*/
!     ==================================================================        !/*is         28*/
#include <lcs/double.h>                                                         /*!is         29*/
      character lkcan*8                                                         !/*is         30*/
#ifndef OLDM                                                                    /*!is         31*/
#include <lcs/phprm0.h>                                                         /*!is         32*/
#endif                                                                          /*!is         33*/
#include <lcs/inout.h>                                                          /*!is         34*/
#include <lcs/banka.h>                                                          /*!is         35*/
      dimension exi(20), vp(3,20), erc(20), jjp(20)                             !/*is         36*/
      dimension xmasss(3,3), bb(3600), csa(15), pit(3), pir(6), ixa(2),         !/*is         37*/
     & ixq(2)                                                                   !/*is         38*/
      dimension vec0(3)                                                         !/*is         39*/
      data vec0 /dp0,dp0,dp1/                                                   !/*is         40*/
#ifdef OLDM                                                                     /*!is         41*/
      data elmass, amumev /0.511d0,931.20793d0/                                 !/*is         42*/
#endif                                                                          /*!is         43*/
      nnuc=1                                                                    !/*is         44*/
      noprts=nprts                                                              !/*is         45*/
      if (noprts.lt.0) return                                                   !/*is         46*/
      if (mb.eq.0) go to 20                                                     !/*is         47*/
      do 10 i=1,mb                                                              !/*is         48*/
      noprts=noprts+1                                                           !/*is         49*/
      k=9*(i-1)                                                                 !/*is         50*/
      kbb=bb(k+1)                                                               !/*is         51*/
      if (kbb.lt.0) then                                                        !/*is         52*/
        kbb=16-kbb                                                              !/*is         53*/
      elseif (kbb.eq.7.or.kbb.eq.8) then                                        !/*is         54*/
        kbb=kbb+6                                                               !/*is         55*/
      elseif (kbb.eq.9) then                                                    !/*is         56*/
        kbb=kbb+7                                                               !/*is         57*/
      endif                                                                     !/*is         58*/
      kind(noprts)=kbb-1                                                        !/*is         59*/
      ep(noprts)=bb(k+2)                                                        !/*is         60*/
      if (ep(noprts).le.dp0) then                                               !/*is         61*/
        ep(noprts)=1.0e-06                                                      !/*is         62*/
        write (io,*) ' nocas,bb(k+2) ',nocas,bb(k+2)                            !/*is         63*/
        write (itty,*) ' nocas,bb(k+2) ',nocas,bb(k+2)                          !/*is         64*/
      endif                                                                     !/*is         65*/
      pp=sqrt(bb(k+5)**2+bb(k+6)**2+bb(k+3)**2)                                 !/*is         66*/
      alpha(noprts)=bb(k+5)/pp                                                  !/*is         67*/
      beta(noprts)=bb(k+6)/pp                                                   !/*is         68*/
      gam(noprts)=bb(k+3)/pp                                                    !/*is         69*/
      if (kbb.eq.14) then                                                       !/*is         70*/
        if (rang().gt.dph) kind(noprts)=14                                      !/*is         71*/
      endif                                                                     !/*is         72*/
   10 continue                                                                  !/*is         73*/
   20 continue                                                                  !/*is         74*/
      extemp=max(ex+expr,dp0)                                                   !/*is         75*/
      exn(1)=ex                                                                 !/*is         76*/
      exn(2)=expr                                                               !/*is         77*/
      if (ixa(2).le.1.or.exn(2).lt.dp0) then                                    !/*is         78*/
        exn(1)=extemp                                                           !/*is         79*/
        exn(2)=dp0                                                              !/*is         80*/
      endif                                                                     !/*is         81*/
      if (ixa(1).le.1.or.exn(1).lt.dp0) then                                    !/*is         82*/
        exn(1)=dp0                                                              !/*is         83*/
        if (ixa(2).gt.1) exn(2)=extemp                                          !/*is         84*/
      endif                                                                     !/*is         85*/
      do 30 i=1,2                                                               !/*is         86*/
      ian(i)=ixa(i)                                                             !/*is         87*/
      iqn(i)=ixq(i)                                                             !/*is         88*/
   30 continue                                                                  !/*is         89*/
      if (csa(1).ne.dp0) go to 50                                               !/*is         90*/
      if (ixa(2).le.ixa(1)) then                                                !/*is         91*/
        if (ixa(2).eq.0) then                                                   !/*is         92*/
          if (ixa(1).eq.0) nnuc=0                                               !/*is         93*/
          go to 50                                                              !/*is         94*/
        endif                                                                   !/*is         95*/
        ja=ixa(2)                                                               !/*is         96*/
        jq=ixq(2)                                                               !/*is         97*/
        fa=ja                                                                   !/*is         98*/
        fq=jq                                                                   !/*is         99*/
        fn=fa-fq                                                                !/*is        100*/
        if (fq.le.dp2.and.fn.le.dp2) then                                       !/*is        101*/
          xmp0=xmasss(ixq(2)+1,ixa(2)-ixq(2)+1)                                 !/*is        102*/
        elseif (fq.eq.dp0.or.fn.eq.dp0) then                                    !/*is        103*/
          xmp0=fq*xmasss(2,1)+fn*xmasss(1,2)                                    !/*is        104*/
        else                                                                    !/*is        105*/
#ifdef OLDM                                                                     /*!is        106*/
          xmp0=energy(fa,fq)+fa*amumev-fq*elmass                                !/*is        107*/
#else                                                                           /*!is        108*/
          xmp0=xmnuc(fa,fq)                                                     !/*is        109*/
#endif                                                                          /*!is        110*/
        endif                                                                   !/*is        111*/
        xmp1=xmp0+exn(2)                                                        !/*is        112*/
        ex0=exn(2)                                                              !/*is        113*/
        emp2=sqrt(xmp1**2+pir(1)**2+pir(2)**2+pir(3)**2)                        !/*is        114*/
        call boost (emp2,pir,eprt1,pit,voc,vec0)                                !/*is        115*/
      else                                                                      !/*is        116*/
        nnuc=2                                                                  !/*is        117*/
        ja=ixa(1)                                                               !/*is        118*/
        if (ja.eq.0) go to 50                                                   !/*is        119*/
        jq=ixq(1)                                                               !/*is        120*/
        fa=ja                                                                   !/*is        121*/
        fq=jq                                                                   !/*is        122*/
        fn=fa-fq                                                                !/*is        123*/
        if (fq.le.dp2.and.fn.le.dp2) then                                       !/*is        124*/
          xmp0=xmasss(ixq(1)+1,ixa(1)-ixq(1)+1)                                 !/*is        125*/
        elseif (fq.eq.dp0.or.fn.eq.dp0) then                                    !/*is        126*/
          xmp0=fq*xmasss(2,1)+fn*xmasss(1,2)                                    !/*is        127*/
        else                                                                    !/*is        128*/
#ifdef OLDM                                                                     /*!is        129*/
          xmp0=energy(fa,fq)+fa*amumev-fq*elmass                                !/*is        130*/
#else                                                                           /*!is        131*/
          xmp0=xmnuc(fa,fq)                                                     !/*is        132*/
#endif                                                                          /*!is        133*/
        endif                                                                   !/*is        134*/
        xmp1=xmp0+exn(1)                                                        !/*is        135*/
        ex0=exn(1)                                                              !/*is        136*/
        eprt1=sqrt(xmp1**2+pit(1)**2+pit(2)**2+pit(3)**2)                       !/*is        137*/
      endif                                                                     !/*is        138*/
      pp=sqrt(pit(1)**2+pit(2)**2+pit(3)**2)                                    !/*is        139*/
      pit(1)=pit(1)/pp                                                          !/*is        140*/
      pit(2)=pit(2)/pp                                                          !/*is        141*/
      pit(3)=pit(3)/pp                                                          !/*is        142*/
      er=eprt1-xmp1                                                             !/*is        143*/
      if (ja.eq.1) then                                                         !/*is        144*/
        noprts=noprts+1                                                         !/*is        145*/
        i=noprts                                                                !/*is        146*/
        kind(i)=ja-jq                                                           !/*is        147*/
        ep(i)=er                                                                !/*is        148*/
        if (ep(i).le.dp0) then                                                  !/*is        149*/
          ep(i)=1.0e-06                                                         !/*is        150*/
          write (io,*) ' nocas,er ',nocas,er                                    !/*is        151*/
          write (itty,*) ' nocas,er ',nocas,er                                  !/*is        152*/
        endif                                                                   !/*is        153*/
        alpha(i)=pit(1)                                                         !/*is        154*/
        beta(i)=pit(2)                                                          !/*is        155*/
        gam(i)=pit(3)                                                           !/*is        156*/
        go to 50                                                                !/*is        157*/
      endif                                                                     !/*is        158*/
      call moment (jq,ja,pit(1),pit(2),pit(3),ex0,er,ierr,exi,vp,erc,npt        !/*is        159*/
     & ,jjp)                                                                    !/*is        160*/
      if (ierr.lt.0) then                                                       !/*is        161*/
        noprts=noprts+1                                                         !/*is        162*/
        i=noprts                                                                !/*is        163*/
        if (ja.eq.2) then                                                       !/*is        164*/
          kind(i)=7                                                             !/*is        165*/
        elseif (ja.eq.4) then                                                   !/*is        166*/
          kind(i)=10                                                            !/*is        167*/
        elseif (jq.eq.1) then                                                   !/*is        168*/
          kind(i)=8                                                             !/*is        169*/
        else                                                                    !/*is        170*/
          kind(i)=9                                                             !/*is        171*/
        endif                                                                   !/*is        172*/
        ep(i)=sqrt(pp+xmp0**2)-xmp0                                             !/*is        173*/
        if (ep(i).le.dp0) then                                                  !/*is        174*/
          ep(i)=1.0e-06                                                         !/*is        175*/
          write (io,*) ' nocas,pp,xmp0 ',nocas,pp,xmp0                          !/*is        176*/
          write (itty,*) ' nocas,pp,xmp0 ',nocas,pp,xmp0                        !/*is        177*/
        endif                                                                   !/*is        178*/
        alpha(i)=pit(1)                                                         !/*is        179*/
        beta(i)=pit(2)                                                          !/*is        180*/
        gam(i)=pit(3)                                                           !/*is        181*/
        go to 50                                                                !/*is        182*/
      elseif (ierr.gt.0) then                                                   !/*is        183*/
        nocas=igetd(1)                                                          !/*is        184*/
        write (io,60) nocas,jq,ja                                               !/*is        185*/
        lkcan='islink 1'                                                        !/*is        186*/
        kcan=1                                                                  !/*is        187*/
        go to 50                                                                !/*is        188*/
      endif                                                                     !/*is        189*/
      do 40 i=1,npt                                                             !/*is        190*/
      j=noprts+i                                                                !/*is        191*/
      ipt=jjp(i)                                                                !/*is        192*/
      if (ipt.le.2) then                                                        !/*is        193*/
        kind(j)=2-ipt                                                           !/*is        194*/
      elseif (ipt.le.5) then                                                    !/*is        195*/
        kind(j)=ipt+4                                                           !/*is        196*/
      elseif (ipt.eq.7) then                                                    !/*is        197*/
        kind(j)=10                                                              !/*is        198*/
      else                                                                      !/*is        199*/
        nocas=igetd(1)                                                          !/*islk        1*/
        write (io,70) nocas,jq,ja,ipt                                           !/*islk        2*/
        lkcan='islink 2'                                                        !/*islk        3*/
        kcan=1                                                                  !/*islk        4*/
        go to 50                                                                !/*islk        5*/
      endif                                                                     !/*is        201*/
      if (exi(i).ne.dp0) call expirx(1,'islink','exi(i).ne.dp0')                !/*islk        6*/
      ep(j)=erc(i)                                                              !/*is        203*/
      if (ep(j).le.dp0) then                                                    !/*is        204*/
        ep(j)=1.0e-06                                                           !/*is        205*/
        write (io,*) ' nocas,erc(i) ',nocas,erc(i)                              !/*is        206*/
        write (itty,*) ' nocas,erc(i) ',nocas,erc(i)                            !/*is        207*/
      endif                                                                     !/*is        208*/
      alpha(j)=vp(1,i)                                                          !/*is        209*/
      beta(j)=vp(2,i)                                                           !/*is        210*/
      gam(j)=vp(3,i)                                                            !/*is        211*/
   40 continue                                                                  !/*is        212*/
      noprts=noprts+npt                                                         !/*is        213*/
   50 continue                                                                  !/*is        214*/
      return                                                                    !/*is        215*/
!                                                                               !/*is        216*/
   60 format (2x,i6,' jq=',i3,' ja=',i3)                                        !/*is        217*/
!                                                                               !/*islk        7*/
   70 format (2x,i6,' jq=',i3,' ja=',i3,' FBRL ipt =',i3/                       !/*islk        8*/
     & ' (too much projectile pickup)')                                         !/*islk        9*/
!                                                                               !/*islk       10*/
      end                                                                       !/*is        218*/

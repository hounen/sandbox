!     $Id: sighad.F,v 1.4 2002/03/27 18:14:50 gwm Exp $                       !/*si          1*/
      function sighad (is,a1,z1,a2,z2,t0)                                       !/*si          2*/
!  B&P ROUTINE                                                                  !/*si          3*/
!     code for calculation of nucleon-nucleus and pion-nucleus                  !/*si          4*/
!     total(is=1) and nonelastic(is=2)cross-sections(mbarns)                    !/*si          5*/
!     is=0 exit                                                                 !/*si          6*/
!     a1=1.0 for nucleon or 0<a1<0.2d0 for pions                                !/*si          7*/
!     z1-projectile charge number                                               !/*si          8*/
!     a2,z2- target nucleus mass and charge numbers (4.0<=a2<=239.0)            !/*si          9*/
!     t- projectile particle kinetic energy(mev;14(20)mev<t<1tev)               !/*si         10*/
!                                                                               !/*si         11*/
#include <lcs/double.h>                                                         /*!si         12*/
#include <hexs/barpo.h>                                                         /*!si         13*/
!                                                                               !/*si         14*/
!     aai(k)- target mass numbers                                               !/*si         15*/
!     iener(l,in)-projectile energy(mev)                                        !/*si         16*/
! cross-sections are stored in isig1(k,l),isig2(k,l),isig3(k,l),isig4(k,        !/*si         17*/
!      isig1(l,k)- neutron cross sections                                       !/*si         18*/
!      isig2(l,k)- proton cross sections                                        !/*si         19*/
!      isig3(l,k)- pi- meson cross sections                                     !/*si         20*/
!      isig4(l,k)- pi+ meson cross sections                                     !/*si         21*/
!      k- vs. target mass numbers                                               !/*si         22*/
!      l- vs. energy                                                            !/*si         23*/
!                                                                               !/*si         24*/
      dimension ss(19), ee(3), ff(3), sig(2), jsig(2), ne(2)                    !/*si         25*/
      t=t0                                                                      !/*si         26*/
      sighad=dp0                                                                !/*si         27*/
      if (is.le.0) return                                                       !/*si         28*/
      if (a1.le.dp0.or.a1.gt.dp1) go to 110                                     !/*si         29*/
      if (a1.lt.dp1.and.a1.gt.0.2d0) go to 110                                  !/*si         30*/
!     if (a2.lt.4.0.or.a2.gt.239.) go to 130                                    !/*si         31*/
      if (a2.lt.dp2.or.a2.gt.260.d0) go to 130                                  !/*si         32*/
      if (abs(z1).gt.dp1) go to 110                                             !/*si         33*/
      if (t.lt.14.d0) go to 120                                                 !/*si         34*/
!     if (t.lt.20.0.and.a1.lt.0.2d0) go to 120                                  !/*si         35*/
      if (t.lt.19.999d0.and.a1.lt.0.2d0) go to 120                              !/*si         36*/
      ns=4                                                                      !/*si         37*/
      ne(1)=51                                                                  !/*si         38*/
      ne(2)=43                                                                  !/*si         39*/
      nel=19                                                                    !/*si         40*/
!     ne(in) - number of energy points (51 for nucleons,43 for pions)           !/*si         41*/
!     nel - number of targets (19 for nucleons and for pions)                   !/*si         42*/
!     in=1 for nucleons                                                         !/*si         43*/
!     in=2 for pions                                                            !/*si         44*/
      if (a1.eq.dp1) in=1                                                       !/*si         45*/
      if (a1.lt.0.2d0) in=2                                                     !/*si         46*/
      in1=(in-1)*ne(1)                                                          !/*si         47*/
      is1=1                                                                     !/*si         48*/
      is2=2                                                                     !/*si         49*/
      if (t.ge.1.d+06) t=1.d+06                                                 !/*si         50*/
!     selection of energy iener(lp,in)<t<iener(lp+1,in)                         !/*si         51*/
      do 10 l=1,ne(in)                                                          !/*si         52*/
      ti=iener(l+in1)                                                           !/*si         53*/
      if (ti-t) 10,20,20                                                        !/*si         54*/
   10 continue                                                                  !/*si         55*/
   20 lp=l-1                                                                    !/*si         56*/
      if (l.le.2) lp=1                                                          !/*si         57*/
      if (l.ge.ne(in)) lp=ne(in)-1                                              !/*si         58*/
      npar=1                                                                    !/*si         59*/
      nee=3                                                                     !/*si         60*/
!     npar=1 for neutron,proton,pi-,p+.                                         !/*si         61*/
!     npar=2 for meson pi-0                                                     !/*si         62*/
      if (z1.eq.dp0.and.a1.le.0.2d0) npar=2                                     !/*si         63*/
      do 100 ipar=1,npar                                                        !/*si         64*/
      zp1=z1                                                                    !/*si         65*/
      if (ipar.eq.1.and.npar.eq.2) zp1=-1.                                      !/*si         66*/
      if (ipar.eq.2.and.npar.eq.2) zp1=1.                                       !/*si         67*/
      if (zp1.eq.dp0.and.a1.eq.dp1) js=1                                        !/*si         68*/
      if (zp1.eq.dp1.and.a1.eq.dp1) js=2                                        !/*si         69*/
      if (zp1.lt.dp0.and.a1.lt.0.2d0) js=3                                      !/*si         70*/
      if (zp1.eq.dp1.and.a1.lt.0.2d0) js=4                                      !/*si         71*/
!     flag js=1  cross sections for neutrons                                    !/*si         72*/
!     flag js=2  cross sections for protons                                     !/*si         73*/
!     flag js=3  cross sections for mesons pi-                                  !/*si         74*/
!     flag js=4  cross sections for mesons pi+                                  !/*si         75*/
!                                                                               !/*si         76*/
      do 90 li=1,nee                                                            !/*si         77*/
      l1=lp-1+li                                                                !/*si         78*/
      ee(li)=iener(l1+in1)                                                      !/*si         79*/
      do 80 k=1,nel                                                             !/*si         80*/
      kn=(k-1)*ne(in)                                                           !/*si         81*/
      go to (30,40,50,60), js                                                   !/*si         82*/
!     jsig(1)-total cross sections                                              !/*si         83*/
!     jsig(2)-nonelastic cross sections                                         !/*si         84*/
   30 jsig(1)=isig1(kn+l1)/10000                                                !/*si         85*/
      jsig(2)=isig1(kn+l1)-10000*jsig(1)                                        !/*si         86*/
      go to 70                                                                  !/*si         87*/
   40 jsig(1)=isig2(kn+l1)/10000                                                !/*si         88*/
      jsig(2)=isig2(kn+l1)-10000*jsig(1)                                        !/*si         89*/
      go to 70                                                                  !/*si         90*/
   50 jsig(1)=isig3(kn+l1)/10000                                                !/*si         91*/
      jsig(2)=isig3(kn+l1)-10000*jsig(1)                                        !/*si         92*/
      go to 70                                                                  !/*si         93*/
   60 jsig(1)=isig4(kn+l1)/10000                                                !/*si         94*/
      jsig(2)=isig4(kn+l1)-10000*jsig(1)                                        !/*si         95*/
   70 continue                                                                  !/*si         96*/
   80 ss(k)=jsig(is)                                                            !/*si         97*/
!     quadratic interpolation of cross sectons vs target mass                   !/*si         98*/
   90 ff(li)=eint(a2,aai,ss,nel,is2)                                            !/*si         99*/
!     linear interpolation of cross sections vs energy                          !/*si        100*/
  100 sig(ipar)=eint(t,ee,ff,nee,is1)                                           !/*si        101*/
      if (npar.eq.1) sighad=sig(1)                                              !/*si        102*/
      if (npar.eq.2) sighad=(sig(1)+sig(2))/2                                   !/*si        103*/
!      write(6,*) ' sig(1),sig(2) ',sig(1),sig(2)                               !/*si        104*/
      if (sighad) 105,150,150                                                   !/*si        105*/
  105 write (6,180) a1,z1,a2,z2,t0,sighad                                       !/*si        106*/
      go to 140                                                                 !/*si        107*/
  110 continue                                                                  !/*si        108*/
      write (6,160) a1,z1                                                       !/*si        109*/
      go to 140                                                                 !/*si        110*/
  120 continue                                                                  !/*si        111*/
!     write (6,170)                                                             !/*si        112*/
      go to 140                                                                 !/*si        113*/
  130 write (6,170) a2                                                          !/*si        114*/
  140 continue                                                                  !/*si        115*/
      sighad=dp0                                                                !/*si        116*/
  150 continue                                                                  !/*si        117*/
      return                                                                    !/*si        118*/
!                                                                               !/*si        119*/
  160 format (' error in input (a1,z1) of function sighad a1=',f9.4,            !/*si        120*/
     & ' z1=',f9.4)                                                             !/*si        121*/
  170 format (' error in input (a2,z2) of function sighad a2=',f9.4)            !/*si        122*/
  180 format (' error in function sighad a1,z1,a2,z2,t0,sighad=',4f9.4,         !/*si        123*/
     & 1p2d12.4)                                                                !/*si        124*/
      end                                                                       !/*si        125*/

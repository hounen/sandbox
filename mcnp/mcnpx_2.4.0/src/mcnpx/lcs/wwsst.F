!     $Id: wwsst.F,v 1.4 2002/03/27 18:15:26 gwm Exp $                        !/*ww          1*/
      subroutine wwsst (ipl)                                                    !/*ww          2*/
#include <lcs/double.h>                                                         /*!ww          3*/
!     ==================================================================        !/*ww          4*/
!     wwsst stores some quantities from wws into st.  if a pion                 !/*ww          5*/
!     interacts forming an isobar, characteristics of pion (stored into         !/*ww          6*/
!     wws by subr save) are stored into st.  also in allowed isobar             !/*ww          7*/
!     exchange scattering, capture and decay, characteristics of the            !/*ww          8*/
!     isobar (stored into wws by subr save) are stored into st.                 !/*ww          9*/
!                                                                               !/*ww         10*/
! WWSST  calls   SQRT  DCNPAR  COUNT  FLTRNF                                    !/*ww         11*/
! called by GAMMA  PSEL                                                         !/*ww         12*/
!     ==================================================================        !/*ww         13*/
!                                                                               !/*ww         14*/
#include <lcs/inout.h>                                                          /*!ww         15*/
#include <lcs/vegas.h>                                                          /*!ww         16*/
      icsb13=icsb(13)                                                           !/*ww         17*/
      ig=ws(2)/dp5+1.d-03                                                       !/*ww         18*/
      igf=ig*5+16                                                               !/*ww         19*/
      ws(2)=ws(2)+dp5                                                           !/*ww         20*/
      icsb(15)=icsb(15)+1                                                       !/*ww         21*/
      if (icsb(15).ge.i391) go to 330                                           !/*ww         22*/
      icsb8=icsb(15)*i81                                                        !/*ww         23*/
      st(icsb8+1)=-(ws(1)+0.1d0)                                                !/*ww         24*/
      st(icsb8+2)=ws(2)                                                         !/*ww         25*/
      st(icsb8+3)=wws(3)                                                        !/*ww         26*/
      st(icsb8+4)=wws(4)                                                        !/*ww         27*/
      st(icsb8+5)=wws(5)                                                        !/*ww         28*/
      st(icsb8+6)=dp0                                                           !/*ww         29*/
      st(icsb8+7)=dp0                                                           !/*ww         30*/
      st(icsb8+8)=dp0                                                           !/*ww         31*/
      st(icsb8+9)=ws(9)                                                         !/*ww         32*/
      st(icsb8+10)=ws(10)                                                       !/*ww         33*/
      st(icsb8+11)=ws(11)                                                       !/*ww         34*/
      st(icsb8+12)=ws(12)                                                       !/*ww         35*/
      st(icsb8+13)=ws(13)                                                       !/*ww         36*/
      st(icsb8+14)=ws(14)                                                       !/*ww         37*/
      st(icsb8+15)=ws(25)                                                       !/*ww         38*/
      st(icsb8+16)=wws(24)                                                      !/*ww         39*/
      do 10 i=17,igf                                                            !/*ww         40*/
   10 st(i+icsb8)=st(i+icsb13)                                                  !/*ww         41*/
      st(icsb8+i81)=wws(31)                                                     !/*ww         42*/
      i8igf=icsb8+igf                                                           !/*ww         43*/
      st(i8igf+1)=ws(15)                                                        !/*ww         44*/
      st(i8igf+2)=ws(10)                                                        !/*ww         45*/
      st(i8igf+3)=ws(11)                                                        !/*ww         46*/
      st(i8igf+4)=ws(12)                                                        !/*ww         47*/
      st(i8igf+5)=wws(5)                                                        !/*ww         48*/
      j2=ws(2)/dp5+1.d-03                                                       !/*ww         49*/
      ws(2)=dp5+ws(2)-j2*dp5                                                    !/*ww         50*/
!     if(ind(15).eq.1) ws(2)=dp5                                                !/*ww         51*/
      if (ind(16).eq.1) go to 30                                                !/*ww         52*/
      if (ind(14).ne.1) return                                                  !/*ww         53*/
      i=wwsp(3)                                                                 !/*ww         54*/
      ibt=wws(3)                                                                !/*ww         55*/
      lbt=wws(31)                                                               !/*ww         56*/
      go to 40                                                                  !/*ww         57*/
   20 return                                                                    !/*ww         58*/
   30 i=wsp(3)                                                                  !/*ww         59*/
!-----------------------------------------------------------------------        !/*ww         60*/
! Somehow, particles are getting here with IND(32)=1 but WS(14)>>CSA(9).        !/*ww         61*/
! The problem is undoubtedly elsewhere, so this is temporary.                   !/*ww         62*/
   40 continue                                                                  !/*ww         63*/
      ist=0                                                                     !/*ww         64*/
      if (ind(32).eq.1.and.istept.eq.0) ind(32)=0                               !/*ww         65*/
      if (ind(32).eq.0.and.istepp.eq.0) ind(32)=1                               !/*ww         66*/
      if ((wws(3).gt.dp2).and.(wws(3).lt.dp10)) go to 70                        !/*ww         67*/
!-----------------------------------------------------------------------        !/*ww         68*/
      it=1                                                                      !/*ww         69*/
      if (wwsp(31).ne.dp0) go to 50                                             !/*ww         70*/
      if (wws(31).ne.dp0) it=4                                                  !/*ww         71*/
      entp=etep(1)                                                              !/*ww         72*/
      go to 60                                                                  !/*ww         73*/
   50 it=2                                                                      !/*ww         74*/
      if (wws(31).ne.dp0) it=3                                                  !/*ww         75*/
      entp=etep(2)                                                              !/*ww         76*/
   60 monin(it)=monin(it)+1                                                     !/*ww         77*/
      monin(6)=monin(6)+1                                                       !/*ww         78*/
      if (ind(32).eq.2) monin(7)=monin(7)+1                                     !/*ww         79*/
      se(it)=se(it)+entp                                                        !/*ww         80*/
      ses(it)=ses(it)+entp*entp                                                 !/*ww         81*/
      srs1=ws(10)*ws(10)+ws(11)*ws(11)                                          !/*ww         82*/
      srs(it)=srs(it)+srs1                                                      !/*ww         83*/
      sr(it)=sr(it)+sqrt(srs1)                                                  !/*ww         84*/
      if (it.ne.4) go to 70                                                     !/*ww         85*/
      if (wws(2).gt.5.1d0.or.wws(31).le.dp0) go to 70                           !/*ww         86*/
      it=2                                                                      !/*ww         87*/
      go to 60                                                                  !/*ww         88*/
!-----------------------------------------------------------------------        !/*ww         89*/
!  70 if (ind(32)-1) 100,80,71                                                  !/*ww         90*/
   70 continue                                                                  !/*ww         91*/
      if (ind(14).eq.1) then                                                    !/*ww         92*/
        jws3=ws(3)                                                              !/*ww         93*/
        jwsp3=wsp(3)                                                            !/*ww         94*/
      else                                                                      !/*ww         95*/
        jws3=ibt                                                                !/*ww         96*/
        jwsp3=i                                                                 !/*ww         97*/
      endif                                                                     !/*ww         98*/
      if (ind(32)-1) 200,130,80                                                 !/*ww         99*/
!                           cas-cas interactions (for a1+a2)                    !/*ww        100*/
!-----some complications due to partially allowed pauli (y.yariv 12/29/8        !/*ww        101*/
   80 if (ipl.le.0) go to 20                                                    !/*ww        102*/
      istt=0                                                                    !/*ww        103*/
      if (istept.eq.0) istt=2                                                   !/*ww        104*/
      go to (90,100,110,120), ipl                                               !/*ww        105*/
   90 sel(jws3)=sel(jws3)+dp1                                                   !/*ww        106*/
      call dcnpar (tnpar,nnt,jws3,isc)                                          !/*ww        107*/
      go to 20                                                                  !/*ww        108*/
  100 spr(jws3+istt)=spr(jws3+istt)+dp1                                         !/*ww        109*/
      call dcnpar (pnpar,nnp,jws3,isc)                                          !/*ww        110*/
      go to 20                                                                  !/*ww        111*/
  110 sel(jwsp3)=sel(jwsp3)+dp1                                                 !/*ww        112*/
      call dcnpar (tnpar,nnt,jwsp3,isc)                                         !/*ww        113*/
      go to 20                                                                  !/*ww        114*/
  120 spr(jwsp3+istt)=spr(jwsp3+istt)+dp1                                       !/*ww        115*/
      call dcnpar (pnpar,nnp,jwsp3,isc)                                         !/*ww        116*/
      go to 20                                                                  !/*ww        117*/
!                           target interactions (for a1+a2)                     !/*ww        118*/
  130 ws(i+21)=ws(i+21)+dp1                                                     !/*ww        119*/
      if (ipl.eq.0) go to 180                                                   !/*ww        120*/
      go to (140,150,160,170), ipl                                              !/*ww        121*/
  140 if (i.eq.jws3) go to 190                                                  !/*ww        122*/
      sel(jws3)=sel(jws3)+dp1                                                   !/*ww        123*/
      call dcnpar (tnpar,nnt,jws3,isc)                                          !/*ww        124*/
      go to 180                                                                 !/*ww        125*/
  150 spr(jws3+2)=spr(jws3+2)+dp1                                               !/*ww        126*/
      call dcnpar (pnpar,nnp,jws3,isc)                                          !/*ww        127*/
      go to 180                                                                 !/*ww        128*/
  160 if (i.eq.jwsp3) go to 190                                                 !/*ww        129*/
      sel(jwsp3)=sel(jwsp3)+dp1                                                 !/*ww        130*/
      call dcnpar (tnpar,nnt,jwsp3,isc)                                         !/*ww        131*/
      go to 180                                                                 !/*ww        132*/
  170 spr(jwsp3+2)=spr(jwsp3+2)+1                                               !/*ww        133*/
      call dcnpar (pnpar,nnp,jwsp3,isc)                                         !/*ww        134*/
!-----------------------------------------------------------------------        !/*ww        135*/
!-----                                                                          !/*ww        136*/
  180 sel(i)=sel(i)-dp1                                                         !/*ww        137*/
      nnt=nnt+1                                                                 !/*ww        138*/
      tnpar(1,nnt)=ws(10)                                                       !/*ww        139*/
      tnpar(2,nnt)=ws(11)                                                       !/*ww        140*/
      tnpar(3,nnt)=ws(12)                                                       !/*ww        141*/
      tnpar(4,nnt)=i                                                            !/*ww        142*/
      pwsp=dp0                                                                  !/*ww        143*/
      if (wws(3).gt.dp10) pwsp=(sqrt(wsp(5)*wsp(5)-wsp(4)*wsp(4)))              !/*ww        144*/
  190 ws(15)=1.d+02*i+ws(15)                                                    !/*ww        145*/
      ex=ex+exth                                                                !/*ww        146*/
! take out a nucleon                                                            !/*ww        147*/
      call countv (fws3,1,1)                                                    !/*ww        148*/
      pws=sqrt(ws(5)*ws(5)-ws(4)*ws(4))                                         !/*ww        149*/
      pwsp=dp0                                                                  !/*ww        150*/
      if (wws(3).gt.dp10) pwsp=sqrt(wsp(5)*wsp(5)-wsp(4)*wsp(4))                !/*ww        151*/
      po=sqrt(wws(5)*wws(5)-wws(4)*wws(4))                                      !/*ww        152*/
      pxh=wws(6)*po-ws(6)*pws-wsp(6)*pwsp                                       !/*ww        153*/
      pyh=wws(7)*po-ws(7)*pws-wsp(7)*pwsp                                       !/*ww        154*/
      pzh=wws(8)*po-ws(8)*pws-wsp(8)*pwsp                                       !/*ww        155*/
      pit(1)=pit(1)+pxh                                                         !/*ww        156*/
      pit(2)=pit(2)+pyh                                                         !/*ww        157*/
      pit(3)=pit(3)+pzh                                                         !/*ww        158*/
      pit(4)=pit(4)+ws(11)*pzh-ws(12)*pyh                                       !/*ww        159*/
      pit(5)=pit(5)+ws(12)*pxh-ws(10)*pzh                                       !/*ww        160*/
      pit(6)=pit(6)+ws(10)*pyh-ws(11)*pxh                                       !/*ww        161*/
      if (wws(2).le.5.1d0.and.wws(31).gt.dp0) go to 270                         !/*ww        162*/
      go to 20                                                                  !/*ww        163*/
!                           projectile interactions (for a1+a2)                 !/*ww        164*/
  200 ist=0                                                                     !/*ww        165*/
      ck=spr(i)/(spr(i+2)+spr(i))                                               !/*ww        166*/
      rand=rang()                                                               !/*ww        167*/
      if (rand.gt.ck) ist=2                                                     !/*ww        168*/
!-----some complications due to partially allowed pauli (y.yariv 12/29/8        !/*ww        169*/
      if (ipl.eq.0) go to 250                                                   !/*ww        170*/
!-----------------------------------------------------------------------        !/*ww        171*/
      go to (210,220,230,240), ipl                                              !/*ww        172*/
  210 sel(jws3)=sel(jws3)+dp1                                                   !/*ww        173*/
      call dcnpar (tnpar,nnt,jws3,isc)                                          !/*ww        174*/
      go to 250                                                                 !/*ww        175*/
  220 if (i.eq.jws3) go to 260                                                  !/*ww        176*/
      spr(jws3+ist)=spr(jws3+ist)+dp1                                           !/*ww        177*/
      call dcnpar (pnpar,nnp,jws3,isc)                                          !/*ww        178*/
      go to 250                                                                 !/*ww        179*/
  230 sel(jwsp3)=sel(jwsp3)+dp1                                                 !/*ww        180*/
      call dcnpar (tnpar,nnt,jwsp3,isc)                                         !/*ww        181*/
      go to 250                                                                 !/*ww        182*/
  240 if (i.eq.jwsp3) go to 260                                                 !/*ww        183*/
      spr(jwsp3+ist)=spr(jwsp3+ist)+dp1                                         !/*ww        184*/
      call dcnpar (pnpar,nnp,jwsp3,isc)                                         !/*ww        185*/
      if (ist.eq.0) then                                                        !/*ww        186*/
        nrep=nrep+1                                                             !/*ww        187*/
        jrep(nrep)=i                                                            !/*ww        188*/
      endif                                                                     !/*ww        189*/
!-----------------------------------------------------------------------        !/*ww        190*/
!-----                                                                          !/*ww        191*/
  250 spr(i+ist)=spr(i+ist)-dp1                                                 !/*ww        192*/
      nnp1=nnp+1                                                                !/*ww        193*/
      pnpar(4,nnp1)=i                                                           !/*ww        194*/
  260 ws(15)=1.d+03+1.d+02*i+ws(15)                                             !/*ww        195*/
      wsp(i+21)=wsp(i+21)+dp1                                                   !/*ww        196*/
      expr=expr+eprh(1)                                                         !/*ww        197*/
! take out a nucleon                                                            !/*ww        198*/
      call countv (fwsp3,2,1)                                                   !/*ww        199*/
      ws10=ws(10)-proj(5)                                                       !/*ww        200*/
      ws11=ws(11)-proj(6)                                                       !/*ww        201*/
      ws12=(ws(12)-proj(7))/gama                                                !/*ww        202*/
      pir(1)=pir(1)+eprh(2)                                                     !/*ww        203*/
      pir(2)=pir(2)+eprh(3)                                                     !/*ww        204*/
      pir(3)=pir(3)+eprh(4)                                                     !/*ww        205*/
      pir(4)=pir(4)+ws11*eprh(4)-ws12*eprh(3)                                   !/*ww        206*/
      pir(5)=pir(5)+ws12*eprh(2)-ws10*eprh(4)                                   !/*ww        207*/
      pir(6)=pir(6)+ws10*eprh(3)-ws11*eprh(2)                                   !/*ww        208*/
      go to 290                                                                 !/*ww        209*/
  270 if (ipl.gt.0) go to 280                                                   !/*ww        210*/
      spr(ibt)=spr(ibt)-dp1                                                     !/*ww        211*/
      nnp1=nnp+1                                                                !/*ww        212*/
      pnpar(4,nnp1)=ibt                                                         !/*ww        213*/
  280 ws(15)=2.d+03+(ibt+i-2)*1.d+02+ws(15)                                     !/*ww        214*/
      wsp(ibt+21)=wsp(ibt+21)+dp1                                               !/*ww        215*/
      expr=expr+eh(lbt)                                                         !/*ww        216*/
! take out a nucleon                                                            !/*ww        217*/
      type=abs(jpro(lbt))                                                       !/*ww        218*/
      call countv (type,2,1)                                                    !/*ww        219*/
      ws10=ws(10)-proj(5)                                                       !/*ww        220*/
      ws11=ws(11)-proj(6)                                                       !/*ww        221*/
      ws12=(ws(12)-proj(7))/gama                                                !/*ww        222*/
      pir(1)=pir(1)+ph(1,lbt)                                                   !/*ww        223*/
      pir(2)=pir(2)+ph(2,lbt)                                                   !/*ww        224*/
      pir(3)=pir(3)+ph(3,lbt)                                                   !/*ww        225*/
      pir(4)=pir(4)+ws11*ph(3,lbt)-ws12*ph(2,lbt)                               !/*ww        226*/
      pir(5)=pir(5)+ws12*ph(1,lbt)-ws10*ph(3,lbt)                               !/*ww        227*/
      pir(6)=pir(6)+ws10*ph(2,lbt)-ws11*ph(1,lbt)                               !/*ww        228*/
  290 if (ipl.gt.0) go to 300                                                   !/*ww        229*/
      nnp=nnp+1                                                                 !/*ww        230*/
      pnpar(1,nnp)=ws(10)                                                       !/*ww        231*/
      pnpar(2,nnp)=ws(11)                                                       !/*ww        232*/
      pnpar(3,nnp)=ws(12)                                                       !/*ww        233*/
  300 if (ind(32).eq.1) return                                                  !/*ww        234*/
      if (ist.eq.0) go to 310                                                   !/*ww        235*/
      if (istept.eq.0) return                                                   !/*ww        236*/
      npmon(i)=npmon(i)+1                                                       !/*ww        237*/
      npmon(3)=npmon(3)+1                                                       !/*ww        238*/
#ifdef NPM                                                                      /*!ww        239*/
      write (io,340) npmon,(ind(iii),iii=1,35)                                  !/*ww        240*/
#endif                                                                          /*!ww        241*/
      return                                                                    !/*ww        242*/
  310 if (istept.gt.0) go to 320                                                !/*ww        243*/
      npmon(i)=npmon(i)-1                                                       !/*ww        244*/
      npmon(3)=npmon(3)-1                                                       !/*ww        245*/
#ifdef NPM                                                                      /*!ww        246*/
      write (io,350) npmon,(ind(iii),iii=1,35)                                  !/*ww        247*/
#endif                                                                          /*!ww        248*/
  320 if (ipl.eq.2.or.ipl.eq.4) return                                          !/*ww        249*/
      nrep=nrep+1                                                               !/*ww        250*/
      jrep(nrep)=i                                                              !/*ww        251*/
      return                                                                    !/*ww        252*/
  330 kcan=2                                                                    !/*ww        253*/
      lkcan='wwsst'                                                             !/*ww        254*/
      return                                                                    !/*ww        255*/
!                                                                               !/*ww        256*/
  340 format ('wwsst.163 +',3i5/35i3)                                           !/*ww        257*/
  350 format ('wwsst.167 -',3i5/35i3)                                           !/*ww        258*/
      end                                                                       !/*ww        259*/

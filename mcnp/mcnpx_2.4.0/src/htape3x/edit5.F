!     $Id: edit5.F,v 1.5 2002/03/27 18:13:57 gwm Exp $                        !/*ed          1*/
      subroutine edit5 (tlly,tlsq,cov,maxa,maxa1,m)                             !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/lpt.h>                                                        /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/bndy.h>                                                       /*!ed          6*/
#include <htape3x/ctconv.h>                                                     /*!ed          7*/
      dimension tlly(4,maxa1,1), tlsq(4,maxa1,1)                                !/*ed          8*/
      dimension cov(2,maxa1,1)                                                  !/*ed          9*/
      dimension val(4), sd(4)                                                   !/*ed         10*/
#ifdef HTPLOT                                                                   /*!ed         11*/
      dimension y00(260,4)                                                      !/*ed         12*/
      equivalence (y0(1),y00(1,1))                                              !/*ed         13*/
      character laby1*8, laby2*21, labx*11, title1*34                           !/*ed         14*/
      character title2*27, tmat*4, treg*4, title*41                             !/*ed         15*/
      character title3*33, title4*26                                            !/*ed         16*/
      character bin*3                                                           !/*ed         17*/
      data laby1 /'fraction'/, laby2 /'mean excitation (mev)'/,                 !/*ed         18*/
     & labx /'mass number'/,                                                    !/*ed         19*/
     & title1 /'post-evaporation mass distribution'/,                           !/*ed         20*/
     & title2 /'post-evaporation excitation'/, tmat /' mat'/,                   !/*ed         21*/
     & treg /' reg'/, title3 /'pre-evaporation mass distribution'/,             !/*ed         22*/
     & title4 /'pre-evaporation excitation'/                                    !/*ed         23*/
      do 10 j=1,4                                                               !/*ed         24*/
      do 10 i=1,260                                                             !/*ed         25*/
   10 y00(i,j)=dp0                                                              !/*ed         26*/
      if (nparm.gt.0) write (bin,'(i3)') lparm(m)                               !/*ed         27*/
#endif                                                                          /*!ed         28*/
      if (lbunch) then                                                          !/*ed         29*/
        if (jmat.eq.0) write (io,210) (lparm(i),i=1,nparm)                      !/*ed         30*/
        if (jmat.ne.0) write (io,220) (lparm(i),i=1,nparm)                      !/*ed         31*/
      elseif (nparm.gt.0) then                                                  !/*ed         32*/
        if (jmat.eq.0) write (io,160) lparm(m)                                  !/*ed         33*/
        if (jmat.ne.0) write (io,170) lparm(m)                                  !/*ed         34*/
      else                                                                      !/*ed         35*/
        write (io,180)                                                          !/*ed         36*/
      endif                                                                     !/*ed         37*/
      if (lsnprm) write (io,230) dnparm(m)                                      !/*ed         38*/
      write (io,240)                                                            !/*ed         39*/
      do 40 j=1,maxa1                                                           !/*ed         40*/
      val(1)=tlly(1,j,m)                                                        !/*ed         41*/
      val(3)=tlly(3,j,m)                                                        !/*ed         42*/
      if (val(1).eq.dp0.and.val(3).eq.dp0) go to 40                             !/*ed         43*/
      val(2)=dp0                                                                !/*ed         44*/
      val(4)=dp0                                                                !/*ed         45*/
      sd(2)=dp0                                                                 !/*ed         46*/
      sd(4)=dp0                                                                 !/*ed         47*/
      sd(1)=tlsq(1,j,m)                                                         !/*ed         48*/
      sd(3)=tlsq(3,j,m)                                                         !/*ed         49*/
      if (val(1).ne.dp0) val(2)=tlly(2,j,m)/val(1)                              !/*ed         50*/
      if (val(3).ne.dp0) val(4)=tlly(4,j,m)/val(3)                              !/*ed         51*/
      if (val(1).ne.dp0) sd(2)=sd(1)**2+tlsq(2,j,m)**2-2.0*cov(1,j,m)           !/*ed         52*/
      if (val(3).ne.dp0) sd(4)=sd(3)**2+tlsq(4,j,m)**2-2.0*cov(2,j,m)           !/*ed         53*/
      sd(2)=max(sd(2),dp0)                                                      !/*ed         54*/
      sd(4)=max(sd(4),dp0)                                                      !/*ed         55*/
      sd(2)=sqrt(sd(2))                                                         !/*ed         56*/
      sd(4)=sqrt(sd(4))                                                         !/*ed         57*/
      val(3)=val(3)/dnparm(m)                                                   !/*ed         58*/
      val(1)=val(1)/dnparm(m)                                                   !/*ed         59*/
#ifdef HTPLOT                                                                   /*!ed         60*/
      if (kplot.eq.0.or.j.gt.maxa) go to 30                                     !/*ed         61*/
      do 20 i=1,4                                                               !/*ed         62*/
   20 y00(j,i)=val(i)                                                           !/*ed         63*/
   30 continue                                                                  !/*ed         64*/
#endif                                                                          /*!ed         65*/
      if (j.lt.maxa1) write (io,190) j,(val(i),sd(i),i=1,4)                     !/*ed         66*/
      if (j.eq.maxa1) write (io,200) (val(i),sd(i),i=1,4)                       !/*ed         67*/
   40 continue                                                                  !/*ed         68*/
#ifdef HTPLOT                                                                   /*!ed         69*/
      if (kplot.eq.0) go to 140                                                 !/*ed         70*/
      write (bin,'(i3)') lparm(m)                                               !/*ed         71*/
      do 50 i=1,260                                                             !/*ed         72*/
      j=2*i-1                                                                   !/*ed         73*/
      xx(j)=real(i)-0.5                                                         !/*ed         74*/
   50 xx(j+1)=xx(j)                                                             !/*ed         75*/
      do 60 i=1,maxa1                                                           !/*ed         76*/
      nup=maxa1+1-i                                                             !/*ed         77*/
      if (y00(nup,1).gt.dp0) go to 70                                           !/*ed         78*/
   60 continue                                                                  !/*ed         79*/
   70 continue                                                                  !/*ed         80*/
      nup=nup+1                                                                 !/*ed         81*/
      do 80 i=1,maxa1                                                           !/*ed         82*/
      nlo=i                                                                     !/*ed         83*/
      if (y00(i,1).gt.dp0) go to 90                                             !/*ed         84*/
   80 continue                                                                  !/*ed         85*/
   90 continue                                                                  !/*ed         86*/
      n1=2*nlo-1                                                                !/*ed         87*/
      n2=2*nup-n1+1                                                             !/*ed         88*/
      call fix (base,y00(1,1),yy,nup)                                           !/*ed         89*/
      if (lbunch.or.nparm.eq.0) then                                            !/*ed         90*/
        title=title1                                                            !/*ed         91*/
      else                                                                      !/*ed         92*/
        if (jmat.ne.0) title=title1//tmat//bin                                  !/*ed         93*/
        if (jmat.eq.0) title=title1//treg//bin                                  !/*ed         94*/
      endif                                                                     !/*ed         95*/
      call hxlogy (xx(n1),yy(n1),n2,labx,laby1,title)                           !/*ed         96*/
      call gpage                                                                !/*ed         97*/
      nplot=nplot+1                                                             !/*ed         98*/
      write (io,150) nplot,icase,title                                          !/*ed         99*/
      call fix (base,y00(1,2),yy,nup)                                           !/*ed        100*/
      if (lbunch.or.nparm.eq.0) then                                            !/*ed        101*/
        title=title2                                                            !/*ed        102*/
      else                                                                      !/*ed        103*/
        if (jmat.ne.0) title=title2//tmat//bin                                  !/*ed        104*/
        if (jmat.eq.0) title=title2//treg//bin                                  !/*ed        105*/
      endif                                                                     !/*ed        106*/
      call hxy (xx(n1),yy(n1),n2,labx,laby2,title)                              !/*ed        107*/
      call gpage                                                                !/*ed        108*/
      nplot=nplot+1                                                             !/*ed        109*/
      write (io,150) nplot,icase,title                                          !/*ed        110*/
      do 100 i=1,maxa1                                                          !/*ed        111*/
      nup=maxa1+1-i                                                             !/*ed        112*/
      if (y00(nup,3).gt.dp0) go to 110                                          !/*ed        113*/
  100 continue                                                                  !/*ed        114*/
  110 continue                                                                  !/*ed        115*/
      nup=nup+1                                                                 !/*ed        116*/
      do 120 i=1,maxa1                                                          !/*ed        117*/
      nlo=i                                                                     !/*ed        118*/
      if (y00(i,3).gt.dp0) go to 130                                            !/*ed        119*/
  120 continue                                                                  !/*ed        120*/
  130 continue                                                                  !/*ed        121*/
      n1=2*nlo-1                                                                !/*ed        122*/
      n2=2*nup-n1+1                                                             !/*ed        123*/
      call fix (base,y00(1,3),yy,nup)                                           !/*ed        124*/
      if (lbunch.or.nparm.eq.0) then                                            !/*ed        125*/
        title=title3                                                            !/*ed        126*/
      else                                                                      !/*ed        127*/
        if (jmat.ne.0) title=title3//tmat//bin                                  !/*ed        128*/
        if (jmat.eq.0) title=title3//treg//bin                                  !/*ed        129*/
      endif                                                                     !/*ed        130*/
      call hxlogy (xx(n1),yy(n1),n2,labx,laby1,title)                           !/*ed        131*/
      call gpage                                                                !/*ed        132*/
      nplot=nplot+1                                                             !/*ed        133*/
      write (io,150) nplot,icase,title                                          !/*ed        134*/
      call fix (base,y00(1,4),yy,nup)                                           !/*ed        135*/
      if (lbunch.or.nparm.eq.0) then                                            !/*ed        136*/
        title=title4                                                            !/*ed        137*/
      else                                                                      !/*ed        138*/
        if (jmat.ne.0) title=title4//tmat//bin                                  !/*ed        139*/
        if (jmat.eq.0) title=title4//treg//bin                                  !/*ed        140*/
      endif                                                                     !/*ed        141*/
      call hxy (xx(n1),yy(n1),n2,labx,laby2,title)                              !/*ed        142*/
      call gpage                                                                !/*ed        143*/
      nplot=nplot+1                                                             !/*ed        144*/
      write (io,150) nplot,icase,title                                          !/*ed        145*/
  140 continue                                                                  !/*ed        146*/
#endif                                                                          /*!ed        147*/
      return                                                                    !/*ed        148*/
!                                                                               !/*ed        149*/
  150 format ('0plot no.',i3,' case no.',i3,5x,a)                               !/*ed        150*/
  160 format (/21x,'residual nuclei in cell',i4)                                !/*ed        151*/
  170 format (/21x,'residual nuclei in material',i4)                            !/*ed        152*/
  180 format (/25x,'total residual nuclei')                                     !/*ed        153*/
  190 format (' a =',i4,4(1pd16.4,0pf6.3))                                      !/*ed        154*/
  200 format (' all a  ',4(1pd16.4,0pf6.3))                                     !/*ed        155*/
  210 format (/10x,'residual nuclei in cells',15i4/(35x,15i4))                  !/*ed        156*/
  220 format (/10x,'residual nuclei in materials',15i4/(38x,15i4))              !/*ed        157*/
  230 format (/35x,'tally divided by',1pd14.5)                                  !/*ed        158*/
  240 format (/22x,'after evaporation',26x,'before evaporation'/                !/*ed        159*/
     & '  mass ',10x,'frequency',10x,'mean excitation',10x,'frequency',         !/*ed        160*/
     & 10x,'mean excitation')                                                   !/*ed        161*/
      end                                                                       !/*ed        162*/

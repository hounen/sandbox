!     $Id: selpt.F,v 1.4 2002/03/27 18:15:23 gwm Exp $                        !/*se          1*/
      subroutine selpt (gdc9,gdc10)                                             !/*se          2*/
#include <lcs/double.h>                                                         /*!se          3*/
!     ==================================================================        !/*se          4*/
!     selpt selects partner of a collision and also advances                    !/*se          5*/
!     the particle to a new position.                                           !/*se          6*/
!                                                                               !/*se          7*/
! SELPT  calls   LOOK  SQRT  FLTRNF  WRITEM                                     !/*se          8*/
! called by ISABEL                                                              !/*se          9*/
!     ==================================================================        !/*se         10*/
!                                                                               !/*se         11*/
#include <lcs/vegas.h>                                                          /*!se         12*/
! bug fix 4/6/89                                                                !/*se         13*/
      data xc, yc, zc /3*dp0/                                                   !/*se         14*/
!                                                                               !/*se         15*/
!     <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>        !/*se         16*/
!                                                                               !/*se         17*/
      if (ind(34).lt.0) go to 10                                                !/*se         18*/
      call look (ws(14),xa,istept)                                              !/*se         19*/
      istepp=0                                                                  !/*se         20*/
      if (csa(1).ne.dp0) go to 10                                               !/*se         21*/
      xc=ws(10)-proj(5)                                                         !/*se         22*/
      yc=ws(11)-proj(6)                                                         !/*se         23*/
      proj7=proj(7)-dp10*ws(16)*voc                                             !/*se         24*/
      zc=(ws(12)-proj7)/gama                                                    !/*se         25*/
      arg=sqrt(xc*xc+yc*yc+zc*zc)                                               !/*se         26*/
      call look (arg,ya,istepp)                                                 !/*se         27*/
   10 ind(30)=0                                                                 !/*se         28*/
      ind(33)=0                                                                 !/*se         29*/
      ind(31)=0                                                                 !/*se         30*/
      ind(32)=1                                                                 !/*se         31*/
      ind(34)=0                                                                 !/*se         32*/
      atr=sel(1)+sel(2)                                                         !/*se         33*/
      if (atr.ge.dp1.and.istept.ge.1) go to 20                                  !/*se         34*/
!          (no interactions in targ) possible if no target-partners or          !/*se         35*/
!          in the outermost region of the potential                             !/*se         36*/
      ind(30)=1                                                                 !/*se         37*/
      go to 50                                                                  !/*se         38*/
   20 csel=sel(1)/atr                                                           !/*se         39*/
      ind(33)=1                                                                 !/*se         40*/
      wsp(31)=dp0                                                               !/*se         41*/
!              select z of partner based on z,a of target                       !/*se         42*/
      r1=rang()                                                                 !/*se         43*/
      if (r1.gt.csel) go to 30                                                  !/*se         44*/
!            picked proton                                                      !/*se         45*/
      wsp(3)=dp1                                                                !/*se         46*/
      wsp(4)=amass(1)                                                           !/*se         47*/
      go to 40                                                                  !/*se         48*/
!            picked neutron                                                     !/*se         49*/
   30 wsp(3)=dp2                                                                !/*se         50*/
      wsp(4)=amass(2)                                                           !/*se         51*/
   40 wsp(19)=wsp(4)*wsp(4)                                                     !/*se         52*/
   50 apr=spr(1)+spr(2)+spr(3)+spr(4)                                           !/*se         53*/
      if ((istepp*apr).lt.0.1d0) go to 60                                       !/*se         54*/
      if (ws(31).eq.5.d+02) go to 60                                            !/*se         55*/
      if (ws(2).gt.5.1d0.or.ws(31).le.dp0) go to 70                             !/*se         56*/
   60 ind(31)=1                                                                 !/*se         57*/
      go to 90                                                                  !/*se         58*/
!              pick a partner out of the projectile                             !/*se         59*/
   70 csel=(spr(1)+spr(3))/apr                                                  !/*se         60*/
      ind(33)=1                                                                 !/*se         61*/
      wspr(3)=dp2                                                               !/*se         62*/
      wspr(4)=amass(2)                                                          !/*se         63*/
      r1=rang()                                                                 !/*se         64*/
      if (r1.gt.csel) go to 80                                                  !/*se         65*/
      wspr(3)=dp1                                                               !/*se         66*/
      wspr(4)=amass(1)                                                          !/*se         67*/
   80 wspr(19)=wspr(4)*wspr(4)                                                  !/*se         68*/
   90 do 100 i=8,18                                                             !/*se         69*/
      ind(i)=0                                                                  !/*se         70*/
  100 continue                                                                  !/*se         71*/
      ind(18)=0                                                                 !/*se         72*/
      ind(20)=0                                                                 !/*se         73*/
!***added for high energy                                                       !/*se         74*/
      ind(27)=0                                                                 !/*se         75*/
!                 ws24 = fermi energy                                           !/*se         76*/
      ind(28)=0                                                                 !/*se         77*/
      ind(29)=0                                                                 !/*se         78*/
      do 110 i=5,12                                                             !/*se         79*/
      gws(i)=ws(i)                                                              !/*se         80*/
  110 continue                                                                  !/*se         81*/
      gws(1)=ws(14)                                                             !/*se         82*/
      gws(2)=ws(24)                                                             !/*se         83*/
      gws(3)=ws(25)                                                             !/*se         84*/
      gws(15)=ws(31)                                                            !/*se         85*/
      gdc10=rang()                                                              !/*se         86*/
      r1=rang()                                                                 !/*se         87*/
      wsp(15)=dp2*r1-dp1                                                        !/*se         88*/
!       check if possibility of partner-choice exists this interaction          !/*se         89*/
!       in fermi-sea (ind33=1) or in cas-cas (ntdel.gt.1)                       !/*se         90*/
!       for no particles in fermi sea (ind(33)=0) below cutoff isobars          !/*se         91*/
!       leave the system                                                        !/*se         92*/
      if (ind(33).eq.1) go to 130                                               !/*se         93*/
      if (ntdel.gt.0.and.ind(22).ne.1.and.icsb(1).gt.1) go to 120               !/*se         94*/
!          flag that particle left the system                                   !/*se         95*/
      ind(34)=3                                                                 !/*se         96*/
      go to 240                                                                 !/*se         97*/
!                                                                               !/*se         98*/
! ind(22)=1 whenever isobar is forced to do something on the spot.              !/*se         99*/
!  another partner may be chosen but should not advance the particle.           !/*se        100*/
! otherwise one would be outside the nucleus if isobar is trying to             !/*se        101*/
!  leave but cannot decay because there is not enough momentum to be            !/*se        102*/
!  divided between the two decay particles.                                     !/*se        103*/
! change made 10/16/70 so that isobars below cut-off are not advanced           !/*se        104*/
!                                                                               !/*se        105*/
!   target isobars below cutoff returned; projectile                            !/*se        106*/
!   ones are frozen in the projectile                                           !/*se        107*/
!     ind22=1==>isobar below cutoff; ws31=0==>target                            !/*se        108*/
!                                                                               !/*se        109*/
  120 continue                                                                  !/*se        110*/
  130 if (ind(22).eq.1.and.ws(31).eq.dp0) go to 240                             !/*se        111*/
!                                                                               !/*se        112*/
!  "virgin" projectile particles are frozen in the projectile                   !/*se        113*/
!  i.e. they are moving with the projectile velocity.                           !/*se        114*/
!  so are particles allowed to fall below fermi sea (y. yariv 12/29/86)         !/*se        115*/
!                                                                               !/*se        116*/
      if (ws(31).eq.5.d+02) go to 140                                           !/*se        117*/
      if (ind(22).eq.1) go to 140                                               !/*se        118*/
      if (ws(2).gt.5.1d0.or.ws(31).le.dp0) go to 150                            !/*se        119*/
!                                                                               !/*se        120*/
  140 ws(20)=voc*ws(16)                                                         !/*se        121*/
      ws(12)=ws(12)+dp10*ws(20)                                                 !/*se        122*/
      ws(25)=ws(10)**2+ws(11)**2+ws(12)**2                                      !/*se        123*/
      ws(14)=sqrt(ws(25))                                                       !/*se        124*/
      if (ws(14).gt.csa(9).and.istept.gt.0) ind(34)=2                           !/*se        125*/
      go to 240                                                                 !/*se        126*/
!                                                                               !/*se        127*/
  150 if (ws(5).gt.0) go to 160                                                 !/*se        128*/
      kcan=1                                                                    !/*se        129*/
      lkcan='selpt'                                                             !/*se        130*/
      go to 240                                                                 !/*se        131*/
!                                                                               !/*se        132*/
  160 ws(20)=(sqrt(ws(5)**2-ws(4)**2)/ws(5))*ws(16)                             !/*se        133*/
      gdc9=dp10*ws(20)                                                          !/*se        134*/
      if (ws(31).ne.dp0) go to 170                                              !/*se        135*/
      if (istept.eq.1) then                                                     !/*se        136*/
        glim=xa(2)-xa(3)                                                        !/*se        137*/
      elseif (istept.ge.ieght-1.or.xa(istept+2).eq.dp0) then                    !/*se        138*/
        glim=xa(istept-1)-xa(istept)                                            !/*se        139*/
      else                                                                      !/*se        140*/
        glim=min(xa(istept-1)-xa(istept),xa(istept+1)-xa(istept+2))             !/*se        141*/
      endif                                                                     !/*se        142*/
      gdc9=min(gdc9,glim)                                                       !/*se        143*/
      ws(20)=0.1d0*gdc9                                                         !/*se        144*/
  170 continue                                                                  !/*se        145*/
      ws(10)=gdc9*ws(6)+ws(10)                                                  !/*se        146*/
      ws(11)=gdc9*ws(7)+ws(11)                                                  !/*se        147*/
      ws(12)=gdc9*ws(8)+ws(12)                                                  !/*se        148*/
      ws(25)=ws(10)**2+ws(11)**2+ws(12)**2                                      !/*se        149*/
      ws(14)=sqrt(ws(25))                                                       !/*se        150*/
      xcc=ws(10)-proj(5)                                                        !/*se        151*/
      ycc=ws(11)-proj(6)                                                        !/*se        152*/
      zcc=(ws(12)-proj(7))/gama                                                 !/*se        153*/
      arg=sqrt(xcc*xcc+ycc*ycc+zcc*zcc)                                         !/*se        154*/
      dxc=xcc-xc                                                                !/*se        155*/
      dyc=ycc-yc                                                                !/*se        156*/
      dzc=zcc-zc                                                                !/*se        157*/
!-----wspr(31) is the distance traversed in projectile (y.y. 7/31/84)           !/*se        158*/
      wspr(31)=sqrt(dxc*dxc+dyc*dyc+dzc*dzc)/dp10                               !/*se        159*/
!  check whether there is a boundary crossing                                   !/*se        160*/
      if (ws(31).eq.dp0) go to 180                                              !/*se        161*/
      call look (arg,ya,istpp)                                                  !/*se        162*/
      if (csa(1).ne.dp0) istpp=0                                                !/*se        163*/
      if (istpp.ne.istepp) ind(8)=1                                             !/*se        164*/
      idstp=abs(istpp-istepp)                                                   !/*se        165*/
      if (idstp.ne.0) ind(8)=1                                                  !/*se        166*/
      if (idstp.le.1) go to 190                                                 !/*se        167*/
      if (mprin.eq.0) mprin=-1                                                  !/*se        168*/
      call writem (istepp,istpp,ws,2)                                           !/*se        169*/
      go to 190                                                                 !/*se        170*/
  180 call look (ws(14),xa,istpt)                                               !/*se        171*/
      idstp=abs(istpt-istept)                                                   !/*se        172*/
      if (idstp.ne.0) ind(8)=1                                                  !/*se        173*/
      if (idstp.le.1) go to 190                                                 !/*se        174*/
      if (mprin.eq.0) mprin=-1                                                  !/*se        175*/
      call writem (istept,istpt,ws,1)                                           !/*se        176*/
!  check whether the particle leaves projectile (ind(34)=1),                    !/*se        177*/
!  the target (ind(34)=2), or the system (ind(34)=3).                           !/*se        178*/
!  once a particle left the target it is not allowed                            !/*se        179*/
!  to reenter it, and if it tries set ind(34)=dp3                               !/*se        180*/
  190 if (csa(1).ne.dp0.and.istpt.eq.0) go to 200                               !/*se        181*/
      if (arg.lt.proj(9)) go to 220                                             !/*se        182*/
      if (ws(14).lt.csa(9).and.istept.gt.0) go to 210                           !/*se        183*/
  200 ind(34)=3                                                                 !/*se        184*/
      go to 240                                                                 !/*se        185*/
!                                                                               !/*se        186*/
  210 if (istepp.eq.0) go to 240                                                !/*se        187*/
      ind(34)=1                                                                 !/*se        188*/
      go to 240                                                                 !/*se        189*/
!                                                                               !/*se        190*/
  220 if (ws(14).gt.csa(9)) go to 230                                           !/*se        191*/
      if (istept.eq.0) go to 200                                                !/*se        192*/
      go to 240                                                                 !/*se        193*/
!                                                                               !/*se        194*/
  230 if (istept.eq.0) go to 240                                                !/*se        195*/
      ind(34)=2                                                                 !/*se        196*/
!                                                                               !/*se        197*/
  240 return                                                                    !/*se        198*/
!                                                                               !/*se        199*/
      end                                                                       !/*se        200*/

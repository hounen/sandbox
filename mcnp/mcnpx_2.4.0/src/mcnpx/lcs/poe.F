!     $Id: poe.F,v 1.4 2002/03/27 18:15:16 gwm Exp $                          !/*po          1*/
      subroutine poe                                                            !/*po          2*/
#include <lcs/double.h>                                                         /*!po          3*/
!     ==================================================================        !/*po          4*/
!     poe calculates the point of entry                                         !/*po          5*/
!                                                                               !/*po          6*/
! POE    calls   POENEW  REGB  REFL                                             !/*po          7*/
! called by ISABEL                                                              !/*po          8*/
!     ==================================================================        !/*po          9*/
!                                                                               !/*po         10*/
#include <lcs/inout.h>                                                          /*!po         11*/
#include <lcs/vegas.h>                                                          /*!po         12*/
! csa(12) contains the running sum of cascades that are calculated.             !/*po         13*/
!                                                                               !/*po         14*/
      csa(12)=csa(12)+dp1                                                       !/*po         15*/
!                                                                               !/*po         16*/
!  initiates the counters, icsb(i), and zero most of the indicators,            !/*po         17*/
!  ind(i).                                                                      !/*po         18*/
!                                                                               !/*po         19*/
      do 10 i=1,20                                                              !/*po         20*/
   10 ind(i)=0                                                                  !/*po         21*/
      ind(3)=1                                                                  !/*po         22*/
      ind(22)=0                                                                 !/*po         23*/
      ind(34)=0                                                                 !/*po         24*/
      nrep=0                                                                    !/*po         25*/
      iexc=1                                                                    !/*po         26*/
      do 20 ii=1,21                                                             !/*po         27*/
      wsp(ii)=dp0                                                               !/*po         28*/
   20 wspr(ii)=dp0                                                              !/*po         29*/
      wsp(31)=dp0                                                               !/*po         30*/
      wspr(31)=dp0                                                              !/*po         31*/
      ex=dp0                                                                    !/*po         32*/
      expr=dp0                                                                  !/*po         33*/
      ixa(1)=csa(3)                                                             !/*po         34*/
      ixq(1)=csa(4)                                                             !/*po         35*/
      if (csa(1).eq.0) then                                                     !/*po         36*/
        ixa(2)=proj(3)                                                          !/*po         37*/
        ixq(2)=proj(4)                                                          !/*po         38*/
      else                                                                      !/*po         39*/
        ixa(2)=0                                                                !/*po         40*/
        ixq(2)=0                                                                !/*po         41*/
      endif                                                                     !/*po         42*/
      do 30 i=1,6                                                               !/*po         43*/
      pit(i)=dp0                                                                !/*po         44*/
   30 pir(i)=dp0                                                                !/*po         45*/
      call poenew                                                               !/*po         46*/
      if (ind(7).eq.1) go to 70                                                 !/*po         47*/
      ind(2)=1                                                                  !/*po         48*/
      i=abs(ws(3))                                                              !/*po         49*/
      ws(4)=amass(i)                                                            !/*po         50*/
      refra6=dp0                                                                !/*po         51*/
      dele=dp0                                                                  !/*po         52*/
      csa11=csa(11)                                                             !/*po         53*/
      icsa1=csa(1)                                                              !/*po         54*/
      if (icsa1.eq.0) go to 110                                                 !/*po         55*/
!                                                                               !/*po         56*/
      istept=0                                                                  !/*po         57*/
      istpt=1                                                                   !/*po         58*/
      if (icsa1.gt.0) go to 40                                                  !/*po         59*/
      icsa1p=-icsa1                                                             !/*po         60*/
      dele=ef(icsa1p,1)*vap                                                     !/*po         61*/
      ws(24)=dele                                                               !/*po         62*/
      go to 60                                                                  !/*po         63*/
   40 if (icsa1.gt.2) go to 50                                                  !/*po         64*/
      dele=ef(icsa1,1)+csa(5)                                                   !/*po         65*/
      ws(24)=ef(icsa1,1)                                                        !/*po         66*/
      go to 60                                                                  !/*po         67*/
   50 ws(24)=dp0                                                                !/*po         68*/
      if (icsa1.eq.3) dele=vpion-csa11                                          !/*po         69*/
!----- presumed bug fix                                                         !/*po         70*/
      if (icsa1.eq.4) dele=vpion                                                !/*po         71*/
      if (icsa1.eq.5) dele=vpion+csa11                                          !/*po         72*/
   60 if (icsa1.ne.3.and.icsa1.ne.1) go to 80                                   !/*po         73*/
      if ((csa(2)-ws(4)-csa11).gt.0) go to 80                                   !/*po         74*/
      write (io,150)                                                            !/*po         75*/
      write (io,130) (ws(i),i=1,14)                                             !/*po         76*/
      write (io,140) (csa(i),i=1,14)                                            !/*po         77*/
   70 continue                                                                  !/*po         78*/
      ind(3)=1                                                                  !/*po         79*/
      ind(7)=1                                                                  !/*po         80*/
      return                                                                    !/*po         81*/
   80 if (csa(3).le.dp1) go to 100                                              !/*po         82*/
      if (icsa1.le.-1) go to 90                                                 !/*po         83*/
      ind21=ind(21)                                                             !/*po         84*/
!       for model without refraction skip refraction at the surface             !/*po         85*/
!       of the nucleus.                                                         !/*po         86*/
      if (ind21.eq.3) go to 100                                                 !/*po         87*/
      if (ind21.eq.6) go to 100                                                 !/*po         88*/
!                                                                               !/*po         89*/
   90 ind(8)=1                                                                  !/*po         90*/
      istpt=1                                                                   !/*po         91*/
      call regb (dist)                                                          !/*po         92*/
      call refl (0,dele,csa11,iref)                                             !/*po         93*/
!     ws(31) = ws31                                                             !/*po         94*/
      go to 110                                                                 !/*po         95*/
  100 ws(5)=ws(5)+dele                                                          !/*po         96*/
  110 ws(21)=dp0                                                                !/*po         97*/
      ws(32)=dp0                                                                !/*po         98*/
      icsb(1)=1                                                                 !/*po         99*/
      icsb(2)=1                                                                 !/*po        100*/
      icsb(3)=0                                                                 !/*po        101*/
      icsb(4)=0                                                                 !/*po        102*/
      icsb(5)=i81                                                               !/*po        103*/
      icsb(7)=0                                                                 !/*po        104*/
      icsb(9)=22                                                                !/*po        105*/
      icsb(11)=0                                                                !/*po        106*/
      icsb(16)=0                                                                !/*po        107*/
      icsb(15)=0                                                                !/*po        108*/
      icsb(6)=icsb(7)*i81                                                       !/*po        109*/
      icsb(8)=icsb(15)*i81                                                      !/*po        110*/
      icsb(10)=icsb(11)*icsb(9)                                                 !/*po        111*/
      icsb(12)=icsb(16)*icsb(9)                                                 !/*po        112*/
      icsb(13)=icsb(6)                                                          !/*po        113*/
      ws(13)=csa(14)                                                            !/*po        114*/
      ws(16)=csa(14)                                                            !/*po        115*/
      ws(17)=csa(14)                                                            !/*po        116*/
      ws(18)=csa(14)                                                            !/*po        117*/
      ws(19)=csa(14)                                                            !/*po        118*/
      ws(15)=dp0                                                                !/*po        119*/
      sel(1)=csa(4)                                                             !/*po        120*/
      sel(2)=csa(3)-csa(4)                                                      !/*po        121*/
      nnt=0                                                                     !/*po        122*/
      nnp=0                                                                     !/*po        123*/
!                                                                               !/*po        124*/
      poe1=poe1+dp1                                                             !/*po        125*/
      ws(1)=poe1                                                                !/*po        126*/
      ws(2)=dp5                                                                 !/*po        127*/
      do 120 i=1,14                                                             !/*po        128*/
  120 st(i)=ws(i)                                                               !/*po        129*/
      st(i81)=ws(31)                                                            !/*po        130*/
! for the incident particle st(17),st(21) are zero and st(18),st(19),           !/*po        131*/
!  st(20) are the x,y,z, of the point of entry.                                 !/*po        132*/
      st(16)=ws(24)                                                             !/*po        133*/
      st(18)=ws(10)                                                             !/*po        134*/
      st(19)=ws(11)                                                             !/*po        135*/
      st(20)=ws(12)                                                             !/*po        136*/
      st(17)=dp0                                                                !/*po        137*/
      st(21)=dp0                                                                !/*po        138*/
      return                                                                    !/*po        139*/
!                                                                               !/*po        140*/
  130 format (' ws:',1p7e13.5)                                                  !/*po        141*/
  140 format (' csa:',1p7e13.5)                                                 !/*po        142*/
  150 format ('1 insufficient energy to penetrate the coulomb barrier')         !/*po        143*/
      end                                                                       !/*po        144*/

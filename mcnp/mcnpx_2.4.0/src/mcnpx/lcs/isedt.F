!     $Id: isedt.F,v 1.4 2002/03/27 18:15:09 gwm Exp $                        !/*is          1*/
      subroutine isedt (w)                                                      !/*is          2*/
!     ==================================================================        !/*is          3*/
!     edit does three things.                                                   !/*is          4*/
!     (1)whenever a particle escapes, its identity, energy and cosine           !/*is          5*/
!        with respect to the z-axis are stored for later use.                   !/*is          6*/
!     (2)when a cascade is over edit does some partial editing                  !/*is          7*/
!        such as calculating zae.                                               !/*is          8*/
!     (3)when all the cascades have been calculated, a final editing is         !/*is          9*/
!        done.                                                                  !/*is         10*/
!                                                                               !/*is         11*/
! ISEDT  calls   IGETD  ISLINK                                                  !/*is         12*/
! called by ISABEL  AIOUT  RLAR  CASCAS  GAMMA  PSEL                            !/*is         13*/
!     ==================================================================        !/*is         14*/
#include <lcs/double.h>                                                         /*!is         15*/
#include <lcs/inout.h>                                                          /*!is         16*/
#include <lcs/vegas.h>                                                          /*!is         17*/
      dimension w(32)                                                           !/*is         18*/
!                                                                               !/*is         19*/
! when icsb(14)=0, statement 2 starts final editing.                            !/*is         20*/
!                                                                               !/*is         21*/
! ind(7)=1 when a cascade is finished.  start partial  editing.                 !/*is         22*/
!                                                                               !/*is         23*/
      if (ind(7).eq.1) go to 70                                                 !/*is         24*/
!                                                                               !/*is         25*/
! ind(3)=1 when there has been no interaction (transparency).                   !/*is         26*/
!                                                                               !/*is         27*/
      if (ind(3).eq.1) go to 50                                                 !/*is         28*/
!                                                                               !/*is         29*/
!  store the identity, energy and cosine w.r.t. z-axis of                       !/*is         30*/
!  the escaping particle into aa for later use.                                 !/*is         31*/
!                                                                               !/*is         32*/
   10 if (istept.eq.dp0) go to 40                                               !/*is         33*/
      ws(21)=ws(21)+dp1                                                         !/*is         34*/
      ma=ws(21)                                                                 !/*is         35*/
      if (ma.le.360) go to 30                                                   !/*is         36*/
   20 kcan=3                                                                    !/*is         37*/
      lkcan='isedt 1'                                                           !/*is         38*/
      return                                                                    !/*is         39*/
   30 aa(ma)=w(3)                                                               !/*is         40*/
      if (ind(34).eq.2) return                                                  !/*is         41*/
   40 mb=9.d0*ws(32)                                                            !/*is         42*/
      if (mb.gt.3600) go to 20                                                  !/*is         43*/
      if ((w(5)-w(4)).lt.dp0) then                                              !/*is         44*/
        kcan=1                                                                  !/*is         45*/
        lkcan='isedt 4'                                                         !/*is         46*/
        return                                                                  !/*is         47*/
      endif                                                                     !/*is         48*/
      ws(32)=ws(32)+dp1                                                         !/*is         49*/
      bb(mb+1)=w(3)                                                             !/*is         50*/
      bb(mb+4)=w(4)                                                             !/*is         51*/
      bb(mb+5)=w(6)                                                             !/*is         52*/
      bb(mb+6)=w(7)                                                             !/*is         53*/
      bb(mb+2)=w(5)-w(4)                                                        !/*is         54*/
      bb(mb+3)=w(8)                                                             !/*is         55*/
      bb(mb+7)=w(10)                                                            !/*is         56*/
      bb(mb+8)=w(11)                                                            !/*is         57*/
      bb(mb+9)=w(12)                                                            !/*is         58*/
      return                                                                    !/*is         59*/
!                                                                               !/*is         60*/
! for transparencies there is no partial editing.                               !/*is         61*/
   50 if (npout.gt.0.or.icsb(1).gt.0) go to 10                                  !/*is         62*/
   60 continue                                                                  !/*is         63*/
      transp=transp+dp1                                                         !/*is         64*/
      icsb(14)=icsb(14)+1                                                       !/*is         65*/
      nprts=-1                                                                  !/*is         66*/
      go to 260                                                                 !/*is         67*/
   70 if (ind(3).eq.1) go to 60                                                 !/*is         68*/
!                                                                               !/*is         69*/
! for incident nucleons the intiial excitation energy is the sum of in-         !/*is         70*/
!  cident kinetic energy and binding energy.  for incident pions the            !/*is         71*/
!  initial excitation energy is just the kinetic energy.                        !/*is         72*/
!                                                                               !/*is         73*/
      do 80 i=1,11                                                              !/*is         74*/
   80 na(i)=0                                                                   !/*is         75*/
      if (csa(1).eq.0) then                                                     !/*is         76*/
        ia=npmon(3)                                                             !/*is         77*/
        iq=csa(4)+npmon(1)                                                      !/*is         78*/
      else                                                                      !/*is         79*/
        ia=csa(3)+dp1                                                           !/*is         80*/
        iq=csa(4)                                                               !/*is         81*/
        if (csa(1).eq.dp1.or.csa(1).eq.dp3.or.csa(1).eq.7.d0) iq=iq+1           !/*is         82*/
        if (csa(1).eq.-dp1.or.csa(1).eq.dp5.or.csa(1).eq.9.d0) iq=iq-1          !/*is         83*/
        if (csa(1).gt.dp2) ia=ia-1                                              !/*is         84*/
        if (csa(1).le.-dp1) ia=ia-1+npmon(1)+npmon(2)                           !/*is         85*/
      endif                                                                     !/*is         86*/
      if (ws(21).eq.dp0) go to 210                                              !/*is         87*/
! the z, a, ex of residual nucleus are calculated.                              !/*is         88*/
      m=ws(21)                                                                  !/*is         89*/
      do 200 i=1,m                                                              !/*is         90*/
      k=aa(i)                                                                   !/*is         91*/
      if (k+1) 200,90,100                                                       !/*is         92*/
   90 iq=iq+1                                                                   !/*is         93*/
      go to 200                                                                 !/*is         94*/
  100 if (k-2) 110,120,130                                                      !/*is         95*/
  110 iq=iq-1                                                                   !/*is         96*/
  120 ia=ia-1                                                                   !/*is         97*/
      go to 200                                                                 !/*is         98*/
  130 if (k-4) 140,200,150                                                      !/*is         99*/
  140 iq=iq-1                                                                   !/*is        100*/
      go to 200                                                                 !/*is        101*/
  150 if (k.gt.9) go to 180                                                     !/*is        102*/
      if (k.eq.5) go to 170                                                     !/*is        103*/
      if (k-8) 160,200,170                                                      !/*is        104*/
  160 iq=iq-1                                                                   !/*is        105*/
      go to 200                                                                 !/*is        106*/
  170 iq=iq+1                                                                   !/*is        107*/
      go to 200                                                                 !/*is        108*/
  180 ia=ia-1                                                                   !/*is        109*/
      if (k.ge.24) go to 190                                                    !/*is        110*/
      iq=iq-1                                                                   !/*is        111*/
      if (k.eq.13) iq=iq-1                                                      !/*is        112*/
      go to 200                                                                 !/*is        113*/
  190 if (k.eq.25) iq=iq+1                                                      !/*is        114*/
  200 continue                                                                  !/*is        115*/
  210 continue                                                                  !/*is        116*/
      if (ws(32).eq.dp0) go to 230                                              !/*is        117*/
      m=9.d0*ws(32)-8.d0                                                        !/*is        118*/
      do 220 i=1,m,9                                                            !/*is        119*/
      kn=bb(i)                                                                  !/*is        120*/
      if (kn.gt.9) go to 220                                                    !/*is        121*/
      if (kn.le.0) kn=9-kn                                                      !/*is        122*/
      na(kn)=na(kn)+1                                                           !/*is        123*/
  220 continue                                                                  !/*is        124*/
!     no. of charged particles                                                  !/*is        125*/
      na(6)=na(1)+na(3)+na(5)+na(7)+na(9)+na(10)                                !/*is        126*/
  230 if (csa(1).eq.dp0) go to 240                                              !/*is        127*/
      iap=0                                                                     !/*is        128*/
      izp=0                                                                     !/*is        129*/
      go to 250                                                                 !/*is        130*/
!                                                                               !/*is        131*/
!   izp,iap are # protons, nucleons remaining in projectile                     !/*is        132*/
!                                                                               !/*is        133*/
  240 izp=csa(4)+proj(4)-iq-na(1)-na(3)+na(5)-na(7)+na(9)+na(10)                !/*is        134*/
      iap=csa(3)+proj(3)-ia-na(1)-na(2)-na(10)-na(11)                           !/*is        135*/
  250 continue                                                                  !/*is        136*/
      mb=ws(32)                                                                 !/*is        137*/
!-----------------------------------------------------------------------        !/*is        138*/
! test for consistency                                                          !/*is        139*/
      if (izp.lt.0.or.izp.gt.iap.or.ixq(2).lt.0.or.ixq(2).gt.ixa(2))            !/*is        140*/
     & then                                                                     !/*is        141*/
        kcan=1                                                                  !/*is        142*/
        write (io,280)                                                          !/*is        143*/
      endif                                                                     !/*is        144*/
      if (iq.lt.0.or.iq.gt.ia.or.ixq(1).lt.0.or.ixq(1).gt.ixa(1)) then          !/*is        145*/
        kcan=1                                                                  !/*is        146*/
        write (io,290)                                                          !/*is        147*/
      endif                                                                     !/*is        148*/
      if (ixq(2).ne.izp.or.ixa(2).ne.iap.or.ixq(1).ne.iq.or.ixa(1).ne.ia        !/*is        149*/
     & ) kcan=1                                                                 !/*is        150*/
      if (kcan.eq.1) then                                                       !/*is        151*/
        nocas=igetd(1)                                                          !/*is        152*/
        write (io,270) nocas,ia,iq,iap,iap,(ixa(i),ixq(i),i=1,2)                !/*is        153*/
        lkcan='isedt 2'                                                         !/*is        154*/
        return                                                                  !/*is        155*/
      endif                                                                     !/*is        156*/
!-----------------------------------------------------------------------        !/*is        157*/
      nprts=0                                                                   !/*is        158*/
  260 continue                                                                  !/*is        159*/
      call islink (bb,csa,ixa,ixq,kcan,lkcan,mb,nprts,voc,xmasss,ex,expr        !/*is        160*/
     & ,pit,pir)                                                                !/*is        161*/
      return                                                                    !/*is        162*/
!                                                                               !/*is        163*/
  270 format (2x,i6,' ia=',i3,' iq=',i3,' iap=',i3,' izp=',i3,' ixa1='          !/*is        164*/
     & ,i3,' ixq1=',i3,' ixa2=',i3,' ixq2=',i3)                                 !/*is        165*/
  280 format (' projectile negatives ')                                         !/*is        166*/
  290 format (' target negatives ')                                             !/*is        167*/
      end                                                                       !/*is        168*/

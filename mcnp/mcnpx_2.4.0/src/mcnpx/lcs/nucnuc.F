!     $Id: nucnuc.F,v 1.4 2002/03/27 18:15:12 gwm Exp $                       !/*nu          1*/
      subroutine nucnuc (einc,p0,nofas,it,efas,alpfas,betfas,gamfas             !/*nu          2*/
     & ,wtfas,itype)                                                            !/*nu          3*/
! NUCNUC     calls...  ERRORF  PROB  FLTRNF  STOR  PROT  PI  SQRT  AZIRN        !/*nu          4*/
!                      POLRN                                                    !/*nu          5*/
!     NUCNUC called by...   HEH                                                 !/*nu          6*/
#include <lcs/double.h>                                                         /*!nu          7*/
!        no nucleon conservation                                                !/*nu          8*/
      dimension efas(1), it(1), alpfas(1), betfas(1), gamfas(1), wtfas(1        !/*nu          9*/
     & ), pb(5)                                                                 !/*nu         10*/
#include <lcs/inout.h>                                                          /*!nu         11*/
      dimension sp(5)                                                           !/*nu         12*/
      data count /0.d0/, witty /.03d0/, ddd /.15d0/, c45d /.707d0/              !/*nu         13*/
      ntimes=0                                                                  !/*nu         14*/
      go to 20                                                                  !/*nu         15*/
   10 ntimes=ntimes+1                                                           !/*nu         16*/
      write (io,220)                                                            !/*nu         17*/
      if (ntimes.ge.10) call errorf ('nucnuc.1')                                !/*nu         18*/
   20 continue                                                                  !/*nu         19*/
      erem=einc                                                                 !/*nu         20*/
      st0r=einc-0.139d0                                                         !/*nu         21*/
      nofas=0                                                                   !/*nu         22*/
      call prob (itype,einc,p0,pb(1))                                           !/*nu         23*/
      cons=pb(1)+pb(2)                                                          !/*nu         24*/
      sp(1)=pb(1)/cons                                                          !/*nu         25*/
      sp(2)=dp1                                                                 !/*nu         26*/
      cons=pb(3)+pb(4)+pb(5)                                                    !/*nu         27*/
      sp(3)=pb(3)/cons                                                          !/*nu         28*/
      sp(4)=pb(4)/cons+sp(3)                                                    !/*nu         29*/
      sp(5)=dp1                                                                 !/*nu         30*/
      numnuc=0                                                                  !/*nu         31*/
   30 continue                                                                  !/*nu         32*/
      val=rang()                                                                !/*nu         33*/
      do 40 i=1,5                                                               !/*nu         34*/
      if (val.lt.pb(i)) go to 50                                                !/*nu         35*/
   40 continue                                                                  !/*nu         36*/
      call errorf ('nucnuc.2')                                                  !/*nu         37*/
   50 j=1                                                                       !/*nu         38*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*nu         39*/
      if (w.ne.dp0) go to 70                                                    !/*nu         40*/
      w=dp1                                                                     !/*nu         41*/
   60 e=dp0                                                                     !/*nu         42*/
      if (i.gt.2) go to 100                                                     !/*nu         43*/
      call prot (p0,e,p,a,b,g)                                                  !/*nu         44*/
      if (e.gt.st0r) go to 60                                                   !/*nu         45*/
      j=4                                                                       !/*nu         46*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*nu         47*/
      if (i.eq.0) go to 30                                                      !/*nu         48*/
      wmass=dp0                                                                 !/*nu         49*/
   70 erem=erem-e-wmass                                                         !/*nu         50*/
      if (erem.lt.dp0) go to 110                                                !/*nu         51*/
   80 nofas=nofas+1                                                             !/*nu         52*/
      if (nofas.gt.60) go to 10                                                 !/*nu         53*/
      if (i.gt.2) go to 90                                                      !/*nu         54*/
      numnuc=numnuc+1                                                           !/*nu         55*/
      if (numnuc.lt.3) go to 90                                                 !/*nu         56*/
      nofas=nofas-1                                                             !/*nu         57*/
      j=2                                                                       !/*nu         58*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*nu         59*/
      erem=erem+e+wmass                                                         !/*nu         60*/
      go to 30                                                                  !/*nu         61*/
   90 efas(nofas)=e*1.d3                                                        !/*nu         62*/
      it(nofas)=i-1                                                             !/*nu         63*/
      alpfas(nofas)=a                                                           !/*nu         64*/
      betfas(nofas)=b                                                           !/*nu         65*/
      gamfas(nofas)=g                                                           !/*nu         66*/
      wtfas(nofas)=abs(w)                                                       !/*nu         67*/
      if (w.lt.dp0) return                                                      !/*nu         68*/
      go to 30                                                                  !/*nu         69*/
  100 call pi (p0,i,e,p,a,b,g)                                                  !/*nu         70*/
      if (e.gt.st0r) go to 60                                                   !/*nu         71*/
      j=4                                                                       !/*nu         72*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*nu         73*/
      if (i.eq.0) go to 30                                                      !/*nu         74*/
      wmass=0.139d0                                                             !/*nu         75*/
      go to 70                                                                  !/*nu         76*/
  110 erem=erem+e+wmass                                                         !/*nu         77*/
      j=2                                                                       !/*nu         78*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*nu         79*/
      if (numnuc.gt.1) go to 130                                                !/*nu         80*/
      if (numnuc.gt.0) go to 120                                                !/*nu         81*/
      numnuc=numnuc+1                                                           !/*nu         82*/
      val=rang()                                                                !/*nu         83*/
      i=1                                                                       !/*nu         84*/
      if (val.gt.sp(1)) i=2                                                     !/*nu         85*/
      val=rang()                                                                !/*nu         86*/
      e=erem*val                                                                !/*nu         87*/
      erem=erem-e                                                               !/*nu         88*/
      p=sqrt(e*(e+1.88d0))                                                      !/*nu         89*/
      w=dp1                                                                     !/*nu         90*/
      call prot (p0,e,p,a,b,g)                                                  !/*nu         91*/
      j=3                                                                       !/*nu         92*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*nu         93*/
      nofas=nofas+1                                                             !/*nu         94*/
      if (nofas.gt.60) go to 10                                                 !/*nu         95*/
      efas(nofas)=e*1.d3                                                        !/*nu         96*/
      it(nofas)=i-1                                                             !/*nu         97*/
      alpfas(nofas)=a                                                           !/*nu         98*/
      betfas(nofas)=b                                                           !/*nu         99*/
      gamfas(nofas)=g                                                           !/*nu        100*/
      wtfas(nofas)=abs(w)                                                       !/*nu        101*/
  120 continue                                                                  !/*nu        102*/
      val=rang()                                                                !/*nu        103*/
      i=1                                                                       !/*nu        104*/
      if (val.gt.sp(1)) i=2                                                     !/*nu        105*/
      go to 150                                                                 !/*nu        106*/
  130 continue                                                                  !/*nu        107*/
      val=rang()                                                                !/*nu        108*/
      i=3                                                                       !/*nu        109*/
      if (val.gt.sp(3)) i=4                                                     !/*nu        110*/
      if (val.gt.sp(4)) i=5                                                     !/*nu        111*/
      if (erem.gt.(einc*ddd)) go to 140                                         !/*nu        112*/
      val=rang()                                                                !/*nu        113*/
      if (val.lt.witty) go to 200                                               !/*nu        114*/
  140 if (i.gt.2) go to 170                                                     !/*nu        115*/
  150 e=erem                                                                    !/*nu        116*/
      if (e.lt.dp0) call errorf ('nucnuc.3')                                    !/*nu        117*/
      p=sqrt(e*e+1.88d0*e)                                                      !/*nu        118*/
      w=dp1                                                                     !/*nu        119*/
      call prot (p0,e,p,a,b,g)                                                  !/*nu        120*/
  160 j=3                                                                       !/*nu        121*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*nu        122*/
      w=-dp1                                                                    !/*nu        123*/
      go to 80                                                                  !/*nu        124*/
  170 e=erem-0.139d0                                                            !/*nu        125*/
      if (e.lt.dp0) go to 180                                                   !/*nu        126*/
      p=sqrt(e*e+.278d0*e)                                                      !/*nu        127*/
      w=dp1                                                                     !/*nu        128*/
      call pi (p0,i,e,p,a,b,g)                                                  !/*nu        129*/
      go to 160                                                                 !/*nu        130*/
  180 if (erem.lt.dp0) call errorf ('nucnuc.4')                                 !/*nu        131*/
      count=count+dp1                                                           !/*nu        132*/
      temp=erem/nofas                                                           !/*nu        133*/
      temp=temp*1.d+03                                                          !/*nu        134*/
      do 190 i=1,nofas                                                          !/*nu        135*/
  190 efas(i)=efas(i)+temp                                                      !/*nu        136*/
      return                                                                    !/*nu        137*/
  200 if ((i.gt.2).and.(erem.lt.0.139d0)) go to 180                             !/*nu        138*/
      call azirn (csa,sna)                                                      !/*nu        139*/
  210 call polrn (csp,snp)                                                      !/*nu        140*/
      if (csp.gt.c45d) go to 210                                                !/*nu        141*/
      a=snp*csa                                                                 !/*nu        142*/
      b=snp*sna                                                                 !/*nu        143*/
      g=csp                                                                     !/*nu        144*/
      wmass=dp0                                                                 !/*nu        145*/
      if (i.gt.2) wmass=0.139d0                                                 !/*nu        146*/
      e=erem-wmass                                                              !/*nu        147*/
      w=-dp1                                                                    !/*nu        148*/
      go to 80                                                                  !/*nu        149*/
!                                                                               !/*nu        150*/
  220 format ('0',10x,'nofas greater than 60 in nucnuc')                        !/*nu        151*/
      end                                                                       !/*nu        152*/

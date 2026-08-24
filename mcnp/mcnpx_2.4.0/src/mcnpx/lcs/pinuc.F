!     $Id: pinuc.F,v 1.4 2002/03/27 18:15:15 gwm Exp $                        !/*pi          1*/
      subroutine pinuc (einc,p0,nofas,it,efas,alpfas,betfas,gamfas,wtfas        !/*pi          2*/
     & ,itype)                                                                  !/*pi          3*/
! PINUC      calls...  ERRORF  PROB  FLTRNF  STOR  PI  SQRT  AZIRN              !/*pi          4*/
!                      POLRN                                                    !/*pi          5*/
!     PINUC  called by...   HEH                                                 !/*pi          6*/
#include <lcs/double.h>                                                         /*!pi          7*/
      dimension efas(1), it(1), alpfas(1), betfas(1), gamfas(1), wtfas(1        !/*pi          8*/
     & ), pb(5)                                                                 !/*pi          9*/
#include <lcs/inout.h>                                                          /*!pi         10*/
      dimension sp(5)                                                           !/*pi         11*/
      data count /0.d0/, witty /.03d0/, ddd /.15d0/, c45d /.707d0/              !/*pi         12*/
      ntimes=0                                                                  !/*pi         13*/
      go to 20                                                                  !/*pi         14*/
   10 ntimes=ntimes+1                                                           !/*pi         15*/
      write (io,190)                                                            !/*pi         16*/
      if (ntimes.ge.10) call errorf ('pinuc.1')                                 !/*pi         17*/
   20 continue                                                                  !/*pi         18*/
      erem=einc+0.139d0                                                         !/*pi         19*/
      st0r=einc-0.139d0                                                         !/*pi         20*/
      nofas=0                                                                   !/*pi         21*/
      call prob (itype,einc,p0,pb(1))                                           !/*pi         22*/
      cons=pb(1)+pb(2)                                                          !/*pi         23*/
      sp(1)=pb(1)/cons                                                          !/*pi         24*/
      sp(2)=dp1                                                                 !/*pi         25*/
      cons=pb(3)+pb(4)+pb(5)                                                    !/*pi         26*/
      sp(3)=pb(3)/cons                                                          !/*pi         27*/
      sp(4)=pb(4)/cons+sp(3)                                                    !/*pi         28*/
      sp(5)=dp1                                                                 !/*pi         29*/
      numnuc=0                                                                  !/*pi         30*/
   30 continue                                                                  !/*pi         31*/
      val=rang()                                                                !/*pi         32*/
      do 40 i=1,5                                                               !/*pi         33*/
      if (val.lt.pb(i)) go to 50                                                !/*pi         34*/
   40 continue                                                                  !/*pi         35*/
      call errorf ('pinuc.2')                                                   !/*pi         36*/
   50 j=1                                                                       !/*pi         37*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*pi         38*/
      if (w.ne.dp0) go to 70                                                    !/*pi         39*/
      w=dp1                                                                     !/*pi         40*/
      wmass=dp0                                                                 !/*pi         41*/
      if (i.gt.2) wmass=0.139d0                                                 !/*pi         42*/
   60 e=dp0                                                                     !/*pi         43*/
      call pi (p0,i,e,p,a,b,g)                                                  !/*pi         44*/
      if (e.gt.st0r) go to 60                                                   !/*pi         45*/
      j=4                                                                       !/*pi         46*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*pi         47*/
      if (i.eq.0) go to 30                                                      !/*pi         48*/
   70 erem=erem-e-wmass                                                         !/*pi         49*/
      if (erem.lt.dp0) go to 100                                                !/*pi         50*/
   80 nofas=nofas+1                                                             !/*pi         51*/
      if (nofas.gt.60) go to 10                                                 !/*pi         52*/
      if (i.gt.2) go to 90                                                      !/*pi         53*/
      numnuc=numnuc+1                                                           !/*pi         54*/
      if (numnuc.lt.2) go to 90                                                 !/*pi         55*/
      nofas=nofas-1                                                             !/*pi         56*/
      j=2                                                                       !/*pi         57*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*pi         58*/
      erem=erem+e+wmass                                                         !/*pi         59*/
      go to 30                                                                  !/*pi         60*/
   90 efas(nofas)=e*1.d3                                                        !/*pi         61*/
      it(nofas)=i-1                                                             !/*pi         62*/
      alpfas(nofas)=a                                                           !/*pi         63*/
      betfas(nofas)=b                                                           !/*pi         64*/
      gamfas(nofas)=g                                                           !/*pi         65*/
      wtfas(nofas)=abs(w)                                                       !/*pi         66*/
      if (w.lt.dp0) return                                                      !/*pi         67*/
      go to 30                                                                  !/*pi         68*/
  100 erem=erem+e+wmass                                                         !/*pi         69*/
      j=2                                                                       !/*pi         70*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*pi         71*/
      if (numnuc.gt.0) go to 110                                                !/*pi         72*/
      val=rang()                                                                !/*pi         73*/
      i=1                                                                       !/*pi         74*/
      if (val.gt.sp(1)) i=2                                                     !/*pi         75*/
      e=erem                                                                    !/*pi         76*/
      p=sqrt(e*(e+1.88d0))                                                      !/*pi         77*/
      go to 140                                                                 !/*pi         78*/
  110 continue                                                                  !/*pi         79*/
      val=rang()                                                                !/*pi         80*/
      i=3                                                                       !/*pi         81*/
      if (val.gt.sp(3)) i=4                                                     !/*pi         82*/
      if (val.gt.sp(4)) i=5                                                     !/*pi         83*/
      if (erem.gt.(einc*ddd)) go to 120                                         !/*pi         84*/
      val=rang()                                                                !/*pi         85*/
      if (val.lt.witty) go to 170                                               !/*pi         86*/
  120 if ((erem.lt.0.139d0).and.(i.gt.2)) go to 150                             !/*pi         87*/
      wmass=dp0                                                                 !/*pi         88*/
      if (i.gt.2) wmass=0.139d0                                                 !/*pi         89*/
      e=erem-wmass                                                              !/*pi         90*/
      if (e.lt.dp0) call errorf ('pinuc.3')                                     !/*pi         91*/
      if (i.gt.2) go to 130                                                     !/*pi         92*/
      p=sqrt(e*e+1.88d0*e)                                                      !/*pi         93*/
      go to 140                                                                 !/*pi         94*/
  130 p=sqrt(e*e+.278d0*e)                                                      !/*pi         95*/
  140 w=dp1                                                                     !/*pi         96*/
      call pi (p0,i,e,p,a,b,g)                                                  !/*pi         97*/
      j=3                                                                       !/*pi         98*/
      call stor (p0,i,e,a,b,g,w,j,erem,p,wmass,itype)                           !/*pi         99*/
      w=-dp1                                                                    !/*pi        100*/
      go to 80                                                                  !/*pi        101*/
  150 if (erem.lt.dp0) call errorf ('pinuc.4')                                  !/*pi        102*/
      count=count+dp1                                                           !/*pi        103*/
      temp=(erem/nofas)*1.d+03                                                  !/*pi        104*/
      do 160 i=1,nofas                                                          !/*pi        105*/
  160 efas(i)=efas(i)+temp                                                      !/*pi        106*/
      return                                                                    !/*pi        107*/
  170 if ((i.gt.2).and.(erem.lt.0.139d0)) go to 150                             !/*pi        108*/
      call azirn (csa,sna)                                                      !/*pi        109*/
  180 call polrn (csp,snp)                                                      !/*pi        110*/
      if (csp.gt.c45d) go to 180                                                !/*pi        111*/
      a=snp*csa                                                                 !/*pi        112*/
      b=snp*sna                                                                 !/*pi        113*/
      g=csp                                                                     !/*pi        114*/
      wmass=dp0                                                                 !/*pi        115*/
      if (i.gt.2) wmass=0.139d0                                                 !/*pi        116*/
      e=erem-wmass                                                              !/*pi        117*/
      w=-dp1                                                                    !/*pi        118*/
      go to 80                                                                  !/*pi        119*/
!                                                                               !/*pi        120*/
  190 format ('0',10x,'nofas greater than 60 in pinuc')                         !/*pi        121*/
      end                                                                       !/*pi        122*/

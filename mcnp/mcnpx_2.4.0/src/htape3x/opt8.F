!     $Id: opt8.F,v 1.4 2002/03/27 18:14:02 gwm Exp $                         !/*op          1*/
      subroutine opt8 (tlly,hold,tlsq,mxz0,mxn0)                                !/*op          2*/
!cc                                                                             !/*op          3*/
!                                                                               !/*op          4*/
!ccc                                                                            !/*op          5*/
!  options:                                                                     !/*op          6*/
!cccc                                                                           !/*op          7*/
!     kopt = 0 - distribution after evaporation and fission                     !/*op          8*/
!     kopt = 1 - distribution before evaporation and fission                    !/*op          9*/
!                                                                               !/*op         10*/
#include <htape3x/com32b.h>                                                     /*!op         11*/
#include <htape3x/label.h>                                                      /*!op         12*/
#include <htape3x/contrl.h>                                                     /*!op         13*/
#include <htape3x/big.h>                                                        /*!op         14*/
#include <htape3x/big4.h>                                                       /*!op         15*/
      dimension hold(mxn0,mxz0,1), tlly(mxn0,mxz0,1), tlsq(mxn0,mxz0,1)         !/*op         16*/
      dimension nc4(8)                                                          !/*op         17*/
      dimension dela(lpt), delz(lpt)                                            !/*op         18*/
      data dela /2*dp1,5*dp0,dp2,2*dp3,dp4,5*dp0,2*-1.d0,4*dp0/                 !/*op         19*/
      data delz /dp1,dp0,dp1,dp0,-1.d0,dp1,-1.d0,2*dp1,2*dp2,dp0,dp1,2          !/*op         20*/
     & *dp0,2*-1.d0,dp0,-1.d0,dp1,2*dp0/                                        !/*op         21*/
      data nc4 /0,1,0,0,0,1,0,0/                                                !/*op         22*/
      if (ncol.ne.2.and.ncol.ne.6) go to 120                                    !/*op         23*/
   10 continue                                                                  !/*op         24*/
      if (lelem.eq.-1) go to 120                                                !/*op         25*/
      if (ifeng.and.(name.ne.1)) go to 120                                      !/*op         26*/
      if (nparm.le.0) go to 30                                                  !/*op         27*/
      if (jmat.eq.0) icomp=bold                                                 !/*op         28*/
      if (jmat.ne.0) icomp=mat                                                  !/*op         29*/
      do 20 ib=1,nparm                                                          !/*op         30*/
      if (lparm(ib).eq.icomp) go to 40                                          !/*op         31*/
   20 continue                                                                  !/*op         32*/
      go to 120                                                                 !/*op         33*/
   30 continue                                                                  !/*op         34*/
      ib=1                                                                      !/*op         35*/
   40 continue                                                                  !/*op         36*/
      if (lbunch) ib=1                                                          !/*op         37*/
      if (kopt.eq.2.and.mnuc.le.1) go to 120                                    !/*op         38*/
      if (mnuc.eq.1.or.mnuc.eq.2) then                                          !/*op         39*/
        apr2=aprr(1)                                                            !/*op         40*/
        zpr2=zprr(1)                                                            !/*op         41*/
        uur2=uur(1)                                                             !/*op         42*/
      else                                                                      !/*op         43*/
        apr2=dp0                                                                !/*op         44*/
        zpr2=dp0                                                                !/*op         45*/
      endif                                                                     !/*op         46*/
      if (mnuc.eq.2) then                                                       !/*op         47*/
        apr1=aprr(2)                                                            !/*op         48*/
        zpr1=zprr(2)                                                            !/*op         49*/
        uur1=uur(2)                                                             !/*op         50*/
      else                                                                      !/*op         51*/
        apr1=dp0                                                                !/*op         52*/
        zpr1=dp0                                                                !/*op         53*/
      endif                                                                     !/*op         54*/
      if (kopt.eq.0) go to 80                                                   !/*op         55*/
      apr2=apr2+apr1                                                            !/*op         56*/
      zpr2=zpr2+zpr1                                                            !/*op         57*/
      apr1=dp0                                                                  !/*op         58*/
      zpr1=dp0                                                                  !/*op         59*/
      if (nabov.eq.0) go to 60                                                  !/*op         60*/
      do 50 i=1,nabov                                                           !/*op         61*/
      itp=tipa(i)+dp1                                                           !/*op         62*/
      if (jtipa(i).eq.6.or.jtipa(i).eq.7) then                                  !/*op         63*/
        apr2=apr2+dela(itp)                                                     !/*op         64*/
        zpr2=zpr2+delz(itp)                                                     !/*op         65*/
      endif                                                                     !/*op         66*/
      if (kopt.eq.2) go to 50                                                   !/*op         67*/
      if (jtipa(i).eq.5) then                                                   !/*op         68*/
!     if (jtipa(i).eq.2.or.jtipa(i).eq.5) then                                  !/*op         69*/
        apr2=apr2+dela(itp)                                                     !/*op         70*/
        zpr2=zpr2+delz(itp)                                                     !/*op         71*/
      endif                                                                     !/*op         72*/
   50 continue                                                                  !/*op         73*/
   60 continue                                                                  !/*op         74*/
      if (nbelo.eq.0) go to 80                                                  !/*op         75*/
      do 70 i=1,nbelo                                                           !/*op         76*/
      itp=tipb(i)+dp1                                                           !/*op         77*/
      if (jtipb(i).eq.6.or.jtipb(i).eq.7) then                                  !/*op         78*/
        apr2=apr2+dela(itp)                                                     !/*op         79*/
        zpr2=zpr2+delz(itp)                                                     !/*op         80*/
      endif                                                                     !/*op         81*/
      if (kopt.eq.2) go to 70                                                   !/*op         82*/
      if (jtipb(i).eq.5) then                                                   !/*op         83*/
!     if (jtipb(i).eq.2.or.jtipb(i).eq.5) then                                  !/*op         84*/
        apr2=apr2+dela(itp)                                                     !/*op         85*/
        zpr2=zpr2+delz(itp)                                                     !/*op         86*/
      endif                                                                     !/*op         87*/
   70 continue                                                                  !/*op         88*/
   80 continue                                                                  !/*op         89*/
      iz=zpr2                                                                   !/*op         90*/
      ia=apr2                                                                   !/*op         91*/
      if (ia.eq.0.and.iz.eq.0) go to 120                                        !/*op         92*/
      inn=ia-iz+1                                                               !/*op         93*/
      iz=iz+1                                                                   !/*op         94*/
      if (iz.le.0.or.inn.le.0) go to 130                                        !/*op         95*/
!-----                                                                          !/*op         96*/
      if (kopt.ne.0.or.ipht.eq.0) go to 110                                     !/*op         97*/
      jz=iz-1                                                                   !/*op         98*/
      k1=iref(jz)                                                               !/*op         99*/
      k2=iref(jz+1)-1                                                           !/*op        100*/
      if (k2.ge.k1) then                                                        !/*op        101*/
        izaid=1000*jz+ia                                                        !/*op        102*/
        do 100 k=k1,k2                                                          !/*op        103*/
        if (izaid.eq.izao(k)) then                                              !/*op        104*/
          wtfin0(k)=wtfin0(k)+wt                                                !/*op        105*/
          if (uur2.le.dp0) go to 110                                            !/*op        106*/
          ip1=ipt(k)                                                            !/*op        107*/
          ip2=ipt(k+1)-1                                                        !/*op        108*/
          if (ip2.ge.ip1) then                                                  !/*op        109*/
            do 90 ip=ip1,ip2                                                    !/*op        110*/
            if (abs(uur2-elo(ip)).le.0.00001d0) then                            !/*op        111*/
              wtfin(ip)=wtfin(ip)+wt                                            !/*op        112*/
              go to 110                                                         !/*op        113*/
            endif                                                               !/*op        114*/
   90       continue                                                            !/*op        115*/
            write (*,*) 'metastable state not tabulated: jz,ia,ex ',jz          !/*op        116*/
     &      ,ia,uur2                                                            !/*op        117*/
            go to 110                                                           !/*op        118*/
          endif                                                                 !/*op        119*/
        endif                                                                   !/*op        120*/
  100   continue                                                                !/*op        121*/
      endif                                                                     !/*op        122*/
  110 continue                                                                  !/*op        123*/
!-----                                                                          !/*op        124*/
      if (iz.ge.mxz0.or.inn.ge.mxn0) go to 140                                  !/*op        125*/
      hold(inn,iz,ib)=hold(inn,iz,ib)+wt                                        !/*op        126*/
      hold(mxn0,iz,ib)=hold(mxn0,iz,ib)+wt                                      !/*op        127*/
      hold(inn,mxz0,ib)=hold(inn,mxz0,ib)+wt                                    !/*op        128*/
      hold(mxn0,mxz0,ib)=hold(mxn0,mxz0,ib)+wt                                  !/*op        129*/
      ia1=apr1                                                                  !/*op        130*/
      if (ia1.eq.0) go to 120                                                   !/*op        131*/
      apr2=apr1                                                                 !/*op        132*/
      zpr2=zpr1                                                                 !/*op        133*/
      uur2=uur1                                                                 !/*op        134*/
      apr1=dp0                                                                  !/*op        135*/
      go to 80                                                                  !/*op        136*/
  120 continue                                                                  !/*op        137*/
      call srchn (nc4)                                                          !/*op        138*/
      if (nocas.eq.nocas0.and.ncol.gt.0) go to 10                               !/*op        139*/
      go to 150                                                                 !/*op        140*/
  130 continue                                                                  !/*op        141*/
      lowaz=lowaz+1                                                             !/*op        142*/
      go to 120                                                                 !/*op        143*/
  140 continue                                                                  !/*op        144*/
      ihiaz=ihiaz+1                                                             !/*op        145*/
      go to 120                                                                 !/*op        146*/
  150 continue                                                                  !/*op        147*/
      return                                                                    !/*op        148*/
!                                                                               !/*op        149*/
      end                                                                       !/*op        150*/

!     $Id: clect.F,v 1.4 2002/03/27 18:14:57 gwm Exp $                        !/*cl          1*/
      subroutine clect (a,z,ja,jz,u,kfiss)                                      !/*cl          2*/
#include <lcs/double.h>                                                         /*!cl          3*/
#include <lcs/orcm.h>                                                           /*!cl          4*/
#include <lcs/lptS.h>                                                           /*!cl          5*/
#include <lcs/cacom.h>                                                          /*!cl          6*/
#include <lcs/ercom.h>                                                          /*!cl          7*/
      data z91 /91.d0/, ucut /dp4/                                              !/*cl          8*/
      data nwr1 /0/, nwr2 /0/, ballim /0.1d0/, um /931.20793d0/                 !/*cl          9*/
      if (fkey.eq.dp1) go to 10                                                 !/*cl         10*/
!.......................................................................        !/*cl         11*/
!///// ORNL fission test /////                                                  !/*cl         12*/
      if (ievap.ne.0.and.kfiss.eq.0.and..not.fisinh.and.nofis.ne.0) then        !/*cl         13*/
        if (z.ge.z91.and.(a-z).ge.z91.and.u.ge.ucut) then                       !/*cl         14*/
          evapp=dp1/(dp1+dp1/gamr(a,z,u))                                       !/*cl         15*/
          fissp=dp1-evapp                                                       !/*cl         16*/
          rfiss=rang()                                                          !/*cl         17*/
          if (rfiss.le.fissp) then                                              !/*cl         18*/
            kfiss=1                                                             !/*cl         19*/
            fisinh=.true.                                                       !/*cl         20*/
            go to 90                                                            !/*cl         21*/
          endif                                                                 !/*cl         22*/
        endif                                                                   !/*cl         23*/
      endif                                                                     !/*cl         24*/
!.......................................................................        !/*cl         25*/
      flkcou(1)=dp0                                                             !/*cl         26*/
      flkcou(2)=dost(1,z-flz9(2))                                               !/*cl         27*/
      flkcou(3)=flkcou(2)+6.d-02                                                !/*cl         28*/
      flkcou(4)=flkcou(2)+1.2d-01                                               !/*cl         29*/
      flkcou(6)=dost(2,z-flz9(6))                                               !/*cl         30*/
      flkcou(5)=flkcou(6)-6.d-02                                                !/*cl         31*/
      ccoul(1)=dp1                                                              !/*cl         32*/
      ccou2=dost(3,z-flz9(2))                                                   !/*cl         33*/
      ccoul(2)=ccou2+dp1                                                        !/*cl         34*/
      ccoul(3)=ccou2*1.5d0+dp3                                                  !/*cl         35*/
      ccoul(4)=ccou2+dp3                                                        !/*cl         36*/
      ccoul(6)=dost(4,z-flz9(6))*dp2+dp2                                        !/*cl         37*/
      ccoul(5)=dp2*ccoul(6)-dp1                                                 !/*cl         38*/
   10 continue                                                                  !/*cl         39*/
      kfiss=0                                                                   !/*cl         40*/
!                                                                               !/*cl         41*/
!*****************************************************************              !/*cl         42*/
!                                                                               !/*cl         43*/
!       main loop to compute the emission probabilities                         !/*cl         44*/
!       for the 6 clusters.1=n,2=p,3=d,4=t,5=he3,6=he4                          !/*cl         45*/
!                                                                               !/*cl         46*/
!       alternate coding for ORNL and RAL models                                !/*cl         47*/
!                                                                               !/*cl         48*/
!*****************************************************************              !/*cl         49*/
!                                                                               !/*cl         50*/
      if (ievap.eq.0) go to 60                                                  !/*cl         51*/
!.......................................................................        !/*cl         52*/
!///// ORNL channel probability section /////                                   !/*cl         53*/
      sigma=dp0                                                                 !/*cl         54*/
      do 20 j=1,6                                                               !/*cl         55*/
      s9(j)=dp0                                                                 !/*cl         56*/
      sos(j)=dp0                                                                !/*cl         57*/
      zz=z-flz9(j)                                                              !/*cl         58*/
      aa=a-fla9(j)                                                              !/*cl         59*/
      q9(j)=1.0d+10                                                             !/*cl         60*/
      if (aa.lt.fla9(j).or.zz.lt.flz9(j).or.aa.le.zz) go to 20                  !/*cl         61*/
      mm=ja-ia6(j)                                                              !/*cl         62*/
      q9(j)=energy(aa,zz)-ex1+exmass(j)                                         !/*cl         63*/
      thresh(j)=q9(j)+.88235d0*flkcou(j)*flz9(j)*zz/(rmass(mm)+rho9(j))         !/*cl         64*/
      nn=aa-zz                                                                  !/*cl         65*/
      izz=zz                                                                    !/*cl         66*/
      smalla(j)=geta(u-thresh(j),izz,nn,ilvden,isdum)                           !/*cl         67*/
      corr=dp0                                                                  !/*cl         68*/
      if (fkey.eq.dp0) corr=cam4(izz)+cam5(nn)                                  !/*cl         69*/
      arg=u-thresh(j)-corr                                                      !/*cl         70*/
      if (arg.ge.dp0) then                                                      !/*cl         71*/
        s9(j)=sqrt(smalla(j)*arg)*dp2                                           !/*cl         72*/
        sos(j)=dp10*s9(j)                                                       !/*cl         73*/
      endif                                                                     !/*cl         74*/
   20 continue                                                                  !/*cl         75*/
      n1=1                                                                      !/*cl         76*/
      do 30 j=1,6                                                               !/*cl         77*/
      if (sos(j).ge.1250.d0) then                                               !/*cl         78*/
        ses=max(s9(1),s9(2),s9(3),s9(4),s9(5),s9(6))                            !/*cl         79*/
        go to 40                                                                !/*cl         80*/
      endif                                                                     !/*cl         81*/
   30 continue                                                                  !/*cl         82*/
      n1=2                                                                      !/*cl         83*/
   40 continue                                                                  !/*cl         84*/
      do 50 j=1,6                                                               !/*cl         85*/
      r9(j)=dp0                                                                 !/*cl         86*/
      if (s9(j).eq.dp0) go to 50                                                !/*cl         87*/
      js=sos(j)+dp1                                                             !/*cl         88*/
      mm=ja-ia6(j)                                                              !/*cl         89*/
      if (n1.ne.1.and.js.lt.1000) then                                          !/*cl         90*/
        fjs=js                                                                  !/*cl         91*/
        strun(j)=fjs-dp1                                                        !/*cl         92*/
        eye1(j)=(pp1(js)+(pp1(js+1)-pp1(js))*(sos(j)-strun(j)))/smalla(j        !/*cl         93*/
     &  )**2                                                                    !/*cl         94*/
      else                                                                      !/*cl         95*/
        if (n1.ne.1) then                                                       !/*cl         96*/
          sas=exp(s9(j)-50.d0)                                                  !/*cl         97*/
        else                                                                    !/*cl         98*/
          sas=exp(s9(j)-ses)                                                    !/*cl         99*/
        endif                                                                   !/*cl        100*/
        eye1(j)=(s9(j)**2-dp3*s9(j)+dp3)*sas/(dp4*smalla(j)**2)                 !/*cl        101*/
        fjs=js                                                                  !/*cl        102*/
        strun(j)=fjs-dp1                                                        !/*cl        103*/
      endif                                                                     !/*cl        104*/
      if (j.gt.1) then                                                          !/*cl        105*/
        r9(j)=ccoul(j)*rmass(mm)**2*eye1(j)                                     !/*cl        106*/
      else                                                                      !/*cl        107*/
        if (n1.ne.1.and.js.lt.1000) then                                        !/*cl        108*/
          eye0=(pp0(js)+(pp0(js+1)-pp0(js))*(sos(j)-strun(j)))/smalla(j)        !/*cl        109*/
        else                                                                    !/*cl        110*/
          eye0=(s9(j)-dp1)*dph*sas/smalla(j)                                    !/*cl        111*/
        endif                                                                   !/*cl        112*/
        r9(j)=rmass(mm)**2*alph(mm)*(eye1(j)+bet(mm)*eye0)                      !/*cl        113*/
      endif                                                                     !/*cl        114*/
      r9(j)=max(r9(j),dp0)                                                      !/*cl        115*/
      sigma=sigma+r9(j)                                                         !/*cl        116*/
   50 continue                                                                  !/*cl        117*/
      go to 90                                                                  !/*cl        118*/
!.......................................................................        !/*cl        119*/
!///// RAL channel probability section /////                                    !/*cl        120*/
   60 continue                                                                  !/*cl        121*/
      do 70 j=1,6                                                               !/*cl        122*/
      zz=z-flz9(j)                                                              !/*cl        123*/
      aa=a-fla9(j)                                                              !/*cl        124*/
      s9(j)=dp0                                                                 !/*cl        125*/
      r9(j)=dp0                                                                 !/*cl        126*/
      argp=-dp1                                                                 !/*cl        127*/
! this defeats PENBAR ?????                                                     !/*cl        128*/
      q9(j)=1.0d+10                                                             !/*cl        129*/
      if (aa.lt.fla9(j).or.zz.lt.flz9(j).or.aa.le.zz) go to 70                  !/*cl        130*/
      q9(j)=energy(aa,zz)-ex1+exmass(j)                                         !/*cl        131*/
      temp=dp2*zz/aa                                                            !/*cl        132*/
      mm=ja-ia6(j)                                                              !/*cl        133*/
      coulef=0.846927d0*flkcou(j)*flz9(j)*zz/(rmass(mm)+rho9(j))                !/*cl        134*/
      if (j.gt.1) coulef=coulef/(dp1+0.005d0*u/flz9(j))                         !/*cl        135*/
      thresh(j)=q9(j)+coulef                                                    !/*cl        136*/
      nn=int(aa-zz)                                                             !/*cl        137*/
      izz=int(zz)                                                               !/*cl        138*/
      smalla(j)=geta(u-thresh(j),izz,nn,ilvden,isdum)                           !/*cl        139*/
      corr=cam4(izz)+cam5(nn)                                                   !/*cl        140*/
      if (fkey.eq.dp1) corr=dp0                                                 !/*cl        141*/
      arg=u-thresh(j)-corr                                                      !/*cl        142*/
!                                                                               !/*cl        143*/
!     collect available s9.e. for proton for possible use                       !/*cl        144*/
!     in barrier penetration when below neutron threshhold.                     !/*cl        145*/
!                                                                               !/*cl        146*/
      if (j.eq.2) argp=u-q9(2)-corr                                             !/*cl        147*/
      if (arg.lt.dp0) go to 70                                                  !/*cl        148*/
      s9(j)=sqrt(smalla(j)*arg)*dp2                                             !/*cl        149*/
      call drein1 (j,s9(j),smalla(j),eye1(j),eye0)                              !/*cl        150*/
      if (j.gt.1) then                                                          !/*cl        151*/
        r9(j)=rmass(mm)*rmass(mm)*ccoul(j)*eye1(j)                              !/*cl        152*/
      else                                                                      !/*cl        153*/
        r9(j)=rmass(mm)*rmass(mm)*alph(mm)*(eye1(j)+bet(mm)*eye0)               !/*cl        154*/
      endif                                                                     !/*cl        155*/
      r9(j)=max(r9(j),dp0)                                                      !/*cl        156*/
   70 continue                                                                  !/*cl        157*/
      smax=max(s9(1),s9(2),s9(3),s9(4),s9(5),s9(6))                             !/*cl        158*/
      sigma=dp0                                                                 !/*cl        159*/
      do 80 j=1,6                                                               !/*cl        160*/
      r9(j)=r9(j)*exp(s9(j)-smax)                                               !/*cl        161*/
      sigma=sigma+r9(j)                                                         !/*cl        162*/
   80 continue                                                                  !/*cl        163*/
   90 continue                                                                  !/*cl        164*/
      return                                                                    !/*cl        165*/
      end                                                                       !/*cl        166*/

!     $Id: opt7.F,v 1.5 2002/03/27 18:14:02 gwm Exp $                         !/*op          1*/
      subroutine opt7 (hold,ntim,ntim1)                                         !/*op          2*/
#ifdef LCS                                                                      /*!op          3*/
#include <htape3x/com32b.h>                                                     /*!op          4*/
#include <htape3x/phprm0.h>                                                     /*!op          5*/
      dimension hold(ntim1,21,1)                                                !/*op          6*/
#include <htape3x/label.h>                                                      /*!op          7*/
#include <htape3x/contrl.h>                                                     /*!op          8*/
#include <htape3x/bndy.h>                                                       /*!op          9*/
#include <htape3x/ctconv.h>                                                     /*!op         10*/
#include <htape3x/exm.h>                                                        /*!op         11*/
#include <htape3x/comm7.h>                                                      /*!op         12*/
      dimension bin(20)                                                         !/*op         13*/
      dimension dela(lpt), delz(lpt)                                            !/*op         14*/
      dimension flz(6), fla(6)                                                  !/*op         15*/
      data flz /0.,1.,1.,1.,2.,2./, fla /1.,1.,2.,3.,3.,4./                     !/*op         16*/
      data dela /2*dp1,5*dp0,dp2,2*dp3,dp4,5*dp0,2*-1.d0,4*dp0/                 !/*op         17*/
      data delz /dp1,dp0,dp1,dp0,-1.d0,dp1,-1.d0,2*dp1,2*dp2,dp0,dp1,           !/*op         18*/
     &2*dp0,2*-1.d0,dp0,-1.d0,dp1,2*dp0/                                        !/*op         19*/
      data bin /20*dp0/                                                         !/*op         20*/
      data ntest /0/                                                            !/*op         21*/
      casbal=dp0                                                                !/*op         22*/
      ilowe=0                                                                   !/*op         23*/
   10 continue                                                                  !/*op         24*/
      if (ilowe.ne.0) go to 380                                                 !/*op         25*/
      if (ncol.lt.1.or.ncol.gt.8) go to 380                                     !/*op         26*/
      if (nparm.le.0) go to 30                                                  !/*op         27*/
      if (jmat.eq.0.and.ncol.eq.1) icomp=blz                                    !/*op         28*/
      if (jmat.eq.0.and.ncol.ne.1) icomp=bold                                   !/*op         29*/
      if (jmat.ne.0.and.ncol.eq.1) icomp=nmed                                   !/*op         30*/
      if (jmat.ne.0.and.ncol.ne.1) icomp=mat                                    !/*op         31*/
      do 20 ib=1,nparm                                                          !/*op         32*/
      if (lparm(ib).eq.icomp) go to 40                                          !/*op         33*/
   20 continue                                                                  !/*op         34*/
   30 continue                                                                  !/*op         35*/
      ib=0                                                                      !/*op         36*/
   40 continue                                                                  !/*op         37*/
      if (ntim1.le.1) go to 60                                                  !/*op         38*/
      do 50 it=1,ntim                                                           !/*op         39*/
      if (tc.lt.timb(it)) go to 70                                              !/*op         40*/
   50 continue                                                                  !/*op         41*/
      go to 380                                                                 !/*op         42*/
   60 continue                                                                  !/*op         43*/
      it=1                                                                      !/*op         44*/
   70 continue                                                                  !/*op         45*/
      ityp=tip+1.0                                                              !/*op         46*/
      go to (80,100,140,310,140,90,260,330), ncol                               !/*op         47*/
   80 continue                                                                  !/*op         48*/
! starting point for NCOL=1                                                     !/*op         49*/
      bin(1)=bin(1)+wt*e                                                        !/*op         50*/
      bin(2)=bin(2)+wt*pmass(ityp)                                              !/*op         51*/
      if (ityp.eq.4) bin(9)=bin(9)-(pmass(4)+e)*wt                              !/*op         52*/
      if (e.lt.emin(ityp)) then                                                 !/*op         53*/
        ilowe=1                                                                 !/*op         54*/
        ec=dp0                                                                  !/*op         55*/
        if (ityp.eq.2) then                                                     !/*op         56*/
          bin(11)=bin(11)-oldwt*e                                               !/*op         57*/
          bin(12)=bin(12)-oldwt*pmass(ityp)                                     !/*op         58*/
          go to 380                                                             !/*op         59*/
        else                                                                    !/*op         60*/
          go to 320                                                             !/*op         61*/
        endif                                                                   !/*op         62*/
      endif                                                                     !/*op         63*/
      go to 340                                                                 !/*op         64*/
  100 continue                                                                  !/*op         65*/
! starting point for NCOL=2                                                     !/*op         66*/
      if (n1col.lt.0) go to 90                                                  !/*op         67*/
      do 95 i=1,nabov                                                           !/*op         68*/
      itp=tipa(i)+1.0                                                           !/*op         69*/
      if (ea(i).gt.emin(itp)) go to 95                                          !/*op         70*/
      if (itp.eq.1) then                                                        !/*op         71*/
        bin(6)=bin(6)-wta(i)*pmass(itp)                                         !/*op         72*/
        bin(5)=bin(5)-wta(i)*ea(i)                                              !/*op         73*/
      elseif (itp.eq.2) then                                                    !/*op         74*/
        bin(11)=bin(11)-wta(i)*ea(i)                                            !/*op         75*/
        bin(12)=bin(12)-wta(i)*pmass(itp)                                       !/*op         76*/
      endif                                                                     !/*op         77*/
   95 continue                                                                  !/*op         78*/
   90 continue                                                                  !/*op         79*/
! starting point for NCOL=6                                                     !/*op         80*/
      if (mnuc.eq.0) go to 115                                                  !/*op         81*/
      do 110 i=1,mnuc                                                           !/*op         82*/
      if (aprr(i).eq.5..or.aprr(i).gt.3..and.(zprr(i).eq.0..or.aprr(i)          !/*op         83*/
     & .eq.zprr(i))) then                                                       !/*op         84*/
        write (io,410) nocas0,aprr(i),zprr(i)                                   !/*op         85*/
        write (jtty,410) nocas0,aprr(i),zprr(i)                                 !/*op         86*/
      endif                                                                     !/*op         87*/
  110 continue                                                                  !/*op         88*/
  115 continue                                                                  !/*op         89*/
      if (n1col.ge.0) go to 120                                                 !/*op         90*/
      bin(1)=bin(1)+oldwt*e                                                     !/*op         91*/
      bin(2)=bin(2)+oldwt*pmass(ityp)                                           !/*op         92*/
  120 continue                                                                  !/*op         93*/
      if (mnuc.eq.0) go to 135                                                  !/*op         94*/
      do 130 i=1,mnuc                                                           !/*op         95*/
      bin(7)=bin(7)-wt*erecr(i)                                                 !/*op         96*/
      bin(8)=bin(8)-wt*uur(i)                                                   !/*op         97*/
      bin(20)=bin(20)-wt*ecvr(i)                                                !/*op         98*/
  130 continue                                                                  !/*op         99*/
  135 continue                                                                  !/*op        100*/
      if (lelem.eq.-1) then                                                     !/*op        101*/
        apr0=dp1                                                                !/*op        102*/
        zpr0=dp1                                                                !/*op        103*/
      else                                                                      !/*op        104*/
        apr0=a(mel(nmed)+lelem-1)                                               !/*op        105*/
        zpr0=zz(mel(nmed)+lelem-1)                                              !/*op        106*/
      endif                                                                     !/*op        107*/
  140 continue                                                                  !/*op        108*/
! starting point for NCOL=5                                                     !/*op        109*/
      nup=nabov+nbelo                                                           !/*op        110*/
      if (nup.le.0) go to 230                                                   !/*op        111*/
      nlo=1                                                                     !/*op        112*/
      if (n1col.ge.0) then                                                      !/*op        113*/
        if (nbelo.le.0) go to 230                                               !/*op        114*/
        nlo=nabov+1                                                             !/*op        115*/
      endif                                                                     !/*op        116*/
      do 220 j=nlo,nup                                                          !/*op        117*/
      if (j.gt.nabov) then                                                      !/*op        118*/
        i=l02+j-nabov                                                           !/*op        119*/
      else                                                                      !/*op        120*/
        i=j                                                                     !/*op        121*/
      endif                                                                     !/*op        122*/
      itp=tipa(i)+1.0                                                           !/*op        123*/
      go to (150,160,150,190,150,170,170,150,150,150,150,180,150,190,190        !/*op        124*/
     & ,150,150,190,200,200,210,210), itp                                       !/*op        125*/
  150 continue                                                                  !/*op        126*/
!**** P PI+ PI- IONS K+ K- Pbar                                                 !/*op        127*/
      bin(6)=bin(6)-wta(i)*pmass(itp)                                           !/*op        128*/
      bin(5)=bin(5)-wta(i)*ea(i)                                                !/*op        129*/
      go to 220                                                                 !/*op        130*/
  160 continue                                                                  !/*op        131*/
!**** N                                                                         !/*op        132*/
      bin(11)=bin(11)-wta(i)*ea(i)                                              !/*op        133*/
      bin(12)=bin(12)-wta(i)*pmass(itp)                                         !/*op        134*/
      go to 220                                                                 !/*op        135*/
  170 continue                                                                  !/*op        136*/
!**** MU+ MU-                                                                   !/*op        137*/
      bin(itp+10)=bin(itp+10)-wta(i)*pmass(itp)                                 !/*op        138*/
      bin(18)=bin(18)-wta(i)*ea(i)                                              !/*op        139*/
      go to 220                                                                 !/*op        140*/
  180 continue                                                                  !/*op        141*/
!**** PHOTON                                                                    !/*op        142*/
      bin(9)=bin(9)-(pmass(itp)+ea(i))*wta(i)                                   !/*op        143*/
      go to 220                                                                 !/*op        144*/
  190 continue                                                                  !/*op        145*/
!**** PI0 K0L K0S Nbar                                                          !/*op        146*/
      if (n1col.ge.0) go to 220                                                 !/*op        147*/
      bin(4)=bin(4)-wta(i)*pmass(itp)                                           !/*op        148*/
      bin(3)=bin(3)-wta(i)*ea(i)                                                !/*op        149*/
      go to 220                                                                 !/*op        150*/
  200 continue                                                                  !/*op        151*/
!**** E+ E-                                                                     !/*op        152*/
      bin(15)=bin(15)-wta(i)*ea(i)                                              !/*op        153*/
      bin(33-itp)=bin(33-itp)-wta(i)*elmass                                     !/*op        154*/
      go to 220                                                                 !/*op        155*/
  210 continue                                                                  !/*op        156*/
!**** NU NUbar                                                                  !/*op        157*/
      bin(10)=bin(10)-wta(i)*ea(i)                                              !/*op        158*/
  220 continue                                                                  !/*op        159*/
  230 continue                                                                  !/*op        160*/
      if (ncol.eq.5) go to 330                                                  !/*op        161*/
      if (ncol.eq.3) go to 320                                                  !/*op        162*/
      tem=xmnuc(apr0,zpr0)                                                      !/*op        163*/
      if (mnuc.eq.0) go to 255                                                  !/*op        164*/
      do 250 i=1,mnuc                                                           !/*op        165*/
      ia=aprr(i)+0.01                                                           !/*op        166*/
      apr3=ia                                                                   !/*op        167*/
      iz=zprr(i)+0.01                                                           !/*op        168*/
      zpr3=iz                                                                   !/*op        169*/
      if (zpr3.eq.dp0) go to 240                                                !/*op        170*/
      if (apr3.eq.zpr3) tem=tem-apr3*pmass(1)                                   !/*op        171*/
      if (apr3.gt.zpr3) tem=tem-xmnuc(apr3,zpr3)                                !/*op        172*/
      go to 250                                                                 !/*op        173*/
  240 continue                                                                  !/*op        174*/
      tem=tem-aprr(i)*pmass(2)                                                  !/*op        175*/
  250 continue                                                                  !/*op        176*/
  255 continue                                                                  !/*op        177*/
      bin(19)=bin(19)+wt*tem                                                    !/*op        178*/
      go to 330                                                                 !/*op        179*/
  260 continue                                                                  !/*op        180*/
! starting point for NCOL=7                                                     !/*op        181*/
      hold(it,3,nbin)=hold(it,3,nbin)+wt*ec                                     !/*op        182*/
      hold(it,4,nbin)=hold(it,4,nbin)+wt*pmass(ityp)                            !/*op        183*/
      hold(it,21,nbin)=hold(it,21,nbin)+wt*(ec+pmass(ityp))                     !/*op        184*/
      if (.not.lft) go to 270                                                   !/*op        185*/
      hold(ntim1,3,nbin)=hold(ntim1,3,nbin)+wt*ec                               !/*op        186*/
      hold(ntim1,4,nbin)=hold(ntim1,4,nbin)+wt*pmass(ityp)                      !/*op        187*/
      hold(ntim1,21,nbin)=hold(ntim1,21,nbin)+wt*(ec+pmass(ityp))               !/*op        188*/
  270 continue                                                                  !/*op        189*/
      casbal=casbal+wt*(ec+pmass(ityp))                                         !/*op        190*/
      if (nparm.le.0) go to 310                                                 !/*op        191*/
      if (jmat.eq.0) icomp=blz                                                  !/*op        192*/
      if (jmat.ne.0) icomp=nmed                                                 !/*op        193*/
      do 280 ib1=1,nparm                                                        !/*op        194*/
      if (lparm(ib1).eq.icomp) go to 290                                        !/*op        195*/
  280 continue                                                                  !/*op        196*/
      go to 310                                                                 !/*op        197*/
  290 continue                                                                  !/*op        198*/
      hold(it,3,ib1)=hold(it,3,ib1)+wt*ec                                       !/*op        199*/
      hold(it,4,ib1)=hold(it,4,ib1)+wt*pmass(ityp)                              !/*op        200*/
      hold(it,21,ib1)=hold(it,21,ib1)+wt*(ec+pmass(ityp))                       !/*op        201*/
      if (.not.lft) go to 300                                                   !/*op        202*/
      hold(ntim1,3,ib1)=hold(ntim1,3,ib1)+wt*ec                                 !/*op        203*/
      hold(ntim1,4,ib1)=hold(ntim1,4,ib1)+wt*pmass(ityp)                        !/*op        204*/
      hold(ntim1,21,ib1)=hold(ntim1,21,ib1)+wt*(ec+pmass(ityp))                 !/*op        205*/
  300 continue                                                                  !/*op        206*/
  310 continue                                                                  !/*op        207*/
! starting point for NCOL=4                                                     !/*op        208*/
      bin(3)=bin(3)-wt*ec                                                       !/*op        209*/
      bin(4)=bin(4)-wt*pmass(ityp)                                              !/*op        210*/
      if (ityp-2) 330,340,330                                                   !/*op        211*/
  320 continue                                                                  !/*op        212*/
! starting point for NCOL=3                                                     !/*op        213*/
      if (ityp.gt.1.and.ityp.lt.8) go to 330                                    !/*op        214*/
      if (ityp.ge.12) go to 330                                                 !/*op        215*/
!     stable particles                                                          !/*op        216*/
      ec=dp0                                                                    !/*op        217*/
      bin(6)=bin(6)-oldwt*pmass(ityp)                                           !/*op        218*/
  330 continue                                                                  !/*op        219*/
      if (delz(ityp).eq.dp0) go to 340                                          !/*op        220*/
      bin(5)=bin(5)-oldwt*(e-ec)                                                !/*op        221*/
  340 continue                                                                  !/*op        222*/
      do 370 i=1,20                                                             !/*op        223*/
      hold(it,i,nbin)=hold(it,i,nbin)+bin(i)                                    !/*op        224*/
      hold(it,21,nbin)=hold(it,21,nbin)+bin(i)                                  !/*op        225*/
      if (.not.lft) go to 350                                                   !/*op        226*/
      hold(ntim1,i,nbin)=hold(ntim1,i,nbin)+bin(i)                              !/*op        227*/
      hold(ntim1,21,nbin)=hold(ntim1,21,nbin)+bin(i)                            !/*op        228*/
  350 continue                                                                  !/*op        229*/
      if (ib.eq.0) go to 360                                                    !/*op        230*/
      hold(it,i,ib)=hold(it,i,ib)+bin(i)                                        !/*op        231*/
      hold(it,21,ib)=hold(it,21,ib)+bin(i)                                      !/*op        232*/
      if (.not.lft) go to 360                                                   !/*op        233*/
      hold(ntim1,i,ib)=hold(ntim1,i,ib)+bin(i)                                  !/*op        234*/
      hold(ntim1,21,ib)=hold(ntim1,21,ib)+bin(i)                                !/*op        235*/
  360 continue                                                                  !/*op        236*/
      casbal=casbal+bin(i)                                                      !/*op        237*/
      bin(i)=dp0                                                                !/*op        238*/
  370 continue                                                                  !/*op        239*/
  380 continue                                                                  !/*op        240*/
      call rednmt                                                               !/*op        241*/
      if (ncol.le.0) go to 390                                                  !/*op        242*/
      if (nocas-nocas0) 390,10,390                                              !/*op        243*/
  390 continue                                                                  !/*op        244*/
      if (abs(casbal).gt.abs(balmax)) then                                      !/*op        245*/
        balmax=casbal                                                           !/*op        246*/
        nocasm=nocas0                                                           !/*op        247*/
      endif                                                                     !/*op        248*/
      if (nobalc.eq.0.and.abs(casbal).lt.0.050) go to 400                       !/*op        249*/
      if (nobalc.ne.0.and.abs(casbal).lt.10.0) go to 400                        !/*op        250*/
      ntest=ntest+1                                                             !/*op        251*/
      if (ntest.le.5.or.casbal.eq.balmax) write (jtty,430) nocas0               !/*op        252*/
     & ,casbal                                                                  !/*op        253*/
      if (ntest.le.50.or.casbal.eq.balmax) write (io,420) nocas0                !/*op        254*/
     & ,casbal                                                                  !/*op        255*/
  400 continue                                                                  !/*op        256*/
!                                                                               !/*op        257*/
  410 format (5x,'light residual - nocas=',i7,' apr=',f5.0,' zpr=',f5.0)        !/*op        258*/
  420 format (5x,'mass-energy balance for cascade #',i7,' =',1pd14.5)           !/*op        259*/
  430 format (' mass-energy balance cascade #',i7,' =',1pd14.5)                 !/*op        260*/
#endif                                                                          /*!op        261*/
      return                                                                    !/*op        262*/
      end                                                                       !/*op        263*/

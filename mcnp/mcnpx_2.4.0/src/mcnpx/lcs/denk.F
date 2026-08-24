!     $Id: denk.F,v 1.4 2002/03/27 18:15:00 gwm Exp $                         !/*de          1*/
      subroutine denk (csa1,ekin,idt,idp,voc0,cutofp,cutofn)                    !/*de          2*/
#include <lcs/double.h>                                                         /*!de          3*/
!     ==================================================================        !/*de          4*/
!     denk establishes the contents of needed arrays and variables for          !/*de          5*/
!     each collision.                                                           !/*de          6*/
!     csa1 is particle type, ekin is kinetic energy of projectile               !/*de          7*/
!     idt identifies the target nucleus                                         !/*de          8*/
!     if csa1.eq.0, idp identifies the projectile nucleus                       !/*de          9*/
!                                                                               !/*de         10*/
! DENK   calls   ENERGY  CLMB3  FLTRNF  SQRT                                    !/*de         11*/
! called by HCASC                                                               !/*de         12*/
!     ==================================================================        !/*de         13*/
!                                                                               !/*de         14*/
#ifdef OLDM                                                                     /*!de         15*/
      parameter (hbar=6.581d-22, clite=2.9979d+10, cmtofm=1.d-13, fsc           !/*de         16*/
     & =dp1/137.04d0, hbarc=hbar*clite, rcoul=1.7d0, conar=dph*fsc              !/*de         17*/
     & *cmtofm*rcoul/hbarc, amu=931.20793d0)                                    !/*de         18*/
#else                                                                           /*!de         19*/
#include <lcs/phprm0.h>                                                         /*!de         20*/
#include <lcs/phprm1.h>                                                         /*!de         21*/
#endif                                                                          /*!de         22*/
#include <lcs/vegas.h>                                                          /*!de         23*/
      dimension efx(34), pefx(34), pfrx(34), pfwx(34), ppfrx(34), ppfwx(        !/*de         24*/
     & 34)                                                                      !/*de         25*/
      equivalence (ef,efx), (pef,pefx), (pfr,pfrx), (pfw,pfwx), (ppfr           !/*de         26*/
     & ,ppfrx), (ppfw,ppfwx)                                                    !/*de         27*/
      dimension f(250)                                                          !/*de         28*/
      data (f(i),i=1,250) /1.14320d0,1.10623d0,1.09273d0,1.08591d0,1.081        !/*de         29*/
     & 89d0,1.07930d0,1.07752d0,1.07624d0,1.07528d0,1.07456d0,1.07399d0,        !/*de         30*/
     & 1.07353d0,1.07317d0,1.07287d0,1.07262d0,1.07241d0,1.07223d0,1.072        !/*de         31*/
     & 08d0,1.07196d0,1.07184d0,1.07175d0,1.07166d0,1.07159d0,1.07153d0,        !/*de         32*/
     & 1.07147d0,1.07142d0,1.07137d0,1.07133d0,1.07129d0,1.07126d0,1.071        !/*de         33*/
     & 23d0,1.07120d0,1.07118d0,1.07116d0,1.07114d0,1.07112d0,1.07110d0,        !/*de         34*/
     & 1.07109d0,1.07107d0,1.07106d0,1.07105d0,1.07104d0,1.07103d0,1.071        !/*de         35*/
     & 02d0,1.07101d0,1.07100d0,1.07099d0,1.07099d0,1.07098d0,1.07097d0,        !/*de         36*/
     & 1.07097d0,1.07096d0,1.07096d0,1.07095d0,1.07095d0,1.07094d0,1.070        !/*de         37*/
     & 94d0,1.07094d0,1.07093d0,1.07093d0,1.07093d0,1.07093d0,1.07092d0,        !/*de         38*/
     & 1.07092d0,1.07092d0,1.07092d0,1.07091d0,1.07091d0,1.07091d0,1.070        !/*de         39*/
     & 91d0,1.07091d0,1.07090d0,1.07090d0,1.07090d0,1.07090d0,1.07090d0,        !/*de         40*/
     & 1.07090d0,1.07090d0,1.07090d0,13*1.07089d0,31*1.07088d0,127*1.070        !/*de         41*/
     & 87d0/                                                                    !/*de         42*/
#ifdef OLDM                                                                     /*!de         43*/
      data elmass, amumev /0.511d0,931.20793d0/                                 !/*de         44*/
#endif                                                                          /*!de         45*/
      inine=ieght+1                                                             !/*de         46*/
      ipt=1                                                                     !/*de         47*/
      if (csa1.lt.dp0) ipt=2                                                    !/*de         48*/
      do 10 i=3,14                                                              !/*de         49*/
   10 csa(i)=hcsa(i,ipt,idt)                                                    !/*de         50*/
      csa(1)=csa1                                                               !/*de         51*/
      fa=csa(3)                                                                 !/*de         52*/
      fq=csa(4)                                                                 !/*de         53*/
      fn=fa-fq                                                                  !/*de         54*/
      if (fq.le.dp2.and.fn.le.dp2) then                                         !/*de         55*/
        xmt0=xmasss(int(fq)+1,int(fn)+1)                                        !/*de         56*/
      else                                                                      !/*de         57*/
#ifdef OLDM                                                                     /*!de         58*/
        xmt0=energy(fa,fq)+fa*amumev-fq*elmass                                  !/*de         59*/
#else                                                                           /*!de         60*/
        xmt0=xmnuc(fa,fq)                                                       !/*de         61*/
#endif                                                                          /*!de         62*/
      endif                                                                     !/*de         63*/
!     xmt0=energy(fa,fq)-fq*(energy(1.,1.)-amass(1))-fn*(energy(1.,             !/*de         64*/
!    &0.)-amass(2))                                                             !/*de         65*/
      if (csa1.ne.dp0) go to 20                                                 !/*de         66*/
      proj(3)=hcsa(3,ipt,idp)                                                   !/*de         67*/
      proj(4)=hcsa(4,ipt,idp)                                                   !/*de         68*/
      csa(15)=hcsa(5,ipt,idp)                                                   !/*de         69*/
      proj(8)=hcsa(8,ipt,idp)                                                   !/*de         70*/
      proj(9)=hcsa(9,ipt,idp)                                                   !/*de         71*/
      proj(10)=hcsa(10,ipt,idp)                                                 !/*de         72*/
      proj(11)=hcsa(11,ipt,idp)                                                 !/*de         73*/
      fa=proj(3)                                                                !/*de         74*/
      fq=proj(4)                                                                !/*de         75*/
      fn=fa-fq                                                                  !/*de         76*/
      if (fq.le.dp2.and.fn.le.dp2) then                                         !/*de         77*/
        xmp0=xmasss(int(fq)+1,int(fn)+1)                                        !/*de         78*/
      else                                                                      !/*de         79*/
#ifdef OLDM                                                                     /*!de         80*/
        xmp0=energy(fa,fq)+fa*amumev-fq*elmass                                  !/*de         81*/
#else                                                                           /*!de         82*/
        xmp0=xmnuc(fa,fq)                                                       !/*de         83*/
#endif                                                                          /*!de         84*/
      endif                                                                     !/*de         85*/
!     xmp0=energy(fa,fq)-fq*(energy(1.,1.)-amass(1))-fn*(energy(1.,             !/*de         86*/
!    &0.)-amass(2))                                                             !/*de         87*/
#ifdef OLDM                                                                     /*!de         88*/
      cbar=proj(4)*csa(11)*csa(9)/(csa(9)+proj(9))                              !/*de         89*/
      if (ekin.le.cbar) then                                                    !/*de         90*/
        ekin=-dp1                                                               !/*de         91*/
        return                                                                  !/*de         92*/
      endif                                                                     !/*de         93*/
      arg0=conar*proj(4)*proj(9)*amu                                            !/*de         94*/
#else                                                                           /*!de         95*/
      arg0=conar*proj(4)*proj(9)*amuo16                                         !/*de         96*/
#endif                                                                          /*!de         97*/
      fk=clmb3(arg0)                                                            !/*de         98*/
      fac=dp3                                                                   !/*de         99*/
      proj(11)=fac*proj(11)/(dp1+(fac/fk-dp1)*rang())                           !/*de        100*/
      go to 30                                                                  !/*de        101*/
   20 continue                                                                  !/*de        102*/
      proj(3)=dp1                                                               !/*de        103*/
      proj(4)=dp1                                                               !/*de        104*/
      csa(15)=dp0                                                               !/*de        105*/
      proj(8)=dp1                                                               !/*de        106*/
      proj(9)=dp0                                                               !/*de        107*/
      proj(10)=dp0                                                              !/*de        108*/
      proj(11)=dp0                                                              !/*de        109*/
      iamas=csa1                                                                !/*de        110*/
      xmp0=amass(abs(iamas))                                                    !/*de        111*/
   30 continue                                                                  !/*de        112*/
#ifdef OLDM                                                                     /*!de        113*/
      arg0=conar*csa(4)*csa(9)*amu                                              !/*de        114*/
#else                                                                           /*!de        115*/
      arg0=conar*csa(4)*csa(9)*amuo16                                           !/*de        116*/
#endif                                                                          /*!de        117*/
      fk=clmb3(arg0)                                                            !/*de        118*/
      fac=dp3                                                                   !/*de        119*/
      csa(11)=fac*csa(11)/(dp1+(fac/fk-dp1)*rang())                             !/*de        120*/
      do 40 i=12,14                                                             !/*de        121*/
   40 proj(i)=dp0                                                               !/*de        122*/
      do 50 i=1,32                                                              !/*de        123*/
      ws(i)=dp0                                                                 !/*de        124*/
   50 wsp(i)=dp0                                                                !/*de        125*/
      csa(11)=csa(11)+proj(11)                                                  !/*de        126*/
      proj(11)=dp0                                                              !/*de        127*/
! reclaim cutofa                                                                !/*de        128*/
      cutofa(1)=csa(5)+csa(11)                                                  !/*de        129*/
      cutofa(2)=csa(5)                                                          !/*de        130*/
      cutofa(4)=csa(5)*(dp1+dp2*rang())                                         !/*de        131*/
      cutofa(3)=max(cutofa(1),cutofa(4))                                        !/*de        132*/
!             cutofa5 should be for i++, but is never used                      !/*de        133*/
      cutofa(5)=cutofa(1)+cutofa(1)-csa(5)                                      !/*de        134*/
      cutofa(6)=cutofa(2)                                                       !/*de        135*/
      cutofa(11)=csa(15)*(dp1+dp2*rang())                                       !/*de        136*/
      cutofa(10)=max(csa(15)+csa(11),cutofa(11))                                !/*de        137*/
      cutofp=cutofa(3)-csa(5)                                                   !/*de        138*/
      cutofn=cutofa(4)-csa(5)                                                   !/*de        139*/
!                                                                               !/*de        140*/
      inine=ieght+1                                                             !/*de        141*/
      do 60 i=1,inine                                                           !/*de        142*/
   60 xa(i)=hx(i,ipt,idt)                                                       !/*de        143*/
      do 70 i=1,ieght                                                           !/*de        144*/
   70 den_(i)=hden(i,ipt,idt)                                                   !/*de        145*/
      inine2=2*inine                                                            !/*de        146*/
      do 80 i=1,inine2                                                          !/*de        147*/
   80 efx(i)=hef(i,ipt,idt)                                                     !/*de        148*/
      do 90 i=1,inine2                                                          !/*de        149*/
   90 pfrx(i)=hpfr(i,ipt,idt)                                                   !/*de        150*/
      do 100 i=1,inine2                                                         !/*de        151*/
  100 pfwx(i)=hpfw(i,ipt,idt)                                                   !/*de        152*/
      if (csa1.ne.dp0) go to 160                                                !/*de        153*/
      do 110 i=1,inine                                                          !/*de        154*/
  110 ya(i)=hx(i,ipt,idp)                                                       !/*de        155*/
      do 120 i=1,ieght                                                          !/*de        156*/
  120 pdn(i)=hden(i,ipt,idp)                                                    !/*de        157*/
      inine2=2*inine                                                            !/*de        158*/
      do 130 i=1,inine2                                                         !/*de        159*/
  130 pefx(i)=hef(i,ipt,idp)                                                    !/*de        160*/
      do 140 i=1,inine2                                                         !/*de        161*/
  140 ppfrx(i)=hpfr(i,ipt,idp)                                                  !/*de        162*/
      do 150 i=1,inine2                                                         !/*de        163*/
  150 ppfwx(i)=hpfw(i,ipt,idp)                                                  !/*de        164*/
      go to 180                                                                 !/*de        165*/
  160 continue                                                                  !/*de        166*/
      do 170 i=1,inine                                                          !/*de        167*/
  170 ya(i)=dp0                                                                 !/*de        168*/
  180 continue                                                                  !/*de        169*/
      if (csa(1).eq.dp0) go to 190                                              !/*de        170*/
!  vap is the ratio of ap to p potential depths                                 !/*de        171*/
      vap=csa(13)                                                               !/*de        172*/
      csa(13)=dp0                                                               !/*de        173*/
  190 continue                                                                  !/*de        174*/
!                                                                               !/*de        175*/
      th=dpth                                                                   !/*de        176*/
      e0=ekin+xmp0                                                              !/*de        177*/
      bige=e0+xmt0                                                              !/*de        178*/
      bigp(1)=dp0                                                               !/*de        179*/
      bigp(2)=dp0                                                               !/*de        180*/
      bigp(3)=sqrt(e0**2-xmp0**2)                                               !/*de        181*/
      voc=bigp(3)/e0                                                            !/*de        182*/
      voc0=voc                                                                  !/*de        183*/
      csa(2)=e0/proj(3)                                                         !/*de        184*/
      gama=sqrt(dp1-voc*voc)                                                    !/*de        185*/
      icsb(14)=1                                                                !/*de        186*/
      if (iden.lt.2) go to 200                                                  !/*de        187*/
      fac=1                                                                     !/*de        188*/
      icsa3=proj(3)                                                             !/*de        189*/
      if (icsa3.eq.2) fac=2.1544d0                                              !/*de        190*/
      if (ibrad.eq.0) then                                                      !/*de        191*/
        pt=1.16d0*fac                                                           !/*de        192*/
      else                                                                      !/*de        193*/
        pt=1.08323d0*f(icsa3)*fac                                               !/*de        194*/
      endif                                                                     !/*de        195*/
      ap=0.99d0/sqrt(dp2)                                                       !/*de        196*/
      r0p=pt*proj(3)**th/ap                                                     !/*de        197*/
  200 continue                                                                  !/*de        198*/
!                                                                               !/*de        199*/
      proj(1)=ef(1,1)+csa(2)+csa(5)                                             !/*de        200*/
      proj(2)=ef(2,1)+csa(2)+csa(5)                                             !/*de        201*/
!                                                                               !/*de        202*/
      sxy=xa(1)+ya(1)                                                           !/*de        203*/
      yp=-sxy                                                                   !/*de        204*/
!                                                                               !/*de        205*/
      ws(3)=csa(1)                                                              !/*de        206*/
!                  avoid vector bound problem with nucleus+nucleus calcu        !/*de        207*/
      if (ws(3).eq.dp0) ws(3)=dp1                                               !/*de        208*/
!                                                                               !/*de        209*/
      iws3=abs(ws(3))                                                           !/*de        210*/
      ws(4)=amass(iws3)                                                         !/*de        211*/
      ws(5)=csa(2)                                                              !/*de        212*/
      ws(22)=dp0                                                                !/*de        213*/
      ws(23)=dp0                                                                !/*de        214*/
      wsp(22)=dp0                                                               !/*de        215*/
      wsp(23)=dp0                                                               !/*de        216*/
      end                                                                       !/*de        217*/

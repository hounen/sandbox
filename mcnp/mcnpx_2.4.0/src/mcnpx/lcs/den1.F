!     $Id: den1.F,v 1.5 2002/03/27 18:15:00 gwm Exp $                         !/*de          1*/
      subroutine den1                                                           !/*de          2*/
#include <lcs/double.h>                                                         /*!de          3*/
!     ==================================================================        !/*de          4*/
!     den1 calculates the boundaries, fermi energy, and density of              !/*de          5*/
!     each region. there are 8 regions defined by x(i).  In the isobar          !/*de          6*/
!     program efp and efn are the fermi kinetic energy of the protons           !/*de          7*/
!     and neutrons.                                                             !/*de          8*/
!                                                                               !/*de          9*/
! DEN1 calls SQRT  EXP  COSH  SINH                                              !/*de         10*/
!     DEN1 called by STORA                                                      !/*de         11*/
!     ==================================================================        !/*de         12*/
!                                                                               !/*de         13*/
#include <lcs/vegas.h>                                                          /*!de         14*/
      dimension depn(2,17)                                                      !/*de         15*/
!       amass(7,9) are k+,k- masses, (8) is k0 mass                             !/*de         16*/
      dimension f(250)                                                          !/*de         17*/
      data tt /2.50682d0/                                                       !/*de         18*/
      data (f(i),i=1,250) /1.14320d0,1.10623d0,1.09273d0,1.08591d0,1.081        !/*de         19*/
     & 89d0,1.07930d0,1.07752d0,1.07624d0,1.07528d0,1.07456d0,1.07399d0,        !/*de         20*/
     & 1.07353d0,1.07317d0,1.07287d0,1.07262d0,1.07241d0,1.07223d0,1.072        !/*de         21*/
     & 08d0,1.07196d0,1.07184d0,1.07175d0,1.07166d0,1.07159d0,1.07153d0,        !/*de         22*/
     & 1.07147d0,1.07142d0,1.07137d0,1.07133d0,1.07129d0,1.07126d0,1.071        !/*de         23*/
     & 23d0,1.07120d0,1.07118d0,1.07116d0,1.07114d0,1.07112d0,1.07110d0,        !/*de         24*/
     & 1.07109d0,1.07107d0,1.07106d0,1.07105d0,1.07104d0,1.07103d0,1.071        !/*de         25*/
     & 02d0,1.07101d0,1.07100d0,1.07099d0,1.07099d0,1.07098d0,1.07097d0,        !/*de         26*/
     & 1.07097d0,1.07096d0,1.07096d0,1.07095d0,1.07095d0,1.07094d0,1.070        !/*de         27*/
     & 94d0,1.07094d0,1.07093d0,1.07093d0,1.07093d0,1.07093d0,1.07092d0,        !/*de         28*/
     & 1.07092d0,1.07092d0,1.07092d0,1.07091d0,1.07091d0,1.07091d0,1.070        !/*de         29*/
     & 91d0,1.07091d0,1.07090d0,1.07090d0,1.07090d0,1.07090d0,1.07090d0,        !/*de         30*/
     & 1.07090d0,1.07090d0,1.07090d0,13*1.07089d0,31*1.07088d0,127*1.070        !/*de         31*/
     & 87d0/                                                                    !/*de         32*/
!                                                                               !/*de         33*/
!  ef(1,i) and ef(2,i) are the kinetic energy of protons and neutrons.          !/*de         34*/
!  depn(1,2;i),and den_(i),are the density of protons, neutrons,                !/*de         35*/
!  and total density. pfr(1,i) and pfr(2,i) are fermi                           !/*de         36*/
!  momentum for protons and neutrons.                                           !/*de         37*/
!                                                                               !/*de         38*/
      th=dpth                                                                   !/*de         39*/
      csa(7)=csa(3)-csa(4)                                                      !/*de         40*/
      csa(8)=csa(4)/csa(3)                                                      !/*de         41*/
      icsa3=csa(3)                                                              !/*de         42*/
      fac=1                                                                     !/*de         43*/
      if (icsa3.eq.2) fac=2.1544d0                                              !/*de         44*/
      ieght=8                                                                   !/*de         45*/
      if (iden-1) 30,40,10                                                      !/*de         46*/
!                                                                               !/*de         47*/
!         krappe generates a density by folding bulk matter                     !/*de         48*/
!         with proton density whose form is yukawa, and whose                   !/*de         49*/
!         convolution is analytic. [annals of physics 99(1976)142 ]             !/*de         50*/
!                                                                               !/*de         51*/
   10 ieght=16                                                                  !/*de         52*/
      if (ibrad.eq.0) then                                                      !/*de         53*/
        pt=1.16d0*fac                                                           !/*de         54*/
      else                                                                      !/*de         55*/
        pt=1.08323d0*f(icsa3)*fac                                               !/*de         56*/
      endif                                                                     !/*de         57*/
      at=0.99d0/sqrt(dp2)                                                       !/*de         58*/
      r0=pt*csa(3)**th/at                                                       !/*de         59*/
      at3=at**3                                                                 !/*de         60*/
!                                                                               !/*de         61*/
!             output yukawa radii and diffusivities                             !/*de         62*/
!                                                                               !/*de         63*/
      qr0=r0*at                                                                 !/*de         64*/
!                                                                               !/*de         65*/
      xa(1)=qr0-dxp(1)                                                          !/*de         66*/
      do 20 k=2,ieght                                                           !/*de         67*/
      xa(k)=xa(k-1)-dxp(ieght+2-k)                                              !/*de         68*/
   20 if (xa(k).lt.dp0) xa(k)=dp0                                               !/*de         69*/
      go to 70                                                                  !/*de         70*/
!                                                                               !/*de         71*/
   30 continue                                                                  !/*de         72*/
      if (ibrad.eq.0) then                                                      !/*de         73*/
        pt=1.18d0*fac                                                           !/*de         74*/
      else                                                                      !/*de         75*/
        pt=1.1019d0*f(icsa3)*fac                                                !/*de         76*/
      endif                                                                     !/*de         77*/
      pt3=pt**3                                                                 !/*de         78*/
      qst=pt3*csa(3)/dp2                                                        !/*de         79*/
      qlt=sqrt(.640561d0+qst*qst)                                               !/*de         80*/
      ct=(qlt+qst)**th-(qlt-qst)**th                                            !/*de         81*/
      go to 50                                                                  !/*de         82*/
   40 continue                                                                  !/*de         83*/
      if (ibrad.eq.0) then                                                      !/*de         84*/
        pt=1.07d0*fac                                                           !/*de         85*/
      else                                                                      !/*de         86*/
        pt=f(icsa3)*fac                                                         !/*de         87*/
      endif                                                                     !/*de         88*/
      ct=pt*csa(3)**th                                                          !/*de         89*/
!                                                                               !/*de         90*/
   50 xa(1)=ct-dx(1)                                                            !/*de         91*/
      do 60 k=2,ieght                                                           !/*de         92*/
      xa(k)=xa(k-1)-dx(ieght+2-k)                                               !/*de         93*/
   60 if (xa(k).lt.dp0) xa(k)=dp0                                               !/*de         94*/
!                                                                               !/*de         95*/
!     here we have read in or otherwise determined the                          !/*de         96*/
!     radius and diffuseness for the target (and projectile)                    !/*de         97*/
!                                                                               !/*de         98*/
   70 inine=ieght+1                                                             !/*de         99*/
      xa(inine)=dp0                                                             !/*de        100*/
      csa(9)=xa(1)                                                              !/*de        101*/
      csa(10)=csa(9)*csa(9)                                                     !/*de        102*/
      etr=1.44d0*csa(4)/xa(1)                                                   !/*de        103*/
      if (csa(3).lt.dp2) etr=dp0                                                !/*de        104*/
      csa(11)=etr                                                               !/*de        105*/
      if (iden-1) 130,140,80                                                    !/*de        106*/
   80 c1=(dp1+r0)*exp(-r0)                                                      !/*de        107*/
      c2=r0*cosh(r0)-sinh(r0)                                                   !/*de        108*/
      c3=th*r0*r0*r0                                                            !/*de        109*/
      c3c=c3-c2*exp(-xa(1)/at)*(dp1+xa(1)/at)                                   !/*de        110*/
      ro0=csa(3)/c3c/at3/dp4/3.14159d0                                          !/*de        111*/
!                                                                               !/*de        112*/
      do 120 i=1,ieght                                                          !/*de        113*/
      x1=xa(i+1)/at                                                             !/*de        114*/
      x2=xa(i)/at                                                               !/*de        115*/
      x13=x1**3                                                                 !/*de        116*/
      x23=x2**3                                                                 !/*de        117*/
!                                                                               !/*de        118*/
!         if small nucleus, set r<0 values to last good one                     !/*de        119*/
!                                                                               !/*de        120*/
      if (x1.eq.x2) go to 110                                                   !/*de        121*/
      vol=th*(x23-x13)                                                          !/*de        122*/
!                                                                               !/*de        123*/
      if (x1.ge.r0) go to 100                                                   !/*de        124*/
      if (x2.gt.r0) go to 90                                                    !/*de        125*/
!                                                                               !/*de        126*/
!  both x1 and x2 le r0                                                         !/*de        127*/
!                                                                               !/*de        128*/
      ajn=th*(x23-x13)+c1*(x1*cosh(x1)-sinh(x1)-x2*cosh(x2)+sinh(x2))           !/*de        129*/
      go to 110                                                                 !/*de        130*/
!                                                                               !/*de        131*/
!  x1 le r0 , x2 gt r0                                                          !/*de        132*/
!                                                                               !/*de        133*/
   90 ajn=c3-th*x13+c1*(x1*cosh(x1)-sinh(x1))-c2*exp(-x2)*(dp1+x2)              !/*de        134*/
      go to 110                                                                 !/*de        135*/
!                                                                               !/*de        136*/
!  both x1 and x2 gt r0                                                         !/*de        137*/
!                                                                               !/*de        138*/
  100 ajn=c2*((dp1+x1)*exp(-x1)-(dp1+x2)*exp(-x2))                              !/*de        139*/
  110 den_(i)=ro0*ajn/vol                                                       !/*de        140*/
  120 continue                                                                  !/*de        141*/
      go to 150                                                                 !/*de        142*/
!                                                                               !/*de        143*/
!         myer prescription for w-s density distribution                        !/*de        144*/
!                                                                               !/*de        145*/
  130 den_(8)=0.2387324d0*csa(3)/(0.025d0*(xa(8)**3+xa(1)**3)+0.06661d0*        !/*de        146*/
     & (xa(7)**3+xa(2)**3)+0.12241d0*(xa(6)**3+xa(3)**3)+0.28598d0*(xa(5        !/*de        147*/
     & )**3+xa(4)**3))                                                          !/*de        148*/
      if (csa(1).ne.dp0) pdn(8)=dp0                                             !/*de        149*/
      den_(7)=0.975d0*den_(8)                                                   !/*de        150*/
      den_(6)=.90839d0*den_(8)                                                  !/*de        151*/
      den_(5)=.78598d0*den_(8)                                                  !/*de        152*/
      den_(4)=dph*den_(8)                                                       !/*de        153*/
      den_(3)=.21402d0*den_(8)                                                  !/*de        154*/
      den_(2)=.09161d0*den_(8)                                                  !/*de        155*/
      den_(1)=0.025d0*den_(8)                                                   !/*de        156*/
      go to 150                                                                 !/*de        157*/
!                                                                               !/*de        158*/
!          original (isobar) prescription for density                           !/*de        159*/
!                                                                               !/*de        160*/
  140 den_(8)=0.2387324d0*csa(3)/(0.2d0*(xa(3)**3+xa(4)**3+xa(5)**3+xa(6        !/*de        161*/
     & )**3)+0.075d0*(xa(7)**3+xa(2)**3)+0.025d0*(xa(8)**3+xa(1)**3))           !/*de        162*/
      if (csa(1).ne.dp0) pdn(8)=dp0                                             !/*de        163*/
      den_(7)=0.975d0*den_(8)                                                   !/*de        164*/
      den_(6)=0.9d0*den_(8)                                                     !/*de        165*/
      den_(5)=0.7d0*den_(8)                                                     !/*de        166*/
      den_(4)=dph*den_(8)                                                       !/*de        167*/
      den_(3)=0.3d0*den_(8)                                                     !/*de        168*/
      den_(2)=0.1d0*den_(8)                                                     !/*de        169*/
      den_(1)=0.025d0*den_(8)                                                   !/*de        170*/
  150 continue                                                                  !/*de        171*/
      do 160 i=1,ieght                                                          !/*de        172*/
      depn(1,i)=csa(8)*den_(i)                                                  !/*de        173*/
      depn(2,i)=den_(i)-depn(1,i)                                               !/*de        174*/
  160 continue                                                                  !/*de        175*/
!                                                                               !/*de        176*/
      cnst=610.453779d0                                                         !/*de        177*/
!                                                                               !/*de        178*/
!     standard local-density approximation                                      !/*de        179*/
!                                                                               !/*de        180*/
      do 170 j=1,2                                                              !/*de        181*/
      do 170 i=1,ieght                                                          !/*de        182*/
      pfr(j,i)=cnst*depn(j,i)**th                                               !/*de        183*/
      pfw(j,i)=dp0                                                              !/*de        184*/
      ef(j,i)=sqrt(pfr(j,i)*pfr(j,i)+amass(j)*amass(j))-amass(j)                !/*de        185*/
!                                                                               !/*de        186*/
!        kill fermi momentum for proton target (mrc, 13 nov 82)                 !/*de        187*/
!                                                                               !/*de        188*/
      if (csa(3).eq.dp1) pfr(j,i)=dp0                                           !/*de        189*/
      if (csa(3).eq.dp1) ef(j,i)=dp0                                            !/*de        190*/
!                                                                               !/*de        191*/
  170 continue                                                                  !/*de        192*/
!                                                                               !/*de        193*/
!  vap is the ratio of ap to p potential depths                                 !/*de        194*/
      if (ef(1,ieght).ne.dp0) csa(13)=vap/ef(1,ieght)                           !/*de        195*/
      ef(1,inine)=dp0                                                           !/*de        196*/
      ef(2,inine)=dp0                                                           !/*de        197*/
      csa(12)=dp0                                                               !/*de        198*/
      return                                                                    !/*de        199*/
      end                                                                       !/*de        200*/

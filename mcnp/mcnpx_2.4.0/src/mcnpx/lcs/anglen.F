!     $Id: anglen.F,v 1.4 2002/03/27 18:14:54 gwm Exp $                       !/*an          1*/
      subroutine anglen (idp1,idp2,kecm,coscm)                                  !/*an          2*/
#include <lcs/double.h>                                                         /*!an          3*/
!     ==================================================================        !/*an          4*/
! ANGLEN calls   FLTRNF                                                         !/*an          5*/
! called by GAMMA                                                               !/*an          6*/
!                                                                               !/*an          7*/
!     anglen selects the cos in the c.m.system for the elastic                  !/*an          8*/
!     scattering of two nucleons.the cos of the selected angle                  !/*an          9*/
!     is coscm,idp1 and idp2 are the identities of the two nucleons             !/*an         10*/
!     colliding.1.implies protons,2.implies neutrons.kecm is the kinetic        !/*an         11*/
!     energy in the c.m.of the 2 particles                                      !/*an         12*/
!     all selections are based on p.clements and l.winsberg (ucrl-9043)         !/*an         13*/
!     polynomial fits to nucleon-nucleon scattering data.                       !/*an         14*/
!     written by george d.harp in march,1969                                    !/*an         15*/
!     program should be all right for energies greater than 6.15 bev            !/*an         16*/
!     ==================================================================        !/*an         17*/
!                                                                               !/*an         18*/
#ifdef CHEAP                                                                    /*!an         19*/
      double precision idp1, idp2, kecm                                         !/*an         20*/
#else                                                                           /*!an         21*/
      real idp1, idp2, kecm                                                     !/*an         22*/
#endif                                                                          /*!an         23*/
!                                                                               !/*an         24*/
! first decide whether part.1 and 2 are different                               !/*an         25*/
      if (idp1.ne.idp2) go to 70                                                !/*an         26*/
! start i-i selection.first the find the k.e.of a proton striking               !/*an         27*/
! stationary proton that would give the same kecm.this energy,                  !/*an         28*/
! ep,will be in units of 939 mev                                                !/*an         29*/
!     ep=kecm*(5.6754243d-7*kecm+2.12992545d-3)                                 !/*an         30*/
      ep=kecm*(5.675424d-7*kecm+2.129925d-3)                                    !/*an         31*/
! at low energies (t less than 126.8 mev)have isotropic scattering              !/*an         32*/
      if (ep.gt..135d0) go to 10                                                !/*an         33*/
      eta=rang()                                                                !/*an         34*/
      coscm=dp2*eta-dp1                                                         !/*an         35*/
      return                                                                    !/*an         36*/
   10 continue                                                                  !/*an         37*/
      if (ep.gt.1.065d0) go to 50                                               !/*an         38*/
      if (ep.gt.0.6d0) go to 20                                                 !/*an         39*/
! now in region t=127 mev to t=1000 mev                                         !/*an         40*/
      alpha=1.949d0-0.327d0*ep                                                  !/*an         41*/
      beta=9.1d0*ep*ep*ep*ep*ep                                                 !/*an         42*/
      cii=alpha/(alpha+beta)                                                    !/*an         43*/
      dii=beta/(alpha+beta)                                                     !/*an         44*/
      go to 30                                                                  !/*an         45*/
   20 continue                                                                  !/*an         46*/
      alpha=-20.97d0+ep*(90.52d0+ep*(-117.25d0+48.02d0*ep))                     !/*an         47*/
      beta=150.03d0+ep*(-578.33d0+ep*(734.67d0-299.46d0*ep))                    !/*an         48*/
      cii=alpha/(alpha+beta)                                                    !/*an         49*/
      dii=beta/(alpha+beta)                                                     !/*an         50*/
   30 continue                                                                  !/*an         51*/
! now use von neumann rejection in this region                                  !/*an         52*/
   40 continue                                                                  !/*an         53*/
      eta1=rang()                                                               !/*an         54*/
      eta2=rang()                                                               !/*an         55*/
      coscm=dp2*eta2-dp1                                                        !/*an         56*/
      if ((cii+dii*coscm*coscm*coscm*coscm).ge.eta1) return                     !/*an         57*/
      go to 40                                                                  !/*an         58*/
   50 continue                                                                  !/*an         59*/
      if (ep.gt.6.55d0) go to 60                                                !/*an         60*/
! now in region t=1000mev to t=6150 mev                                         !/*an         61*/
      n=-9.68d0+ep*(16.89d0+ep*(-4.348d0+0.4469d0*ep))                          !/*an         62*/
      eta1=rang()                                                               !/*an         63*/
      eta2=rang()                                                               !/*an         64*/
      coscm=eta1**(dp1/real(n+1))                                               !/*an         65*/
      if (eta2.lt.dph) coscm=-coscm                                             !/*an         66*/
      return                                                                    !/*an         67*/
   60 continue                                                                  !/*an         68*/
! now in region t greater than 6.15 bev                                         !/*an         69*/
      eta1=rang()                                                               !/*an         70*/
      eta2=rang()                                                               !/*an         71*/
      coscm=eta1**(dp1/51.d0)                                                   !/*an         72*/
      if (eta2.lt.dph) coscm=-coscm                                             !/*an         73*/
      return                                                                    !/*an         74*/
! this finishes i-i collisions now start i-j                                    !/*an         75*/
!                                                                               !/*an         76*/
! first find the k.e.of a neutron striking a stationary proton that             !/*an         77*/
! give the same kecm.this energy,en,will be in units of 939 mev                 !/*an         78*/
   70 continue                                                                  !/*an         79*/
!     en=kecm*(5.6754243d-7*kecm+2.1313931d-3)                                  !/*an         80*/
      en=kecm*(5.675424d-7*kecm+2.131393d-3)                                    !/*an         81*/
! for t less than 14 mev scattering is isotropic                                !/*an         82*/
      if (en.gt..0150) go to 80                                                 !/*an         83*/
      eta=rang()                                                                !/*an         84*/
      coscm=dp2*eta-dp1                                                         !/*an         85*/
      return                                                                    !/*an         86*/
   80 continue                                                                  !/*an         87*/
      if (en.gt.2.d-1) go to 150                                                !/*an         88*/
! now in region from t=14mev to t=188mev                                        !/*an         89*/
      oneoen=dp1/en                                                             !/*an         90*/
      tij=2.1d0+oneoen*(-.1624d0+oneoen*(.04011d0-4.d-4*oneoen))                !/*an         91*/
      uij=1.57d0+en*(251.7d0+en*(-5021.d0+en*(41192.d0-1.162d5*en)))            !/*an         92*/
      if (uij.lt.dp0) uij=dp0                                                   !/*an         93*/
      vij=en*(-41.d0+en*(2978.d0+en*(-33953.d0+108909.d0*en)))                  !/*an         94*/
      if (vij.lt.dp0) vij=dp0                                                   !/*an         95*/
      pij=-.9d0+oneoen*(.377d0+oneoen*(.0184d0-1.77d-4*oneoen))                 !/*an         96*/
      qij=en*(182.9d0+en*(-1408.8d0+3066.2d0*en))                               !/*an         97*/
! the max.occur at cos=1 or -1                                                  !/*an         98*/
      pmax=tij+uij+vij                                                          !/*an         99*/
      if (pmax.lt.(pij+qij)) pmax=pij+qij                                       !/*an        100*/
      aij=tij/pmax                                                              !/*an        101*/
      bij=uij/pmax                                                              !/*an        102*/
      cij=vij/pmax                                                              !/*an        103*/
      dij=pij/pmax                                                              !/*an        104*/
      eij=qij/pmax                                                              !/*an        105*/
      if (uij.eq.dp0) go to 110                                                 !/*an        106*/
      if (vij.eq.dp0) go to 130                                                 !/*an        107*/
! use von neuman rejection for all 3 possible distributions                     !/*an        108*/
   90 continue                                                                  !/*an        109*/
      eta1=rang()                                                               !/*an        110*/
      eta2=rang()                                                               !/*an        111*/
      coscm=dp2*eta1-dp1                                                        !/*an        112*/
      x=abs(coscm)                                                              !/*an        113*/
      if (coscm.lt.dp0) go to 100                                               !/*an        114*/
      if ((dij+eij*x*x*x*x).ge.eta2) return                                     !/*an        115*/
      go to 90                                                                  !/*an        116*/
  100 continue                                                                  !/*an        117*/
      if ((aij+x*x*(bij+cij*(x**12.d0))).ge.eta2) return                        !/*an        118*/
      go to 90                                                                  !/*an        119*/
  110 continue                                                                  !/*an        120*/
      eta1=rang()                                                               !/*an        121*/
      eta2=rang()                                                               !/*an        122*/
      coscm=dp2*eta1-dp1                                                        !/*an        123*/
      x=abs(coscm)                                                              !/*an        124*/
      if (coscm.lt.dp0) go to 120                                               !/*an        125*/
      if ((dij+eij*x*x*x*x).ge.eta2) return                                     !/*an        126*/
      go to 110                                                                 !/*an        127*/
  120 continue                                                                  !/*an        128*/
      if ((aij+cij*(x**14.d0)).ge.eta2) return                                  !/*an        129*/
      go to 110                                                                 !/*an        130*/
  130 continue                                                                  !/*an        131*/
      eta1=rang()                                                               !/*an        132*/
      eta2=rang()                                                               !/*an        133*/
      coscm=dp2*eta1-dp1                                                        !/*an        134*/
      x=coscm                                                                   !/*an        135*/
      if (x.lt.dp0) go to 140                                                   !/*an        136*/
      if ((dij+eij*x*x*x*x).ge.eta2) return                                     !/*an        137*/
      go to 130                                                                 !/*an        138*/
  140 continue                                                                  !/*an        139*/
      if ((aij+bij*x*x).ge.eta2) return                                         !/*an        140*/
      go to 130                                                                 !/*an        141*/
  150 continue                                                                  !/*an        142*/
      if (en.gt..620) go to 180                                                 !/*an        143*/
! now in region from t=188mev to t=582mev                                       !/*an        144*/
      wij=-dph+en*(18.26d0+en*(-41.64d0+25.08d0*en))                            !/*an        145*/
      xij=37.42d0+en*(-227.38d0+en*(512.62d0-380.37d0*en))                      !/*an        146*/
      yij=-4.42d0+en*(35.43d0+en*(-78.62d0+59.52d0*en))                         !/*an        147*/
      zij=dp4                                                                   !/*an        148*/
      rij=1.32d0+en*(3.95d0-6.71d0*en)                                          !/*an        149*/
      sij=4.19d0+en*(3.34d0+en*(-44.86d0+62.18d0*en))                           !/*an        150*/
      pmax=wij+xij+yij+zij                                                      !/*an        151*/
      if (pmax.lt.(rij+sij)) pmax=rij+sij                                       !/*an        152*/
      aij=wij/pmax                                                              !/*an        153*/
      bij=xij/pmax                                                              !/*an        154*/
      cij=yij/pmax                                                              !/*an        155*/
      dij=zij/pmax                                                              !/*an        156*/
      eij=rij/pmax                                                              !/*an        157*/
      fij=sij/pmax                                                              !/*an        158*/
  160 continue                                                                  !/*an        159*/
      eta1=rang()                                                               !/*an        160*/
      eta2=rang()                                                               !/*an        161*/
      coscm=dp2*eta1-dp1                                                        !/*an        162*/
      x=abs(coscm)                                                              !/*an        163*/
      if (coscm.lt.dp0) go to 170                                               !/*an        164*/
      if ((eij+fij*x*x).ge.eta2) return                                         !/*an        165*/
      go to 160                                                                 !/*an        166*/
  170 continue                                                                  !/*an        167*/
      dijx=dp0                                                                  !/*an        168*/
      if (x.gt.0.16d0) dijx=dij*x**96                                           !/*an        169*/
      alpha=aij+x*x*x*x*(bij+cij*x**10+dijx)                                    !/*an        170*/
      if (alpha.ge.eta2) return                                                 !/*an        171*/
      go to 160                                                                 !/*an        172*/
  180 continue                                                                  !/*an        173*/
      if (en.gt.1.065d0) go to 190                                              !/*an        174*/
! now t=582mev to t=1000mev                                                     !/*an        175*/
      alpha=-20.84d0+en*(88.88d0+en*(-114.82d0+47.08d0*en))                     !/*an        176*/
      beta=149.74d0+en*(-562.82d0+en*(704.13d0-285.13d0*en))                    !/*an        177*/
      cii=alpha/(alpha+beta)                                                    !/*an        178*/
      dii=beta/(alpha+beta)                                                     !/*an        179*/
! now can go back to routine already written for i-i collisions                 !/*an        180*/
      go to 40                                                                  !/*an        181*/
  190 continue                                                                  !/*an        182*/
      if (en.gt.6.55d0) go to 200                                               !/*an        183*/
! now in region t=1000mev to t=6150mev                                          !/*an        184*/
! the form of the distribution for i-j collisions is the                        !/*an        185*/
! same as that for i-i collisions                                               !/*an        186*/
      ep=en                                                                     !/*an        187*/
      go to 50                                                                  !/*an        188*/
  200 continue                                                                  !/*an        189*/
! now in region t greater than 6.15 bev and the form of distribution            !/*an        190*/
! for i=j collisions same as for i-i                                            !/*an        191*/
      go to 60                                                                  !/*an        192*/
      end                                                                       !/*an        193*/

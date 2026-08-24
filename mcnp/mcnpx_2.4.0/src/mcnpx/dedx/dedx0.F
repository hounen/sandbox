!     $Id: dedx0.F,v 1.4 2002/03/27 18:14:33 gwm Exp $                        !/*de          1*/
      function dedx0(e_proj,proj_m,z_proj,avz,at_den,fme_,a_,iza_,nel1)         !/*de          2*/
!                                                                               !/*de          3*/
!       dedx for heavy ion, protons,pions,muons                                 !/*de          4*/
!                                                                               !/*de          5*/
!       units: MeV/cm                                                           !/*de          6*/
!                                                                               !/*de          7*/
!       low energy models from SPAR.                                            !/*de          8*/
!       equivalent proton energy < 8 MeV.                                       !/*de          9*/
!                                                                               !/*de         10*/
!         e_proj = projectile kinetic energy (MeV)                              !/*de         11*/
!         proj_m = projectile mass (MeV)                                        !/*de         12*/
!         z_proj = projectile charge number                                     !/*de         13*/
!         avz = <Z> for the medium                                              !/*de         14*/
!             = SUM fme_(i)*z(i)                                                !/*de         15*/
!         at_den = atomic density (1/barn-cm)                                   !/*de         16*/
!         fme_  = array of atom fracions i=1,...,nel1                           !/*de         17*/
!         a_(i) = array of mass numbers i=1,...,nel1                            !/*de         18*/
!                  (more properly, atomic weights)                              !/*de         19*/
!         iza_(i) = array of 1000*Z + A numbers i=1,...,nel1                    !/*de         20*/
!         nel1 = number of isotopes in medium                                   !/*de         21*/
!                                                                               !/*de         22*/
#include <lcs/double.h>                                                         /*!de         23*/
      parameter (emass=0.5109990615d0)                                          !/*de         24*/
      parameter (av_num=0.6022136736d0)                                         !/*de         25*/
      parameter (el_rad=0.28179409238d0)                                        !/*de         26*/
      parameter (stp_con=dp4*dppi*el_rad**2*emass)                              !/*de         27*/
      parameter (prot_mass=938.2723128d0)                                       !/*de         28*/
      parameter (prot_m=prot_mass)                                              !/*de         29*/
      parameter (om30=1.0d-30)                                                  !/*de         30*/
      dimension a_(nel1), fme_(nel1)                                            !/*de         31*/
      dimension iza_(nel1)                                                      !/*de         32*/
      se=dp0                                                                    !/*de         33*/
      snuc=dp0                                                                  !/*de         34*/
      if (e_proj.eq.dp0) go to 20                                               !/*de         35*/
      rel_m=proj_m/prot_m                                                       !/*de         36*/
      if (e_proj.gt.8.d0*rel_m) stop ' energy too large in DEDX0 '              !/*de         37*/
      fac1=z_proj**dpth                                                         !/*de         38*/
      bcut1=.07d0*fac1**2                                                       !/*de         39*/
      bcut2=.0046d0*fac1                                                        !/*de         40*/
      gammasq=(e_proj/proj_m+dp1)**2                                            !/*de         41*/
      betasq=(gammasq-dp1)/gammasq                                              !/*de         42*/
      beta=sqrt(betasq)                                                         !/*de         43*/
      beta0=beta                                                                !/*de         44*/
      fac0=dp1                                                                  !/*de         45*/
      if (beta0.lt.bcut2) then                                                  !/*de         46*/
!-----                                                                          !/*de         47*/
! BETA0 < BCUT2                                                                 !/*de         48*/
        beta=bcut2                                                              !/*de         49*/
        betasq=beta**2                                                          !/*de         50*/
        gammasq=dp1/(dp1-betasq)                                                !/*de         51*/
        ei=(sqrt(gammasq)-dp1)*proj_m                                           !/*de         52*/
        fac0=sqrt(e_proj/ei)                                                    !/*de         53*/
      endif                                                                     !/*de         54*/
      a7=stp_con*avz*at_den*f_linhard(betasq,avz)/betasq                        !/*de         55*/
      zsq=z_proj*z_proj                                                         !/*de         56*/
      se=zsq*fac0*a7                                                            !/*de         57*/
      if (beta0.ge.bcut1) go to 20                                              !/*de         58*/
!-----                                                                          !/*de         59*/
! Compute electronic stopping power                                             !/*de         60*/
      temp=(dp1-exp(-125.d0*beta/fac1**2))**2                                   !/*de         61*/
      se=se*temp                                                                !/*de         62*/
!-----                                                                          !/*de         63*/
! Compute nuclear stopping power                                                !/*de         64*/
      do 10 j=1,nel1                                                            !/*de         65*/
      z = iza_(j)/1000                                                          !/*de         66*/
      q1=sqrt(fac1**2+z**dp2th)                                                 !/*de         67*/
      q2=a_(j)+rel_m                                                            !/*de         68*/
      q3=z_proj*z                                                               !/*de         69*/
      fj=3.255d+04*a_(j)/(q2*q3*q1)                                             !/*de         70*/
      gj=1.96d-4*a_(j)*q2*q1/(q3*rel_m)                                         !/*de         71*/
      ej=fj*e_proj                                                              !/*de         72*/
      if (ej.gt.1.d+03) go to 10                                                !/*de         73*/
      if (ej.gt.dp4) then                                                       !/*de         74*/
        dedp=0.5455d0*log(ej)/(ej*(dp1-0.9988d0*ej**(-1.5391d0)))               !/*de         75*/
      else                                                                      !/*de         76*/
        dedp=4.46426d0*sqrt(ej)*exp(-2.542d0*ej**0.277d0)                       !/*de         77*/
      endif                                                                     !/*de         78*/
      sj=(a_(j)*at_den*fme_(j)/av_num)*dedp/gj                                  !/*de         79*/
      snuc=snuc+sj                                                              !/*de         80*/
   10 continue                                                                  !/*de         81*/
!-----                                                                          !/*de         82*/
   20 continue                                                                  !/*de         83*/
      dedx0=max(se+snuc,om30)                                                   !/*de         84*/
      return                                                                    !/*de         85*/
      end                                                                       !/*de         86*/

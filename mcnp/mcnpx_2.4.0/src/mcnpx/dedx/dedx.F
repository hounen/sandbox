!     $Id: dedx.F,v 1.4 2002/03/27 18:14:33 gwm Exp $                         !/*de          1*/
      function dedx (e_proj,proj_m,z_proj,avz,at_den,bari_ln,fme_,a_,           !/*de          2*/
     & iza_,nel1,jj,del_pr)                                                     !/*de          3*/
!       REAL dedx CODING                                                        !/*de          4*/
!       dedx for heavy ion, protons,pions,muons                                 !/*de          5*/
!       units: MeV/cm                                                           !/*de          6*/
!                                                                               !/*de          7*/
!         e_proj = projectile kinetic energy (MeV)                              !/*de          8*/
!         proj_m = projectile mass (MeV)                                        !/*de          9*/
!         z_proj = projectile charge number                                     !/*de         10*/
!         avz = <Z> for the medium                                              !/*de         11*/
!             = SUM fme_(i)*z(i)                                                !/*de         12*/
!         at_den = atomic density (1/barn-cm)                                   !/*de         13*/
!         bari_ln = <ln I> with I in MeV                                        !/*de         14*/
!                 = SUM fme_(i)*z(i)*ln I(i) / <Z>                              !/*de         15*/
!         fme_(i)  = array of atom fracions i=1,...,nel1                        !/*de         16*/
!         a_(i) = array of mass numbers i=1,...,nel1                            !/*de         17*/
!                  (more properly, atomic weights)                              !/*de         18*/
!         iza_(i) = array of 1000*Z + A numbers i=1,...,nel1                    !/*de         19*/
!         nel1 = number of isotopes in medium                                   !/*de         20*/
!         jj:                                                                   !/*de         21*/
!            jj=1 gaseous element                                               !/*de         22*/
!            jj=2 condensed element                                             !/*de         23*/
!            jj=3 gaseous mixture                                               !/*de         24*/
!            jj=4 condensed mixture                                             !/*de         25*/
!                                                                               !/*de         26*/
#include <lcs/double.h>                                                         /*!de         27*/
      parameter (prot_mass=938.2723128d0)                                       !/*de         28*/
      parameter (prot_m=prot_mass)                                              !/*de         29*/
      dimension fme_(nel1), a_(nel1)                                            !/*de         30*/
      dimension iza_(nel1)                                                      !/*de         31*/
      data elow /1.31d0/, ehigh /5.24d0/                                        !/*de         32*/
      e_prot=e_proj*prot_m/proj_m                                               !/*de         33*/
      del_pr=dp0                                                                !/*de         34*/
      if (e_prot.gt.elow) stp1=z_proj**2*dedx1(e_prot,proj_m,avz,at_den         !/*de         35*/
     & ,bari_ln,fme_(1),iza_(1),nel1,jj,del_pr)                                 !/*de         36*/
      if (e_prot.lt.ehigh) stp2=dedx0(e_proj,proj_m,z_proj,avz,at_den           !/*de         37*/
     & ,fme_(1),a_(1),iza_(1),nel1)                                             !/*de         38*/
      fx=(e_prot-elow)/(ehigh-elow)                                             !/*de         39*/
      fx=min(dp1,max(dp0,fx))                                                   !/*de         40*/
      dedx=fx*stp1+(dp1-fx)*stp2                                                !/*de         41*/
      return                                                                    !/*de         42*/
      end                                                                       !/*de         43*/

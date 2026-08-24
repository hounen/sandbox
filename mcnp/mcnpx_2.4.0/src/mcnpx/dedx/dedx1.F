!     $Id: dedx1.F,v 1.5 2002/03/27 18:14:33 gwm Exp $                        !/*de          1*/
      function dedx1 (e_prot,proj_m,avz,at_den,bari_ln,fme_,iza_,nel1,          !/*de          2*/
     & jj,del_pr)                                                               !/*de          3*/
!                                                                               !/*de          4*/
!       dedx for heavy ion, protons,pions,muons                                 !/*de          5*/
!                                                                               !/*de          6*/
!       compute energy loss above about 1-mev (eone).                           !/*de          7*/
!       limit determined by validity of Janni shell correction.                 !/*de          8*/
!                                                                               !/*de          9*/
!         e_prot = equivalent proton kinetic energy (MeV)                       !/*de         10*/
!         proj_m = projectile mass (MeV)                                        !/*de         11*/
!         avz = <Z> for the medium                                              !/*de         12*/
!             = SUM fme_(i)*z(i)                                                !/*de         13*/
!         at_den = atomic density (1/barn-cm)                                   !/*de         14*/
!         bari_ln = <ln I> with I in MeV                                        !/*de         15*/
!                 = SUM fme_(i)*z(i)*ln I(i) /<Z>                               !/*de         16*/
!         fme_  = array of atom fracions i=1,...,nel1                           !/*de         17*/
!         iza_(i) = array of 1000*Z + A numbers i=1,...,nel1                    !/*de         18*/
!         nel1 = number of isotopes in medium                                   !/*de         19*/
!         jj:                                                                   !/*de         20*/
!            jj=1 gaseous element                                               !/*de         21*/
!            jj=2 condensed element                                             !/*de         22*/
!            jj=3 gaseous mixture                                               !/*de         23*/
!            jj=4 condensed mixture                                             !/*de         24*/
!                                                                               !/*de         25*/
!        this function computes the energy loss and if necessary                !/*de         26*/
!        weights it over all elements to get the energy loss for a              !/*de         27*/
!        compound or mixture.                                                   !/*de         28*/
!                                                                               !/*de         29*/
#include <lcs/double.h>                                                         /*!de         30*/
      dimension fme_(nel1)                                                      !/*de         31*/
      dimension iza_(nel1)                                                      !/*de         32*/
!                                                                               !/*de         33*/
!        con = 2 pi M(el)c**2 R(el)**2 N(Avagadro)                              !/*de         34*/
!                                                                               !/*de         35*/
      parameter (emass=0.5109990615d0)                                          !/*de         36*/
      parameter (prot_mass=938.2723128d0)                                       !/*de         37*/
      parameter (av_num=0.6022136736d0)                                         !/*de         38*/
      parameter (el_rad=0.28179409238d0)                                        !/*de         39*/
      parameter (alpfsc=7.2973530791728595d-03)                                 !/*de         40*/
      parameter (con=2.0*dppi*emass*el_rad**2*av_num)                           !/*de         41*/
!                                                                               !/*de         42*/
      parameter (eone=1.30d0)                                                   !/*de         43*/
!                                                                               !/*de         44*/
      logical ptraut                                                            !/*de         45*/
!                                                                               !/*de         46*/
      data ikshl /1/, ilshl /1/, imshl /1/, inshl /1/, ioshl /1/, ipshl         !/*de         47*/
     & /1/                                                                      !/*de         48*/
!                                                                               !/*de         49*/
      if (e_prot.lt.eone) stop ' e_prot.lt.eone in DEDX1 '                      !/*de         50*/
!                                                                               !/*de         51*/
      rstmas=prot_mass                                                          !/*de         52*/
      s=e_prot/rstmas                                                           !/*de         53*/
      gamma=dp1+s                                                               !/*de         54*/
      betasq=s*(s+dp2)/gamma**2                                                 !/*de         55*/
      dedx1=0.                                                                  !/*de         56*/
!                                                                               !/*de         57*/
!         Get Bethe-Bloch term                                                  !/*de         58*/
!                                                                               !/*de         59*/
      t_0=dp2*emass*betasq/(dp1-betasq)                                         !/*de         60*/
      t_max=t_0/(dp1+dp2*gamma*(emass/proj_m)+(emass/proj_m)**2)                !/*de         61*/
      b_b=log(t_0*t_max)-dp2*(bari_ln+betasq)                                   !/*de         62*/
!                                                                               !/*de         63*/
!         Get density correction                                                !/*de         64*/
!                                                                               !/*de         65*/
      del=del1(betasq,avz,at_den,bari_ln,jj)                                    !/*de         66*/
!                                                                               !/*de         67*/
!         Get shell correction from Janni code                                  !/*de         68*/
!                                                                               !/*de         69*/
      c_ovr_z=dp0                                                               !/*de         70*/
      zsum=dp0                                                                  !/*de         71*/
      ptraut=.false.                                                            !/*de         72*/
      do 10 l=1,nel1                                                            !/*de         73*/
      z = iza_(l)/1000                                                          !/*de         74*/
      t=z*fme_(l)                                                               !/*de         75*/
      zsum=zsum+t                                                               !/*de         76*/
      cklmno=shellj(z,e_prot,ck1,cl1,cl2,cl,cm1,cm2,cm3,cm,cn1,cn2,cn3          !/*de         77*/
     & ,cn4,cn,co1,co2,co3,co,cp1,cp2,cp3,cp,ikshl,ilshl,imshl,inshl            !/*de         78*/
     & ,ioshl,ipshl,ptraut)                                                     !/*de         79*/
      c_ovr_z=c_ovr_z+t*cklmno/z                                                !/*de         80*/
   10 continue                                                                  !/*de         81*/
      c_ovr_z=c_ovr_z/zsum                                                      !/*de         82*/
!                                                                               !/*de         83*/
      dedx1=b_b-dp2*c_ovr_z-del                                                 !/*de         84*/
      dedx1=dedx1+dppi*sqrt(betasq)*alpfsc                                      !/*de         85*/
      dedx1=(con*at_den/av_num)*avz*dedx1/betasq                                !/*de         86*/
      del_pr=(con*at_den/av_num)*avz*del/betasq                                 !/*de         87*/
!                                                                               !/*de         88*/
      return                                                                    !/*de         89*/
      end                                                                       !/*de         90*/

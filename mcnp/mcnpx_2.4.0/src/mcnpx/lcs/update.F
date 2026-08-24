!     $Id: update.F,v 1.17 2002/03/27 18:15:26 gwm Exp $                      !/*up          1*/
      subroutine update(nr)                                                     !/*up          2*/
!     ==================================================================        !/*up          3*/
!     UPDATE banks NABOV-1 particles and puts the NABOV particle                !/*up          4*/
!     into the parameters of the particle to be tracked.                        !/*up          5*/
!     If NABOV=0, the particle being tracked is marked "dead".                  !/*up          6*/
!     UPDATE is called only for lter = id_elastic_collisn_nonterminal           !/*up          7*/
!          (real elastic collision, nonterminal).                               !/*up          8*/
!     ==================================================================        !/*up          9*/
!     << SPABI: E.C. Snow  05/21/99  revised 04/16/2001>>                       !/*up         10*/
      use global_data                                                           !/*upxc        1*/
!                                                                               !/*upxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*upxc        3*/
      implicit integer(kindi) (i-n)                                             !/*upxc        4*/
!                                                                               !/*upxc        5*/
#include <lcs/bankb.h>                                                          /*!up         12*/
#include <globals/interact_outcomes.h>                                          /*!up         13*/
!                                                                               !/*up         14*/
!     MCNP coding taken from COLIDN                                             !/*up         15*/
      if (nabov.le.0) return                                                    !/*up         16*/
#if defined(SPABI) || defined(MESHTAL)                                          /*!up         17*/
      ip=ipt                                                                    !/*up         18*/
#endif                                                                          /*!up         19*/
!                                                                               !/*up         20*/
#ifdef MESHTAL                                                                  /*!up         21*/
      ja=jan                                                                    !/*up         22*/
      eg=zero                                                                   !/*up         23*/
      j=0                                                                       !/*up         24*/
      do 5 k=1,nabov                                                            !/*up         25*/
      ityp = mod(nint(tipa(k)),100) + 1                                         !/*upxc        6*/
      call mtran(ityp,ipt,jan)                                                  !/*up         27*/
      if (ipt.eq.ip .and. jan.eq.ja .and. ea(k).gt.eg) then                     !/*up         28*/
         j=k                                                                    !/*up         29*/
         eg=ea(k)                                                               !/*up         30*/
      endif                                                                     !/*up         31*/
    5 continue                                                                  !/*up         32*/
#endif                                                                          /*!up         33*/
!                                                                               !/*up         34*/
      npa=1                                                                     !/*up         35*/
!        loop over NABOV particles                                              !/*up         36*/
      do 10 k=1,nabov                                                           !/*up         37*/
      ityp = mod(nint(tipa(k)),100) + 1                                         !/*upxc        7*/
      call mtran(ityp,ipt,jan)                                                  !/*up         39*/
!                                                                               !/*up         40*/
!        Do not create particles in zero-importance cells.                      !/*up         41*/
      if (fiml(ipt).eq.0.) then                                                 !/*up         42*/
         if(k.lt.nabov)go to 10                                                 !/*up         43*/
         call expirx(1,'UPDATE','Projectile type misplaced.')                   !/*up         44*/
         return                                                                 !/*up         45*/
      endif                                                                     !/*up         46*/
!                                                                               !/*up         47*/
      erg=ea(k)                                                                 !/*up         48*/
      wgt=wta(k)                                                                !/*up         49*/
!        convert from nsec to shakes                                            !/*up         50*/
      tme=0.1d0*tca(k)                                                          !/*up         51*/
      if (gpt(ipt).ne.zero) then                                                !/*up         52*/
         vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                   !/*up         53*/
      else                                                                      !/*up         54*/
         vel=slite                                                              !/*up         55*/
      endif                                                                     !/*up         56*/
      uuu=ua(k)                                                                 !/*up         57*/
      vvv=va(k)                                                                 !/*up         58*/
      www=wa(k)                                                                 !/*up         59*/
      if(lev.ne.0)call uplev(uuu)                                               !/*up         60*/
!                                                                               !/*up         61*/
#ifdef MESHTAL                                                                  /*!up         62*/
!     Determine if this is a new particle.                                      !/*up         63*/
      if (k.eq.j) then                                                          !/*up         64*/
         mspf=0                                                                 !/*up         65*/
      else                                                                      !/*up         66*/
         mspf=2                                                                 !/*up         67*/
      endif                                                                     !/*up         68*/
#endif                                                                          /*!up         69*/
!                                                                               !/*up         70*/
!        branch on last particle                                                !/*up         71*/
      if(k.eq.nabov) return                                                     !/*up         72*/
      if (nr.ne.id_elastic_collisn_nonterminal.or.ipt.le.3.or.                  !/*upa         1*/
     & charge(ipt).eq.zero) then                                                !/*upa         2*/
         write(iuo,'('' ***** nr ='',i5)')nr                                    !/*up         78*/
         write(iuo,'('' **** ipt ='',i5)')ipt                                   !/*up         79*/
         call expirx(1,'UPDATE',                                                !/*up         80*/
     &   'inconsistent call from elastic collision.')                           !/*up         81*/
         return                                                                 !/*up         82*/
      endif                                                                     !/*up         83*/
      call gain_pax (igain_gen_chg_elastic_recoil, ipt, 1, wgt, wgt*erg)        !/*upa         3*/
      call contrib_pwb (ipt,ipwb_gen_chg_photonuclear,icl,wgt)                  !/*upa         4*/
#ifdef SPABI                                                                    /*!up         86*/
      if(is_spabi.gt.0) then                                                    !/*upa         5*/
         call sec_par_bias(ipt,ip,erg,igain_gen_chg_elastic_recoil,             !/*upa         6*/
     &   ipwb_gen_chg_photonuclear,0,34)                                        !/*upa         7*/
         go to 10                                                               !/*upa         8*/
      endif                                                                     !/*upa         9*/
#endif                                                                          /*!upa        10*/
      call bankit(34)                                                           !/*upa        11*/
   10 continue                                                                  !/*up        126*/
      return                                                                    !/*up        127*/
      end                                                                       !/*up        128*/

!     $Id: updat1.F,v 1.23 2002/03/27 18:15:25 gwm Exp $                      !/*up          1*/
      subroutine updat1(nr)                                                     !/*up          2*/
!     ==================================================================        !/*up          3*/
!     UPDAT1 banks NABOV particles and restores the parameters                  !/*up          4*/
!     of the particle being tracked.                                            !/*up          5*/
!     ==================================================================        !/*up          6*/
!     MCNP coding follows ACEGAM                                                !/*up          7*/
!     <<  SPABI:  E. C. Snow  05/21/99  revised 04/16/2001>>                    !/*up          8*/
      use global_data                                                           !/*upxc        1*/
!                                                                               !/*upxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*upxc        3*/
      implicit integer(kindi) (i-n)                                             !/*upxc        4*/
!                                                                               !/*upxc        5*/
#include <lcs/bankb.h>                                                          /*!up         10*/
#include <globals/interact_outcomes.h>                                          /*!up         11*/
!                                                                               !/*up         12*/
      if (nabov.eq.0) return                                                    !/*up         13*/
#ifdef SPABI                                                                    /*!up         20*/
      ip=ipt                                                                    !/*up         21*/
#endif                                                                          /*!up         22*/
      call savpar(0,0)                                                          !/*upa         1*/
      npa=1                                                                     !/*up         27*/
!                                                                               !/*up         28*/
#ifdef MESHTAL                                                                  /*!up         29*/
      mspf=2                                                                    !/*up         30*/
#endif                                                                          /*!up         31*/
!                                                                               !/*up         32*/
      do 30 ll=1,nabov                                                          !/*up         33*/
      ityp = mod(nint(tipa(ll)),100) + 1                                        !/*upxc        6*/
      call mtran(ityp,ipt,jan)                                                  !/*up         35*/
!                                                                               !/*up         36*/
!        Do not create particles in zero-importance cells.                      !/*up         37*/
      if(fiml(ipt).eq.0.)go to 30                                               !/*up         38*/
!                                                                               !/*up         39*/
      erg=ea(ll)                                                                !/*up         40*/
      wgt=wta(ll)                                                               !/*up         41*/
!        convert from nsec to shakes                                            !/*up         42*/
      tme=tenth*tca(ll)                                                         !/*up         43*/
      if (gpt(ipt).ne.zero) then                                                !/*up         44*/
         vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                   !/*up         45*/
      else                                                                      !/*up         46*/
         vel=slite                                                              !/*up         47*/
      endif                                                                     !/*up         48*/
      uuu=ua(ll)                                                                !/*up         49*/
      vvv=va(ll)                                                                !/*up         50*/
      www=wa(ll)                                                                !/*up         51*/
      if(lev.ne.0)call uplev(uuu)                                               !/*up         52*/
!IDX                                                                            !/*up         53*/
      if (ipt.eq.2.or.ipt.eq.1.and.jan.eq.1.and.erg.le.cut_n) then              !/*up         54*/
         idx=0                                                                  !/*up         55*/
         do 25 j=1,ndx(ipt)                                                     !/*up         56*/
            if((xxx-dxx(ipt,1,j))**2+(yyy-dxx(ipt,2,j))**2+                     !/*up         57*/
     &       (zzz-dxx(ipt,3,j))**2.lt.dxx(ipt,5,j))idx=j                        !/*up         58*/
   25    continue                                                               !/*up         59*/
      endif                                                                     !/*up         60*/
!IDX                                                                            !/*up         61*/
      if (nr.eq.id_nonelastic_collisn_terminal) then                            !/*up         62*/
         nx = igain_all_pls_nucl_interaction                                    !/*up         63*/
         if (ipt.eq.1) then                                                     !/*up         64*/
            nb = ipwb_neutron_nuc_interaction                                   !/*up         65*/
         else if (ipt.eq.2) then                                                !/*up         66*/
            nb = ipwb_photon_nuc_interaction                                    !/*up         67*/
         else if (ipt.eq.3) then                                                !/*up         68*/
            nb = ipwb_electron_nuc_interaction                                  !/*up         69*/
         else if (charge(ipt).eq.zero) then                                     !/*up         70*/
            nb = ipwb_gen_neu_nuc_interaction                                   !/*up         71*/
         else                                                                   !/*up         72*/
            nb = ipwb_gen_chg_nuc_interaction                                   !/*up         73*/
         endif                                                                  !/*up         74*/
      else if (nr.eq.id_elastic_collisn_nonterminal) then                       !/*up         75*/
         if (ipt.gt.3.and.charge(ipt).ne.zero) then                             !/*up         76*/
            nx = igain_gen_chg_elastic_recoil                                   !/*up         77*/
            nb = ipwb_gen_chg_photonuclear                                      !/*up         78*/
         else                                                                   !/*up         79*/
            write(iuo,'('' ***** nr ='',i5)')nr                                 !/*up         80*/
            write(iuo,'('' **** ipt ='',i5)')ipt                                !/*up         81*/
            call expirx(1,'UPDAT1',                                             !/*up         82*/
     &      'illegal particle from elastic collision.')                         !/*up         83*/
            return                                                              !/*up         84*/
         endif                                                                  !/*up         85*/
      else if (nr.eq.id_fulldecay_pseudo_terminal .or.                          !/*up         86*/
     &         nr.eq.id_partdecay_pseudo_nonterminal) then                      !/*up         87*/
         nx = igain_all_pls_particle_decay                                      !/*up         88*/
         if (ipt.eq.2) then                                                     !/*up         89*/
            nb = ipwb_photon_particle_decay                                     !/*up         90*/
         else if (ipt.eq.3) then                                                !/*up         91*/
            nb = ipwb_electron_particle_decay                                   !/*up         92*/
         else if (ipt.eq.1) then                                                !/*up         93*/
!           QUESTION:  Revisit this specification.  In particular, is           !/*up         94*/
!                      this really decay (not yet done for neutrons),           !/*up         95*/
!                      or does it model antineutron annihilation???             !/*up         96*/
            nb = ipwb_neutron_particle_decay                                    !/*up         97*/
         else if (charge(ipt).eq.zero) then                                     !/*up         98*/
            nb = ipwb_gen_neu_particle_decay                                    !/*up         99*/
         else                                                                   !/*up        100*/
            nb = ipwb_gen_chg_particle_decay                                    !/*up        101*/
         endif                                                                  !/*up        102*/
      else                                                                      !/*up        103*/
         write(iuo,'('' ***** nr ='',i5)')nr                                    !/*up        104*/
         call expirx(1,'UPDAT1','call from interact with illegal lter.')        !/*up        105*/
         return                                                                 !/*up        106*/
      endif                                                                     !/*up        107*/
      call gain_pax (nx, ipt, 1, wgt, wgt*erg)                                  !/*up        108*/
      call contrib_pwb (ipt,nb,icl,wgt)                                         !/*up        109*/
#ifdef SPABI                                                                    /*!up        110*/
      if(is_spabi.gt.0) then                                                    !/*upa         2*/
         call sec_par_bias(ipt,ip,erg,nx,nb,0,33)                               !/*upa         3*/
         go to 30                                                               !/*upa         4*/
      endif                                                                     !/*upa         5*/
#endif                                                                          /*!upa         6*/
      call bankit(33)                                                           !/*upa         7*/
   30 continue                                                                  !/*up        150*/
      if(kdb.ne.0)return                                                        !/*up        151*/
      call getpar(0,0)                                                          !/*upa         8*/
      return                                                                    !/*up        157*/
      end                                                                       !/*up        158*/

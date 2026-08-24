!     $Id: blkd.F,v 1.5 2002/03/27 18:13:56 gwm Exp $                         !/*bl          1*/
      block data blkd                                                           !/*bl          2*/
#include <htape3x/com32b.h>                                                     /*!bl          3*/
#include <htape3x/phprm2.h>                                                     /*!bl          4*/
#include <htape3x/lpt.h>                                                        /*!bl          5*/
#include <htape3x/kpt.h>                                                        /*!bl          6*/
#include <htape3x/exm.h>                                                        /*!bl          7*/
      data pmass /wprot,wneut,wpion,wpi0,wpion,2*wmuon,wdeut,wtrit,             !/*bl          8*/
     & w_he3,walph,dp0,wkaon,2*wka0,wkaon,wprot,wneut,2*elmass,2*dp0/           !/*bl          9*/
      data exmass /xsm10,xsm11,xsm21,xsm31,xsm32,xsm42/                         !/*bl         10*/
      data kpart /'proton','neutron','pi+','pi0','pi-','mu+','mu-',             !/*bl         11*/
     & 'deutron','triton','he-3','alpha','photon','K+','K0-long',               !/*bl         12*/
     & 'K0-shrt','K-','p-bar','n-bar','e-','e+','nu','nu-bar','p_casc',         !/*bl         13*/
     & 'n_casc','pi+_casc','pi0_casc','pi-_casc','pi+_decay',                   !/*bl         14*/
     & 'pi0_decay','pi-_decay','ph_decay','ph_de-ex','p_preq','n_preq',         !/*bl         15*/
     & 'd_preq','t_preq','He3_preq','a_preq','p_evap','n_evap',                 !/*bl         16*/
     & 'd_evap','t_evap','He3_evap','a_evap'/                                   !/*bl         17*/
      end                                                                       !/*bl         18*/
